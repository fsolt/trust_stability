# ============================================================================
# REGIME DURABILITY / SURVIVAL: does discontent (x support) shorten the life of
# a regime?  Cox proportional-hazards, the standard tool in the regime-survival
# literature (Svolik; Bernhard et al.), replacing the discrete-time logit.
#
# Risk set : regime spell = maximal run in the same broad regime state
#            (democracy: v2x_regime >= 2 ; autocracy: v2x_regime < 2).
# Clock    : regime age (years since the spell began).
# Failure  : the spell ends -> democracy breaks down / autocracy democratizes;
#            spells ongoing at end of data are right-censored.
# Model    : coxph(Surv(age-1, age, fail) ~ disc_c*supp_c + controls) in
#            counting-process form with time-varying (lagged) covariates,
#            robust SEs clustered by country. HR>1 = discontent SHORTENS
#            survival (destabilizing). Triple measurement uncertainty
#            (discontent x support x V-Dem) via method of composition, 900 draws.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, MASS, glue, ERT, parallel, survival)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. Regime spells, age, failure (binary dem/aut state) ----------------------
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(is_dem = as.integer(v2x_regime >= 2),
           chg    = as.integer(is_dem != lag(is_dem)),
           spell  = cumsum(replace_na(chg, 0L)),
           # failure this year = the regime state changes next year (spell ends);
           # last observed year of a country -> lead is NA -> censored (0)
           fail   = replace_na(as.integer(lead(is_dem) != is_dem), 0L)) %>%
    group_by(iso3c, spell) %>%
    mutate(age = row_number(), spell_state = first(is_dem)) %>%
    ungroup() %>%
    select(iso3c, year, is_dem, spell, spell_state, age, fail)

# 2. Inputs (paired draws) ---------------------------------------------------
load(here("data", "theta_results.rda")); theta_results <- rename(theta_results, discontent = theta)
disc_list <- split(theta_results, theta_results$draw); set.seed(123)
disc_list <- disc_list[sample(length(disc_list), N_DRAWS)]; rm(theta_results)
supp_list <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds"))
supp_list <- lapply(supp_list, function(d) select(as_tibble(d), country, year, support = theta))[seq_len(N_DRAWS)]
ctrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))[seq_len(N_DRAWS)]
cc_xwalk  <- tibble(country = sort(unique(ctrl_list[[1]]$country))) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))

build_one <- function(disc, supp, ctrl) {
    disc %>% select(country, year, discontent) %>%
        inner_join(supp, by = c("country","year")) %>%
        left_join(ctrl, by = c("country","year")) %>%
        left_join(cc_xwalk, by = "country") %>%
        left_join(ert_cy, by = c("iso3c","year")) %>%
        arrange(country, year) %>% group_by(country) %>%
        mutate(disc_l = lag(discontent,1), supp_l = lag(support,1),
               lnGDP_l = lag(lnGDP_imp,1), grth_l = lag(GDP_imp_grth,1),
               regUN_l = lag(Libdem_regUN,1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di,1)) %>%
        ungroup() %>%
        mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
               supp_c = supp_l - mean(supp_l, na.rm=TRUE),
               disc_supp = disc_c * supp_c,
               t0 = age - 1, t1 = age) %>%
        filter(!is.na(age), t1 > t0)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
sd_supp <- sd(data[[1]]$supp_c, na.rm = TRUE); rm(disc_list, supp_list, ctrl_list)

# 3. Method of composition for Cox PH ----------------------------------------
ncores <- max(1L, detectCores() - 2L)
rhs <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l"
mocCox <- function(data, state = NA) {           # state: 1 dem spells, 0 aut spells, NA all
    f <- as.formula(glue("Surv(t0, t1, fail) ~ {rhs} + cluster(country)"))
    vars <- c("t0","t1","fail","country", all.vars(as.formula(glue("~ {rhs}"))))
    prep <- function(d) {
        if (!is.na(state)) d <- d[d$spell_state == state & !is.na(d$spell_state), ]
        d[stats::complete.cases(d[, vars]), , drop = FALSE]
    }
    d1 <- prep(data[[1]]); cn <- names(coef(survival::coxph(f, data = d1)))
    res <- mclapply(seq_along(data), function(s) {
        d <- prep(data[[s]])
        tryCatch({
            m <- survival::coxph(f, data = d)
            b <- tryCatch(MASS::mvrnorm(1, coef(m), vcov(m)), error = function(e) coef(m))
            b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    list(coef = out[stats::complete.cases(out), , drop = FALSE],
         n_fail = sum(d1$fail), n_obs = nrow(d1))
}

message(glue("Fitting regime-survival Cox models on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(
    all_regimes = mocCox(data, NA),
    democracies = mocCox(data, 1),
    autocracies = mocCox(data, 0)
)
saveRDS(res, here("results_regime_survival.rds"))

# 4. Report (hazard ratios; HR>1 = shorter survival) -------------------------
report <- function(r, label) {
    m <- r$coef
    cat(sprintf("\n==== %s ==== failures=%d obs=%d draws=%d ====\n", label, r$n_fail, r$n_obs, nrow(m)))
    bd <- m[["disc_c"]]; bi <- m[["disc_supp"]]
    tab <- tibble(
        quantity = c("discontent (main)", "disc x support", "disc @ LOW support", "disc @ HIGH support"),
        logHR = c(mean(bd), mean(bi), mean(bd - bi*sd_supp), mean(bd + bi*sd_supp)),
        SE    = c(sd(bd),   sd(bi),   sd(bd - bi*sd_supp),   sd(bd + bi*sd_supp))
    ) %>% mutate(HR = round(exp(logHR),3), z = round(logHR/SE,2),
                 p = round(2*(1-pnorm(abs(z))),3), sig = ifelse(p<.05,"*",ifelse(p<.1,".","")),
                 across(c(logHR,SE), ~round(.x,3)))
    print(tab)
}
report(res$all_regimes, "ALL REGIMES (any regime change = failure)")
report(res$democracies, "DEMOCRACIES (breakdown = failure)")
report(res$autocracies, "AUTOCRACIES (democratization = failure)")
