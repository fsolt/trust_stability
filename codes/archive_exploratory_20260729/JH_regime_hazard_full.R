# ============================================================================
# FULLY-SPECIFIED discrete-time regime-hazard model (grouped-duration survival)
# Reports ALL coefficients so control effects are visible. Adds the standard
# regime-survival controls that were missing:
#   - regime tenure     : log(regime age) = how long the regime has already lasted
#   - prior transitions : # of past regime changes (regime instability history)
#   - income inequality : SWIID gini_disp
#   - population        : log(wb_population)
# on top of log GDPpc, GDP growth, regional democracy, %Muslim, resource dep.
# DV = broad RoW regime-category change (more events -> supports more controls):
#   down among democracies / up among autocracies / any (pooled).
# Firth penalized logit (rare events) + method of composition (900 draws,
# discontent x support x V-Dem uncertainty). HR/OR>1 = higher failure hazard.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, MASS, glue, ERT, parallel, logistf)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. ERT: RoW spells, tenure, events, prior-transition count ------------------
# Correct discrete-time hazard coding: a regime spell is a maximal run of
# constant v2x_regime; FAILURE is coded at the LAST year of the run (the regime
# changes NEXT year -> lead()), so the event is decoupled from regime age.
# (Coding failure at the first year of the new run would make event == (age==1),
#  a mechanical separation that blows up the tenure coefficient.)
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(new_spell = replace_na(as.integer(v2x_regime != lag(v2x_regime)), 0L),
           spell       = cumsum(new_spell),
           prior_trans = cumsum(new_spell),                 # # regime changes so far
           fail_any  = replace_na(as.integer(lead(v2x_regime) != v2x_regime), 0L),
           fail_down = replace_na(as.integer(lead(v2x_regime) <  v2x_regime), 0L),
           fail_up   = replace_na(as.integer(lead(v2x_regime) >  v2x_regime), 0L)) %>%
    group_by(iso3c, spell) %>%
    mutate(regime_age = row_number()) %>%                   # tenure of current regime
    ungroup() %>%
    select(iso3c, year, v2x_regime, fail_down, fail_up, fail_any, prior_trans, regime_age)

# 2. Extra controls: inequality + population ---------------------------------
extra_ctrl <- readRDS(file.path(BS_DIR, "country_data.rds")) %>%
    mutate(iso3c = countrycode(vdem_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    transmute(iso3c, year, gini = gini_disp, lnpop = log(wb_population)) %>%
    distinct(iso3c, year, .keep_all = TRUE)

# 3. Inputs (paired draws) ---------------------------------------------------
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
        left_join(extra_ctrl, by = c("iso3c","year")) %>%
        arrange(country, year) %>% group_by(country) %>%
        mutate(disc_l = lag(discontent,1), supp_l = lag(support,1),
               lnGDP_l = lag(lnGDP_imp,1), grth_l = lag(GDP_imp_grth,1),
               regUN_l = lag(Libdem_regUN,1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di,1),
               gini_l = lag(gini,1), lnpop_l = lag(lnpop,1),
               tenure = log(regime_age)) %>%             # regime tenure (log years)
        ungroup() %>%
        mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
               supp_c = supp_l - mean(supp_l, na.rm=TRUE),
               disc_supp = disc_c * supp_c,
               dem = v2x_regime >= 2, aut = v2x_regime < 2)  # at-risk state = current regime
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
sd_supp <- sd(data[[1]]$supp_c, na.rm = TRUE); rm(disc_list, supp_list, ctrl_list)

# 4. Method of composition (Firth or glm), FULL coefficient set --------------
ncores <- max(1L, detectCores() - 2L)
rhs <- paste("disc_c + supp_c + disc_supp +",
             "lnGDP_l + grth_l + regUN_l + musl + res_l +",   # original controls
             "tenure + prior_trans + gini_l + lnpop_l")       # added standard controls

moc <- function(dv, subset_col, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); vars <- all.vars(f)
    prep <- function(d) {
        if (!is.na(subset_col)) d <- d[!is.na(d[[subset_col]]) & d[[subset_col]], ]
        d[stats::complete.cases(d[, vars]), , drop = FALSE]
    }
    d1 <- prep(data[[1]]); cn <- names(coef(glm(f, data = d1, family = binomial())))
    res <- mclapply(seq_along(data), function(s) {
        d <- prep(data[[s]])
        tryCatch({
            if (method == "firth") { m <- logistf::logistf(f, data = d, pl = FALSE); b0 <- coef(m); V <- vcov(m) }
            else { m <- glm(f, data = d, family = binomial()); b0 <- coef(m); V <- sandwich::vcovCL(m, cluster = d$country) }
            b <- tryCatch(MASS::mvrnorm(1, b0, V), error = function(e) b0); b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    list(coef = out[stats::complete.cases(out), , drop = FALSE], n_ev = sum(d1[[vars[1]]]), n_obs = nrow(d1))
}

message(glue("Fitting fully-specified hazard models on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(
    `ANY change [all] (glm)`      = moc("fail_any",  NA,    "glm"),
    `DETERIORATION [dem] (Firth)` = moc("fail_down", "dem", "firth"),
    `IMPROVEMENT [aut] (Firth)`   = moc("fail_up",   "aut", "firth")
)
saveRDS(res, here("results_regime_hazard_full.rds"))

# 5. FULL coefficient report --------------------------------------------------
nice <- c(disc_c="Political discontent", supp_c="Democratic support",
          disc_supp="Discontent x Support", lnGDP_l="Log GDP pc", grth_l="GDP growth",
          regUN_l="Regional democracy", musl="% Muslim", res_l="Resource dependence",
          tenure="Regime tenure (log yrs)", prior_trans="Prior transitions",
          gini_l="Income inequality", lnpop_l="Log population", `(Intercept)`="(Intercept)")
for (nm in names(res)) {
    r <- res[[nm]]; m <- r$coef
    cat(sprintf("\n================ %s ================  events=%d obs=%d draws=%d\n",
                nm, r$n_ev, r$n_obs, nrow(m)))
    s <- suppressWarnings(MOCsumm(m)) %>%
        mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error),
               OR = round(exp(estimate), 3), z = round(estimate/std.error, 2),
               p = round(2*(1-pnorm(abs(z))), 3), sig = ifelse(p<.05,"*",ifelse(p<.1,".","")),
               term = ifelse(term %in% names(nice), unname(nice[term]), term),
               across(c(estimate, std.error), ~round(.x, 3)))
    print(s %>% select(term, estimate, std.error, OR, z, p, sig), n = 30)
}
