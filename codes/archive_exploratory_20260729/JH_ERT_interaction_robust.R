# ============================================================================
# Power-improved tests of discontent x support -> regime (in)stability
# Two improvements over JH_ERT_interaction.R, both about statistical power on
# the CORRECT (transition/survival) DV -- not DV-shopping:
#   (1) FIRTH penalized logit (logistf): standard rare-events bias correction.
#   (2) BROADER event DV: any change in the 4-category RoW regime classification
#       (v2x_regime), not only the dem<->auto reg_trans crossing. ~2x more events
#       (any 61->112; dem deterioration 27->39; aut improvement 27->42).
# Every spec is fit with BOTH standard clustered logit AND Firth. Triple
# measurement uncertainty (discontent x support x V-Dem) via method of
# composition over 900 paired draws. Reported regardless of significance.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, MASS, glue, ERT, parallel, logistf)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. ERT outcome: reg_trans events + RoW-change events + RoW-spell age --------
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime, reg_trans) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c)) %>% arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(row_change = as.integer(v2x_regime != lag(v2x_regime)),
           row_down   = as.integer(v2x_regime <  lag(v2x_regime)),
           row_up     = as.integer(v2x_regime >  lag(v2x_regime)),
           spell      = cumsum(replace_na(lag(replace_na(row_change, 0L), 1L), 0L))) %>%
    group_by(iso3c, spell) %>% mutate(regime_age = row_number()) %>% ungroup() %>%
    select(iso3c, year, v2x_regime, reg_trans, row_change, row_down, row_up, regime_age)

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
               regUN_l = lag(Libdem_regUN,1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di,1),
               v2x_l = lag(v2x_regime,1),
               ev_rt_break = as.integer(reg_trans == -1), ev_rt_trans = as.integer(reg_trans == 1),
               ev_row_down = row_down, ev_row_up = row_up, ev_row_any = row_change,
               t1 = regime_age, t2 = regime_age^2, t3 = regime_age^3) %>% ungroup() %>%
        mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
               supp_c = supp_l - mean(supp_l, na.rm=TRUE),
               disc_supp = disc_c * supp_c)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
sd_supp <- sd(data[[1]]$supp_c, na.rm = TRUE); rm(disc_list, supp_list, ctrl_list)

# 3. Method of composition, method = "glm" (clustered) or "firth" ------------
ncores <- max(1L, detectCores() - 2L)
rhs <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l + t1 + t2 + t3"

moc <- function(dv, subset_col, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); vars <- all.vars(f)
    prep <- function(d) {
        if (!is.na(subset_col)) d <- d[!is.na(d[[subset_col]]) & d[[subset_col]], ]
        d[stats::complete.cases(d[, vars]), , drop = FALSE]
    }
    d1 <- prep(data[[1]])
    cn <- names(coef(glm(f, data = d1, family = binomial())))
    res <- mclapply(seq_along(data), function(s) {
        d <- prep(data[[s]])
        tryCatch({
            if (method == "firth") {
                m <- logistf::logistf(f, data = d, pl = FALSE)
                b0 <- coef(m); V <- vcov(m)
            } else {
                m <- glm(f, data = d, family = binomial())
                b0 <- coef(m); V <- sandwich::vcovCL(m, cluster = d$country)
            }
            b <- tryCatch(MASS::mvrnorm(1, b0, V), error = function(e) b0)
            b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    list(coef = out[stats::complete.cases(out), , drop = FALSE],
         n_ev = sum(d1[[all.vars(f)[1]]]), n_obs = nrow(d1))
}

# 4. Spec grid: {canonical reg_trans, broad RoW} x {dem, aut, all} -----------
specs <- tribble(
    ~name,                              ~dv,           ~subset,      ~pred,
    "reg_trans BREAKDOWN [dem]",        "ev_rt_break", "dem",        "neg",
    "reg_trans TRANSITION [aut]",       "ev_rt_trans", "aut",        "pos",
    "RoW-down DETERIORATION [dem]",     "ev_row_down", "dem",        "neg",
    "RoW-up IMPROVEMENT [aut]",         "ev_row_up",   "aut",        "pos",
    "RoW-any INSTABILITY [all]",        "ev_row_any",  NA,           "?"
)
# helper: turn dem/aut label into an at-risk subset column
data <- map(data, ~ mutate(.x, dem = v2x_l >= 2, aut = v2x_l < 2))

message(glue("Fitting {nrow(specs)} specs x 2 methods on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
results <- list()
for (i in seq_len(nrow(specs))) {
    sp <- specs[i, ]
    subset_col <- if (is.na(sp$subset)) NA_character_ else sp$subset
    for (mth in c("glm", "firth")) {
        key <- glue("{sp$name} | {mth}")
        results[[key]] <- c(moc(sp$dv, subset_col, mth), pred = sp$pred)
    }
}
saveRDS(results, here("results_ert_interaction_robust.rds"))

# 5. Report ------------------------------------------------------------------
for (key in names(results)) {
    r <- results[[key]]; m <- r$coef
    cat(sprintf("\n==== %s ==== events=%d obs=%d draws=%d (pred interaction: %s)\n",
                key, r$n_ev, r$n_obs, nrow(m), r$pred))
    if (nrow(m) < 2) { cat("  (insufficient successful draws)\n"); next }
    bd <- m[["disc_c"]]; bi <- m[["disc_supp"]]
    tab <- tibble(
        term = c("disc_c", "disc_supp", "ME disc @low supp", "ME disc @high supp"),
        est  = c(mean(bd), mean(bi), mean(bd - bi*sd_supp), mean(bd + bi*sd_supp)),
        se   = c(sd(bd),   sd(bi),   sd(bd - bi*sd_supp),   sd(bd + bi*sd_supp))
    ) %>% mutate(z = round(est/se, 2), p = round(2*(1-pnorm(abs(z))), 3),
                 sig = ifelse(p<.05,"*",ifelse(p<.1,".","")), across(c(est,se), ~round(.x,3)))
    print(tab)
}
