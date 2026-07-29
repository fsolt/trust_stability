# ============================================================================
# CORRECT DV: discontent x democratic support -> REGIME TRANSITION (ERT)
#
# Rationale (per JC): the V-Dem liberal democracy *level* (Libdem_VD) measures
# democratic quality/erosion, NOT regime stability, and that DV is already
# worked over by Claassen (2020) / Tai et al. Regime (in)stability must be an
# actual survival/breakdown/transition measure. We therefore test the
# discontent x support interaction on the ERT reg_trans outcome via a
# discrete-time (grouped-duration) hazard model -- the correct combination that
# was NOT yet run (the interaction had only been tested on Libdem_VD).
#
# Specs (mirror JH_ERT_discrete.R + interaction):
#   (A) any transition      : reg_trans != 0,  all regimes at risk
#   (B) democratic breakdown: reg_trans == -1, democracies at risk (v2x_l>=2)
#   (C) democratic transition: reg_trans == 1, autocracies at risk (v2x_l<2)
# Predictions: (B) disc x supp NEGATIVE (support buffers breakdown);
#              (C) disc x supp POSITIVE (support channels discontent to democratization).
# Triple measurement uncertainty (discontent x support x V-Dem) via method of
# composition over 900 paired draws; country-clustered SEs.
# ============================================================================

# 0. Setup -------------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, MASS, glue, ERT, parallel)
source(here("data", "customFunctions.R"))
select <- dplyr::select

N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. ERT reg_trans outcome + regime-age clock --------------------------------
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime, reg_trans) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(trans_ev = if_else(is.na(reg_trans), 0L, as.integer(reg_trans != 0)),
           spell    = cumsum(replace_na(lag(trans_ev, 1L), 0L))) %>%
    group_by(iso3c, spell) %>%
    mutate(regime_age = row_number()) %>%
    ungroup() %>%
    select(iso3c, year, v2x_regime, reg_trans, regime_age)

# 2. Inputs: discontent, support, controls (all draw-varying, paired by index)
load(here("data", "theta_results.rda"))
theta_results <- theta_results %>% rename(discontent = theta)
disc_list <- split(theta_results, theta_results$draw)
set.seed(123); disc_list <- disc_list[sample(length(disc_list), N_DRAWS)]
rm(theta_results)

supp_list <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds"))
supp_list <- lapply(supp_list, function(d) select(as_tibble(d), country, year, support = theta))[seq_len(N_DRAWS)]

ctrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))[seq_len(N_DRAWS)]
cc_xwalk <- tibble(country = sort(unique(ctrl_list[[1]]$country))) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))

# 3. Assemble paired-draw frames ---------------------------------------------
build_one <- function(disc, supp, ctrl) {
    disc %>% select(country, year, discontent) %>%
        inner_join(supp, by = c("country", "year")) %>%
        left_join(ctrl, by = c("country", "year")) %>%
        left_join(cc_xwalk, by = "country") %>%
        left_join(ert_cy, by = c("iso3c", "year")) %>%
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(disc_l = lag(discontent, 1), supp_l = lag(support, 1),
               lnGDP_l = lag(lnGDP_imp, 1), grth_l = lag(GDP_imp_grth, 1),
               regUN_l = lag(Libdem_regUN, 1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di, 1),
               v2x_l = lag(v2x_regime, 1),
               ev_any = as.integer(reg_trans != 0),
               ev_break = as.integer(reg_trans == -1),
               ev_trans = as.integer(reg_trans == 1),
               t1 = regime_age, t2 = regime_age^2, t3 = regime_age^3) %>%
        ungroup() %>%
        # center lagged discontent & support (grand mean) + product
        mutate(disc_c = disc_l - mean(disc_l, na.rm = TRUE),
               supp_c = supp_l - mean(supp_l, na.rm = TRUE),
               disc_supp = disc_c * supp_c)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
sd_supp <- sd(data[[1]]$supp_c, na.rm = TRUE)
rm(disc_list, supp_list, ctrl_list)

# 4. Parallel method of composition for discrete-time logit ------------------
ncores <- max(1L, detectCores() - 2L)
mocParGLM <- function(data, formula, subset_expr = NULL) {
    subset_q <- rlang::enquo(subset_expr)              # capture without evaluating
    has_sub  <- !rlang::quo_is_null(subset_q)
    formula  <- as.formula(formula)
    vars     <- all.vars(formula)
    prep <- function(d) {                              # subset + listwise-complete on model vars
        if (has_sub) d <- dplyr::filter(d, !!subset_q)
        d[stats::complete.cases(d[, vars]), , drop = FALSE]
    }
    d1 <- prep(data[[1]]); fit1 <- glm(formula, data = d1, family = binomial()); cn <- names(coef(fit1))
    res <- mclapply(seq_along(data), function(s) {
        d <- prep(data[[s]])
        tryCatch({
            m <- glm(formula, data = d, family = binomial())
            V <- sandwich::vcovCL(m, cluster = d$country)   # cluster vector aligned to used rows
            b <- tryCatch(MASS::mvrnorm(1, coef(m), V), error = function(e) coef(m))
            b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    list(coef = out[stats::complete.cases(out), , drop = FALSE],
         n_ev = sum(d1[[vars[1]]]), n_obs = nrow(d1))
}

rhs <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l + t1 + t2 + t3"
message(glue("Fitting ERT-transition interaction on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(
    any   = mocParGLM(data, glue("ev_any ~ {rhs}")),
    break_dem = mocParGLM(data, glue("ev_break ~ {rhs}"), subset_expr = v2x_l >= 2),
    trans_aut = mocParGLM(data, glue("ev_trans ~ {rhs}"), subset_expr = v2x_l < 2)
)
saveRDS(res, here("results_ert_interaction.rds"))

# 5. Report: interaction + marginal effect of discontent by support ----------
report <- function(r, label) {
    moc <- r$coef
    cat(sprintf("\n======== %s ========  (events=%d, obs=%d, draws=%d)\n",
                label, r$n_ev, r$n_obs, nrow(moc)))
    s <- suppressWarnings(MOCsumm(moc)) %>%
        mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error),
               z = round(estimate/std.error, 2), p = round(2*(1-pnorm(abs(z))), 3),
               sig = ifelse(p<.05,"*",ifelse(p<.1,".",""))) %>%
        filter(grepl("disc|supp", term))
    print(s %>% select(term, estimate, std.error, z, p, sig))
    bd <- moc[["disc_c"]]; bi <- moc[["disc_supp"]]
    me <- tibble(support = c("low(-1SD)","mean","high(+1SD)"),
                 logOR = c(mean(bd - bi*sd_supp), mean(bd), mean(bd + bi*sd_supp)),
                 SE = c(sd(bd - bi*sd_supp), sd(bd), sd(bd + bi*sd_supp))) %>%
        mutate(z = round(logOR/SE,2), p = round(2*(1-pnorm(abs(z))),3),
               OR = round(exp(logOR),3), sig = ifelse(p<.05,"*",ifelse(p<.1,".","")),
               across(c(logOR,SE), ~round(.x,3)))
    cat("  marginal effect of DISCONTENT (log-odds of the event) by support:\n"); print(me)
}
report(res$any,       "(A) ANY transition [all regimes]")
report(res$break_dem, "(B) DEMOCRATIC BREAKDOWN [democracies]  <- key test, rare events")
report(res$trans_aut, "(C) DEMOCRATIC TRANSITION [autocracies]")
