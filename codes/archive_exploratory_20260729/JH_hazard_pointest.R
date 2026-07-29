# ============================================================================
# NAIVE point-estimate hazard models: what would we conclude if we IGNORED the
# measurement uncertainty in the latent measures -- i.e., plug in posterior
# MEANS of discontent / support / controls and fit a single ordinary model,
# the way Claassen (2020) did before Tai et al. (2024)?
# Compare to the method-of-composition (900-draw) estimates in
# results_regime_hazard_full.rds. Same DV, controls, and specs.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, glue, ERT, logistf)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. ERT events + tenure + prior transitions (correct lead-based coding) ------
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>% group_by(iso3c) %>%
    mutate(new_spell = replace_na(as.integer(v2x_regime != lag(v2x_regime)), 0L),
           spell = cumsum(new_spell), prior_trans = cumsum(new_spell),
           fail_any  = replace_na(as.integer(lead(v2x_regime) != v2x_regime), 0L),
           fail_down = replace_na(as.integer(lead(v2x_regime) <  v2x_regime), 0L),
           fail_up   = replace_na(as.integer(lead(v2x_regime) >  v2x_regime), 0L)) %>%
    group_by(iso3c, spell) %>% mutate(regime_age = row_number()) %>% ungroup() %>%
    select(iso3c, year, v2x_regime, fail_down, fail_up, fail_any, prior_trans, regime_age)

extra_ctrl <- readRDS(file.path(BS_DIR, "country_data.rds")) %>%
    mutate(iso3c = countrycode(vdem_id, "vdem", "iso3c")) %>% filter(!is.na(iso3c)) %>%
    transmute(iso3c, year, gini = gini_disp, lnpop = log(wb_population)) %>%
    distinct(iso3c, year, .keep_all = TRUE)

# 2. POINT ESTIMATES = posterior means across the 900 draws ------------------
load(here("data", "theta_results.rda")); theta_results <- rename(theta_results, discontent = theta)
disc_pt <- theta_results %>% group_by(country, year) %>% summarise(discontent = mean(discontent), .groups = "drop")
supp_pt <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds")) %>% bind_rows() %>%
    group_by(country, year) %>% summarise(support = mean(theta), .groups = "drop")
ctrl_pt <- readRDS(here("data", "exp_ajps_cntrl_list.rds")) %>% bind_rows() %>%
    group_by(country, year) %>%
    summarise(across(c(lnGDP_imp, GDP_imp_grth, Libdem_regUN, Pr_Muslim, Res_cp_WDI_di),
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop")
cc_xwalk <- tibble(country = sort(unique(ctrl_pt$country))) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))

# 3. Single point-estimate dataset -------------------------------------------
dat <- disc_pt %>%
    inner_join(supp_pt, by = c("country","year")) %>%
    left_join(ctrl_pt, by = c("country","year")) %>%
    left_join(cc_xwalk, by = "country") %>%
    left_join(ert_cy, by = c("iso3c","year")) %>%
    left_join(extra_ctrl, by = c("iso3c","year")) %>%
    arrange(country, year) %>% group_by(country) %>%
    mutate(disc_l = lag(discontent,1), supp_l = lag(support,1),
           lnGDP_l = lag(lnGDP_imp,1), grth_l = lag(GDP_imp_grth,1),
           regUN_l = lag(Libdem_regUN,1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di,1),
           gini_l = lag(gini,1), lnpop_l = lag(lnpop,1), tenure = log(regime_age)) %>%
    ungroup() %>%
    mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
           supp_c = supp_l - mean(supp_l, na.rm=TRUE),
           disc_supp = disc_c * supp_c,
           dem = v2x_regime >= 2, aut = v2x_regime < 2)

# 4. Single-fit hazard models (conventional SEs) -----------------------------
rhs <- paste("disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l +",
             "tenure + prior_trans + gini_l + lnpop_l")
fit_one <- function(dv, subset_col, method) {
    d <- dat; if (!is.na(subset_col)) d <- d[!is.na(d[[subset_col]]) & d[[subset_col]], ]
    f <- as.formula(glue("{dv} ~ {rhs}")); d <- d[stats::complete.cases(d[, all.vars(f)]), ]
    tb <- if (method == "firth") {
        m <- logistf::logistf(f, data = d, pl = FALSE)
        tibble(term = names(coef(m)), est = coef(m), se = sqrt(diag(vcov(m))))
    } else {
        m <- glm(f, data = d, family = binomial())
        V <- sandwich::vcovCL(m, cluster = d$country)
        tibble(term = names(coef(m)), est = coef(m), se = sqrt(diag(V)))
    }
    tb %>% mutate(z = est/se, p = 2*(1-pnorm(abs(z))), n_ev = sum(d[[all.vars(f)[1]]]), n = nrow(d))
}
res <- list(
    `ANY [all] glm`      = fit_one("fail_any",  NA,    "glm"),
    `DETERIOR [dem] Firth` = fit_one("fail_down", "dem", "firth"),
    `IMPROVE [aut] Firth`  = fit_one("fail_up",   "aut", "firth")
)

# 5. Report (full coefficients; NAIVE point-estimate SEs) --------------------
nice <- c(disc_c="Political discontent", supp_c="Democratic support", disc_supp="Discontent x Support",
  lnGDP_l="Log GDP pc", grth_l="GDP growth", regUN_l="Regional democracy", musl="% Muslim",
  res_l="Resource dependence", tenure="Regime tenure(log)", prior_trans="Prior transitions",
  gini_l="Income inequality", lnpop_l="Log population")
star <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*","")))
for (nm in names(res)) {
    r <- res[[nm]]
    cat(sprintf("\n======== %s (POINT-ESTIMATE / no uncertainty) ======== events=%d n=%d\n",
                nm, r$n_ev[1], r$n[1]))
    print(r %>% filter(term %in% names(nice)) %>%
          mutate(term = unname(nice[term]), OR = round(exp(est),3),
                 est = round(est,3), se = round(se,3), p = round(p,3), sig = star(p)) %>%
          select(term, est, se, OR, p, sig), n = 30)
}
saveRDS(res, here("results_hazard_pointest.rds"))
