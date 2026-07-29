# ============================================================================
# Specification robustness for the fully-specified regime-hazard model:
#  (P) add PERIOD dummies (1988-99 ref, 2000-09, 2010+) -- regime transitions
#      cluster in temporal waves (post-Cold War, 2010s backsliding); omitted
#      period effects correlated with discontent would confound the focal terms.
#  (C) replace log(tenure) with a CUBIC in regime age (Carter-Signorino-style)
#      to show duration-dependence parameterization does not drive results.
# Each run under BOTH estimation modes: naive point estimates and 900-draw MOC.
# DV = broad RoW regime-category change (lead-coded), as in the main model.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, MASS, glue, ERT, parallel, logistf)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- if (exists("N_DRAWS")) N_DRAWS else 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. ERT: RoW spells, lead-coded failure, tenure, prior transitions ----------
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

# 2. Inputs -------------------------------------------------------------------
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
               tenure = log(regime_age),
               t1 = regime_age, t2 = regime_age^2, t3 = regime_age^3) %>%
        ungroup() %>%
        mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
               supp_c = supp_l - mean(supp_l, na.rm=TRUE),
               disc_supp = disc_c * supp_c,
               per2000s = as.integer(year >= 2000 & year <= 2009),   # ref: 1988-99
               per2010s = as.integer(year >= 2010),
               dem = v2x_regime >= 2, aut = v2x_regime < 2)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
rm(disc_list, supp_list, ctrl_list)

# point-estimate (naive) dataset: replace draw-varying inputs with their
# posterior means (rows are identical across draw frames, so rowMeans is exact)
naive_dat <- data[[1]]
for (v in c("disc_l","supp_l","lnGDP_l","grth_l","regUN_l","musl","res_l","gini_l","lnpop_l")) {
    naive_dat[[v]] <- rowMeans(vapply(data, function(d) d[[v]], numeric(nrow(naive_dat))), na.rm = TRUE)
}
naive_dat <- naive_dat %>%
    mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
           supp_c = supp_l - mean(supp_l, na.rm=TRUE),
           disc_supp = disc_c * supp_c)

# 3. Estimators ---------------------------------------------------------------
ncores <- max(1L, detectCores() - 2L)
base   <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l + prior_trans + gini_l + lnpop_l"
rhs_P  <- glue("{base} + tenure + per2000s + per2010s")   # period dummies
rhs_C  <- glue("{base} + t1 + t2 + t3")                    # cubic duration

fit_naive <- function(rhs, dv, subset_col, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); d <- naive_dat
    if (!is.na(subset_col)) d <- d[!is.na(d[[subset_col]]) & d[[subset_col]], ]
    d <- d[stats::complete.cases(d[, all.vars(f)]), ]
    if (method == "firth") { m <- logistf(f, data = d, pl = FALSE); b <- coef(m); V <- vcov(m) }
    else { m <- glm(f, data = d, family = binomial()); b <- coef(m); V <- sandwich::vcovCL(m, cluster = d$country) }
    tibble(term = names(b), est = b, se = sqrt(diag(V))) %>%
        mutate(p = 2*(1-pnorm(abs(est/se))), n_ev = sum(d[[all.vars(f)[1]]]))
}
moc <- function(rhs, dv, subset_col, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); vars <- all.vars(f)
    prep <- function(d) {
        if (!is.na(subset_col)) d <- d[!is.na(d[[subset_col]]) & d[[subset_col]], ]
        d[stats::complete.cases(d[, vars]), , drop = FALSE]
    }
    d1 <- prep(data[[1]]); cn <- names(coef(glm(f, data = d1, family = binomial())))
    res <- mclapply(seq_along(data), function(s) {
        d <- prep(data[[s]])
        tryCatch({
            if (method == "firth") { m <- logistf(f, data = d, pl = FALSE); b0 <- coef(m); V <- vcov(m) }
            else { m <- glm(f, data = d, family = binomial()); b0 <- coef(m); V <- sandwich::vcovCL(m, cluster = d$country) }
            b <- tryCatch(MASS::mvrnorm(1, b0, V), error = function(e) b0); b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    list(coef = out[stats::complete.cases(out), , drop = FALSE], n_ev = sum(d1[[vars[1]]]), n_obs = nrow(d1))
}

specs <- tribble(~lab, ~dv, ~sub, ~method,
    "ANY [all]",   "fail_any",  NA_character_, "glm",
    "DETER [dem]", "fail_down", "dem",         "firth",
    "IMPRV [aut]", "fail_up",   "aut",         "firth")

message(glue("Running period-dummy and cubic-duration robustness on {N_DRAWS} draws ..."))
set.seed(313)
out <- list()
for (i in seq_len(nrow(specs))) {
    s <- specs[i, ]
    out[[glue("{s$lab} PERIOD naive")]] <- fit_naive(rhs_P, s$dv, s$sub, s$method)
    out[[glue("{s$lab} PERIOD moc")]]   <- moc(rhs_P, s$dv, s$sub, s$method)
    out[[glue("{s$lab} CUBIC moc")]]    <- moc(rhs_C, s$dv, s$sub, s$method)
}
saveRDS(out, here("results_hazard_spec_robust.rds"))

# 4. Report focal + period terms ----------------------------------------------
show <- c("disc_c","supp_c","disc_supp","per2000s","per2010s","tenure")
star <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*","")))
for (nm in names(out)) {
    r <- out[[nm]]
    cat("\n====", nm, "====\n")
    if (is.data.frame(r)) {   # naive
        print(r %>% filter(term %in% show) %>%
              mutate(across(c(est,se,p), ~round(.x,3)), sig = star(p)) %>% select(term,est,se,p,sig,n_ev))
    } else {                   # moc
        m <- r$coef
        tb <- map_dfr(intersect(show, names(m)), \(tm) tibble(term=tm, est=mean(m[[tm]]), se=sd(m[[tm]]))) %>%
            mutate(p = 2*(1-pnorm(abs(est/se))), across(c(est,se,p), ~round(.x,3)), sig = star(p))
        cat("events:", r$n_ev, " draws:", nrow(m), "\n"); print(tb)
    }
}
