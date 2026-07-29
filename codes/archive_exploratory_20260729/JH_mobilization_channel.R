# ============================================================================
# Proximate-channel test: does political discontent predict mass mobilization?
#
# Motivation: discontent does not move regime *level* (results2.rds) or discrete
# regime *transitions* (results_ert_discrete.rds). This script tests the more
# proximate behavioral channel -- anti-/pro-regime MASS MOBILIZATION -- that the
# discontent -> instability chain must run through. Pre-committed as an
# ADDITIONAL hypothesis: the result is reported regardless of significance.
#
# DV: V-Dem general mass mobilization (v2cagenmob), higher = more frequent/large
# mobilization events. We propagate BOTH sides' measurement uncertainty:
#   - discontent (IV): 900 posterior draws  (method of composition)
#   - mobilization (DV): drawn each iteration from N(v2cagenmob, v2cagenmob_sd)
# Model structure mirrors JH_Preliminary analysis.R exactly (pooled OLS + system
# GMM, pooled and regime-split), so coefficients are directly comparable.
# ============================================================================

# 0. Setup -------------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, plm, countrycode, glue, ERT)
source(here("data", "customFunctions.R"))
select <- dplyr::select

N_DRAWS <- 900L
set.seed(313)                                  # reproducible DV-uncertainty draws

# 1. V-Dem mobilization DV (point + measurement SD) --------------------------
data(vdem, package = "ERT")
mob_cy <- vdem %>%
    transmute(iso3c = countrycode(country_id, "vdem", "iso3c"),
              year, mob_mean = v2cagenmob, mob_sd = v2cagenmob_sd) %>%
    filter(!is.na(iso3c), !is.na(mob_mean))

# 2. Discontent + interest draws (same sampling as the main pipeline) --------
load(here("data", "theta_results.rda"))
theta_results <- theta_results %>% rename(discontent = theta)
discontent_results <- split(theta_results, theta_results$draw)
set.seed(123); discontent_results <- discontent_results[sample(length(discontent_results), N_DRAWS)]

load(here("data", "interest_theta_results.rda"))
theta_results <- theta_results %>% rename(interest = theta)
interest_results <- split(theta_results, theta_results$draw)
set.seed(123); interest_results <- interest_results[sample(length(interest_results), N_DRAWS)]
rm(theta_results)

# 3. Controls + crosswalk ----------------------------------------------------
exp_ajps_cntrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))[seq_len(N_DRAWS)]
cc_xwalk <- tibble(country = sort(unique(exp_ajps_cntrl_list[[1]]$country))) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))

# 4. Assemble draw-specific frames (mirrors main script's trim logic) --------
set.seed(313)
build_one <- function(disc, intr, ctrl) {
    disc %>%
        left_join(intr, by = c("country", "year")) %>%
        select(-draw.x, -draw.y) %>%
        left_join(ctrl, by = c("country", "year")) %>%
        left_join(cc_xwalk, by = "country") %>%
        left_join(mob_cy, by = c("iso3c", "year")) %>%
        mutate(
            # DV-side measurement-uncertainty draw
            mob = if_else(!is.na(mob_mean) & !is.na(mob_sd),
                          rnorm(n(), mob_mean, mob_sd), mob_mean),
            # regime-split trims, exactly as in JH_Preliminary analysis.R
            discontent_dem_trim = case_when(is.na(discontent) ~ NA_real_,
                                            Regime_VD > 1 ~ discontent, TRUE ~ 0),
            discontent_aut_trim = case_when(is.na(discontent) ~ NA_real_,
                                            Regime_VD <= 1 ~ discontent, TRUE ~ 0),
            interest_dem_trim   = case_when(is.na(interest) ~ NA_real_,
                                            Regime_VD > 1 ~ interest, TRUE ~ 0),
            interest_aut_trim   = case_when(is.na(interest) ~ NA_real_,
                                            Regime_VD <= 1 ~ interest, TRUE ~ 0),
            # explicit interaction products. pgmm's parser chokes on the "*"
            # operator (plm 2.6.6 + R>=4.2), but lag(a,1)*lag(b,1)==lag(a*b,1),
            # so entering the product as a column is exactly equivalent.
            disc_int       = discontent * interest,
            discdem_intdem = discontent_dem_trim * interest_dem_trim,
            discaut_intaut = discontent_aut_trim * interest_aut_trim
        ) %>%
        select(country, year, mob, discontent, interest, disc_int,
               discontent_dem_trim, discontent_aut_trim,
               interest_dem_trim, interest_aut_trim,
               discdem_intdem, discaut_intaut, everything())
}
data <- pmap(list(discontent_results, interest_results, exp_ajps_cntrl_list), build_one)
data <- map(data, ~ plm::pdata.frame(.x, index = c("country", "year")))
rm(exp_ajps_cntrl_list, discontent_results, interest_results)

# 5. Model equations (DV = mob), mirroring ls_eq2 ----------------------------
# Plain lag() (plm attached) + explicit interaction products (see build_one).
ctrls <- "lag(lnGDP_imp, 1) + lag(GDP_imp_grth, 1) + lag(Libdem_regUN, 1) + lag(Pr_Muslim, 1) + lag(Res_cp_WDI_di, 1)"

eq_pool <- glue("mob ~ lag(mob, 1:2) + lag(discontent, 1) + lag(interest, 1) + lag(disc_int, 1) + {ctrls}")
eq_reg  <- glue("mob ~ lag(mob, 1:2) + lag(discontent_dem_trim, 1) + lag(interest_dem_trim, 1) + lag(discdem_intdem, 1) + lag(discontent_aut_trim, 1) + lag(interest_aut_trim, 1) + lag(discaut_intaut, 1) + {ctrls}")

mod_pool    <- glue("plm({eq_pool}, model = 'pooling', data = aData)")
mod_poolReg <- glue("plm({eq_reg}, model = 'pooling', data = aData)")
mod_gmm     <- glue("pgmm({eq_pool} | lag(mob, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country','year'))")
mod_gmmReg  <- glue("pgmm({eq_reg} | lag(mob, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country','year'))")

# 6. Fit via method of composition (parallelized across draws) ---------------
# Same Rubin logic as methodComposition1/2 (draw one beta from MVN(coef,Vcov)
# per posterior draw), but distributed over cores to keep runtime tractable.
library(parallel)
ncores <- max(1L, detectCores() - 2L)

mocPar <- function(data, model_str, vcovfun) {
    aData <- data[[1]]
    cn <- names(coef(eval(parse(text = model_str))))    # reference coef names
    res <- mclapply(seq_along(data), function(s) {
        aData <- data[[s]]
        tryCatch({
            m <- eval(parse(text = model_str))
            b <- tryCatch(MASS::mvrnorm(1, coef(m), vcovfun(m)),
                          error = function(e) coef(m))    # Vcov not PD -> point est.
            b[cn]                                         # consistent order/length
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res))
    names(out) <- cn
    out[stats::complete.cases(out), , drop = FALSE]       # drop failed draws
}

message(glue("Fitting mobilization-channel models on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(
    pooled          = mocPar(data, mod_pool,    vcovBK),
    `pooled-regime` = mocPar(data, mod_poolReg, vcovBK),
    gmm             = mocPar(data, mod_gmm,     plmHC),
    `gmm-regime`    = mocPar(data, mod_gmmReg,  plmHC)
)
saveRDS(res, here("results_mobilization.rds"))

# 7. Report (discontent / interest terms) ------------------------------------
for (nm in names(res)) {
    cat("\n================ ", nm, " (DV = general mass mobilization) ================\n")
    s <- MOCsumm(res[[nm]]) %>%
        mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error),
               z = round(estimate / std.error, 2),
               p = round(2 * (1 - pnorm(abs(z))), 3))
    print(s, n = 30)
}
