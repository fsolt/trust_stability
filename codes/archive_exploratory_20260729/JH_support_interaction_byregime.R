# ============================================================================
# discontent x democratic support -> democracy level, SPLIT BY REGIME TYPE
# (companion to JH_support_interaction.R, which pools all regimes)
#
# Critical-citizens theory is sharpest within-regime:
#   DEMOCRACIES: high support buffers discontent's erosive effect
#                -> predict disc x supp POSITIVE (erosion only where support low)
#   AUTOCRACIES: high democratic support + discontent = democratization pressure
#                -> disc x supp may raise democracy level
# Each regime subsample is centered internally; triple measurement uncertainty
# (discontent x support x V-Dem) propagated by method of composition (900 draws).
# ============================================================================

# 0. Setup -------------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, plm, countrycode, glue, parallel)
source(here("data", "customFunctions.R"))
select <- dplyr::select

N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1-3. Inputs (as in JH_support_interaction.R) -------------------------------
load(here("data", "theta_results.rda"))
theta_results <- theta_results %>% rename(discontent = theta)
disc_list <- split(theta_results, theta_results$draw)
set.seed(123); disc_list <- disc_list[sample(length(disc_list), N_DRAWS)]
rm(theta_results)

supp_list <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds"))
supp_list <- lapply(supp_list, function(d) select(as_tibble(d), country, year, support = theta))[seq_len(N_DRAWS)]

ctrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))[seq_len(N_DRAWS)]

# 4. Raw frames, then regime subsamples (centered within subsample) ----------
build_raw <- function(disc, supp, ctrl) {
    disc %>% select(country, year, discontent) %>%
        inner_join(supp, by = c("country", "year")) %>%
        left_join(ctrl, by = c("country", "year")) %>%
        arrange(country, year)
}
raw <- pmap(list(disc_list, supp_list, ctrl_list), build_raw)
rm(disc_list, supp_list, ctrl_list)

make_sub <- function(raw_list, keep_dem) {
    map(raw_list, function(d) {
        d2 <- d %>% filter(!is.na(Regime_VD), if (keep_dem) Regime_VD > 1 else Regime_VD <= 1) %>%
            mutate(disc_c = discontent - mean(discontent, na.rm = TRUE),
                   supp_c = support   - mean(support,   na.rm = TRUE),
                   disc_supp = disc_c * supp_c) %>%
            arrange(country, year)
        plm::pdata.frame(d2, index = c("country", "year"))
    })
}
data_dem <- make_sub(raw, TRUE)
data_aut <- make_sub(raw, FALSE)
sd_dem <- sd(data_dem[[1]]$supp_c, na.rm = TRUE)
sd_aut <- sd(data_aut[[1]]$supp_c, na.rm = TRUE)
cat(sprintf("DEM subsample CY (draw1): %d | AUT: %d | SD(supp) dem=%.3f aut=%.3f\n",
            sum(!is.na(data_dem[[1]]$disc_c)), sum(!is.na(data_aut[[1]]$disc_c)), sd_dem, sd_aut))

# 5. Equation + parallel MOC -------------------------------------------------
ctrls <- "lag(lnGDP_imp,1) + lag(GDP_imp_grth,1) + lag(Libdem_regUN,1) + lag(Pr_Muslim,1) + lag(Res_cp_WDI_di,1)"
eq_int <- glue("Libdem_VD ~ lag(Libdem_VD,1:2) + lag(disc_c,1) + lag(supp_c,1) + lag(disc_supp,1) + {ctrls}")
mod_pool <- glue("plm({eq_int}, model='pooling', data = aData)")
mod_gmm  <- glue("pgmm({eq_int} | lag(Libdem_VD,3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country','year'))")

ncores <- max(1L, detectCores() - 2L)
mocPar <- function(data, model_str, vcovfun) {
    aData <- data[[1]]
    cn <- names(coef(eval(parse(text = model_str))))
    res <- mclapply(seq_along(data), function(s) {
        aData <- data[[s]]
        tryCatch({
            m <- eval(parse(text = model_str))
            b <- tryCatch(MASS::mvrnorm(1, coef(m), vcovfun(m)), error = function(e) coef(m))
            b[cn]
        }, error = function(e) setNames(rep(NA_real_, length(cn)), cn))
    }, mc.cores = ncores, mc.set.seed = TRUE)
    out <- as.data.frame(do.call(rbind, res)); names(out) <- cn
    out[stats::complete.cases(out), , drop = FALSE]
}

message(glue("Fitting regime-split interaction on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(
    dem_pooled = mocPar(data_dem, mod_pool, vcovBK),
    dem_gmm    = mocPar(data_dem, mod_gmm,  plmHC),
    aut_pooled = mocPar(data_aut, mod_pool, vcovBK),
    aut_gmm    = mocPar(data_aut, mod_gmm,  plmHC)
)
saveRDS(res, here("results_support_interaction_byregime.rds"))

# 6. Report ------------------------------------------------------------------
report <- function(moc, label, sdv) {
    cat("\n================ ", label, " (DV = Libdem_VD) ================\n")
    s <- suppressWarnings(MOCsumm(moc)) %>%
        mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error),
               z = round(estimate/std.error, 2), p = round(2*(1-pnorm(abs(z))), 3),
               sig = ifelse(p<.05,"*",ifelse(p<.1,".",""))) %>%
        filter(grepl("disc|supp", term))
    print(s %>% select(term, estimate, std.error, z, p, sig))
    bd <- moc[["lag(disc_c, 1)"]]; bi <- moc[["lag(disc_supp, 1)"]]
    me <- tibble(support = c("low(-1SD)","mean","high(+1SD)"),
                 ME = c(mean(bd - bi*sdv), mean(bd), mean(bd + bi*sdv)),
                 SE = c(sd(bd - bi*sdv),   sd(bd),   sd(bd + bi*sdv))) %>%
        mutate(z = round(ME/SE,2), p = round(2*(1-pnorm(abs(z))),3),
               sig = ifelse(p<.05,"*",ifelse(p<.1,".","")), across(c(ME,SE), ~round(.x,3)))
    cat("  marginal effect of DISCONTENT by support level:\n"); print(me)
}
report(res$dem_pooled, "DEMOCRACIES - Pooled OLS", sd_dem)
report(res$dem_gmm,    "DEMOCRACIES - System GMM", sd_dem)
report(res$aut_pooled, "AUTOCRACIES - Pooled OLS", sd_aut)
report(res$aut_gmm,    "AUTOCRACIES - System GMM", sd_aut)
