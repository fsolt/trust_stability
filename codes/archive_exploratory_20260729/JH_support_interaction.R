# ============================================================================
# Conditional hypothesis: discontent x democratic support -> regime (in)stability
#
# Theory (Easton diffuse/specific support; Norris "critical citizens";
# Klingemann "dissatisfied democrats"): the effect of political discontent on
# the regime depends on the reservoir of DIFFUSE democratic support.
#   - high discontent + HIGH support = "critical citizens": pressure for reform
#     WITHIN democracy -> not destabilizing (buffered / pro-democratic).
#   - high discontent + LOW support  = "disaffected": open to authoritarian
#     alternatives -> democratic erosion / instability.
# => The pooled main effect of discontent is ~0 (results2.rds) because these
#    opposite conditional effects cancel. The interaction is the theoretically
#    predicted explanation for that null.
#
# Prediction (DV = democracy level, higher = more democratic):
#   coefficient on discontent x support is POSITIVE; i.e. the marginal effect of
#   discontent is negative at low support and ~0/positive at high support.
#
# Measurement uncertainty is propagated on ALL THREE latent inputs by pairing
# posterior draws (discontent draw s x support draw s x V-Dem control draw s)
# and running the method of composition over s = 1..900.
# ============================================================================

# 0. Setup -------------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, plm, countrycode, glue, parallel)
source(here("data", "customFunctions.R"))
select <- dplyr::select

N_DRAWS <- 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. Discontent draws (same sampling as main pipeline) -----------------------
load(here("data", "theta_results.rda"))
theta_results <- theta_results %>% rename(discontent = theta)
disc_list <- split(theta_results, theta_results$draw)
set.seed(123); disc_list <- disc_list[sample(length(disc_list), N_DRAWS)]
rm(theta_results)

# 2. Democratic support draws (DCPO SupDem, from the backsliding project) -----
supp_list <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds"))
supp_list <- lapply(supp_list, function(d) select(as_tibble(d), country, year, support = theta))
supp_list <- supp_list[seq_len(N_DRAWS)]

# 3. Controls (draw-varying V-Dem outcome + covariates) ----------------------
ctrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))[seq_len(N_DRAWS)]

# 4. Assemble triple-draw frames --------------------------------------------
# center discontent & support (grand-mean, per draw) for an interpretable
# interaction; product column avoids pgmm's broken "*" parser.
build_one <- function(disc, supp, ctrl) {
    disc %>%
        select(country, year, discontent) %>%
        inner_join(supp, by = c("country", "year")) %>%
        left_join(ctrl, by = c("country", "year")) %>%
        mutate(
            disc_c    = discontent - mean(discontent, na.rm = TRUE),
            supp_c    = support   - mean(support,   na.rm = TRUE),
            disc_supp = disc_c * supp_c
        ) %>%
        arrange(country, year)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
data <- map(data, ~ plm::pdata.frame(.x, index = c("country", "year")))

# SD of support (for marginal-effect evaluation at +/-1 SD), pooled over draw 1
sd_supp <- sd(data[[1]]$supp_c, na.rm = TRUE)
rm(disc_list, supp_list, ctrl_list)

# 5. Equations (DV = Libdem_VD), explicit product + plain lag ----------------
ctrls <- "lag(lnGDP_imp,1) + lag(GDP_imp_grth,1) + lag(Libdem_regUN,1) + lag(Pr_Muslim,1) + lag(Res_cp_WDI_di,1)"
eq_int <- glue("Libdem_VD ~ lag(Libdem_VD,1:2) + lag(disc_c,1) + lag(supp_c,1) + lag(disc_supp,1) + {ctrls}")

mod_pool <- glue("plm({eq_int}, model='pooling', data = aData)")
mod_gmm  <- glue("pgmm({eq_int} | lag(Libdem_VD,3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country','year'))")

# 6. Parallel method of composition ------------------------------------------
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

message(glue("Fitting discontent x support interaction on {N_DRAWS} draws ({ncores} cores) ..."))
set.seed(313)
res <- list(pooled = mocPar(data, mod_pool, vcovBK),
            gmm    = mocPar(data, mod_gmm,  plmHC))
saveRDS(res, here("results_support_interaction.rds"))

# 7. Report: coefficients + marginal effect of discontent at low/mean/high supp
report <- function(moc, label) {
    cat("\n================ ", label, " (DV = Libdem_VD) ================\n")
    s <- suppressWarnings(MOCsumm(moc)) %>%
        mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error),
               z = round(estimate/std.error, 2), p = round(2*(1-pnorm(abs(z))), 3),
               sig = ifelse(p<.05,"*",ifelse(p<.1,".",""))) %>%
        filter(grepl("disc|supp", term))
    print(s %>% select(term, estimate, std.error, z, p, sig), n = 20)

    # marginal effect of discontent = b_disc + b_int * support, across draws
    bd <- moc[["lag(disc_c, 1)"]]; bi <- moc[["lag(disc_supp, 1)"]]
    me <- tibble(
        support_level = c("low (-1 SD)", "mean", "high (+1 SD)"),
        ME = c(mean(bd - bi*sd_supp), mean(bd), mean(bd + bi*sd_supp)),
        SE = c(sd(bd - bi*sd_supp),   sd(bd),   sd(bd + bi*sd_supp))
    ) %>% mutate(z = round(ME/SE,2), p = round(2*(1-pnorm(abs(z))),3),
                 across(c(ME,SE), ~round(.x,3)))
    cat("--- marginal effect of DISCONTENT on democracy, by support level ---\n")
    print(me)
}
report(res$pooled, "Pooled OLS")
report(res$gmm,    "System GMM")
