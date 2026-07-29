# ============================================================================
# Powell & Thyne coup robustness: discontent x support -> coup attempts
# Source: powell_thyne_ccode_year.txt V2024.10.30 (official country-year file,
# retrieved from the Internet Archive copy of uky.edu/~clthyn2/coup_data;
# preserved in data-raw/powell_thyne_ccode_year_V20241030.txt).
# Coding: coup1-coup4 per year; 1 = failed attempt, 2 = successful coup.
# DV: any coup attempt (>=1) in country-year; successful-only as check.
# Models: (a) all regimes, glm + country-clustered SEs; (b) autocracies only,
# Firth. Controls = parsimonious set + log RoW regime tenure. Naive vs MOC.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, MASS, glue, ERT, parallel, logistf)
source(here("data", "customFunctions.R")); select <- dplyr::select
N_DRAWS <- if (exists("N_DRAWS")) N_DRAWS else 900L
BS_DIR  <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# 1. Coup DV ------------------------------------------------------------------
pt <- read_tsv(here("data-raw", "powell_thyne_ccode_year_V20241030.txt"),
               show_col_types = FALSE) %>%
    mutate(iso3c = countrycode(ccode, "cown", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    rowwise() %>%
    mutate(coup_att = as.integer(any(c_across(coup1:coup4) %in% c(1, 2))),
           coup_suc = as.integer(any(c_across(coup1:coup4) == 2))) %>%
    ungroup() %>%
    select(iso3c, year, coup_att, coup_suc) %>%
    distinct(iso3c, year, .keep_all = TRUE)

# 2. RoW tenure clock (as elsewhere) + latent inputs --------------------------
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>% group_by(iso3c) %>%
    mutate(new_spell = replace_na(as.integer(v2x_regime != lag(v2x_regime)), 0L),
           spell = cumsum(new_spell)) %>%
    group_by(iso3c, spell) %>% mutate(regime_age = row_number()) %>% ungroup() %>%
    select(iso3c, year, v2x_regime, regime_age)

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
        inner_join(pt, by = c("iso3c","year")) %>%
        arrange(country, year) %>% group_by(country) %>%
        mutate(disc_l = lag(discontent,1), supp_l = lag(support,1),
               lnGDP_l = lag(lnGDP_imp,1), grth_l = lag(GDP_imp_grth,1),
               regUN_l = lag(Libdem_regUN,1), musl = Pr_Muslim, res_l = lag(Res_cp_WDI_di,1),
               tenure = log(regime_age)) %>%
        ungroup() %>%
        mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
               supp_c = supp_l - mean(supp_l, na.rm=TRUE),
               disc_supp = disc_c * supp_c,
               aut = v2x_regime < 2)
}
data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)
rm(disc_list, supp_list, ctrl_list)

naive_dat <- data[[1]]
for (v in c("disc_l","supp_l","lnGDP_l","grth_l","regUN_l","musl","res_l")) {
    naive_dat[[v]] <- rowMeans(vapply(data, function(d) d[[v]], numeric(nrow(naive_dat))), na.rm = TRUE)
}
naive_dat <- naive_dat %>%
    mutate(disc_c = disc_l - mean(disc_l, na.rm=TRUE),
           supp_c = supp_l - mean(supp_l, na.rm=TRUE),
           disc_supp = disc_c * supp_c)

# 3. Estimation ----------------------------------------------------------------
ncores <- max(1L, detectCores() - 2L)
rhs <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l + tenure"

fit_naive <- function(dv, subset_aut, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); d <- naive_dat
    if (subset_aut) d <- d[!is.na(d$aut) & d$aut, ]
    d <- d[stats::complete.cases(d[, all.vars(f)]), ]
    if (method == "firth") { m <- logistf(f, data = d, pl = FALSE); b <- coef(m); V <- vcov(m) }
    else { m <- glm(f, data = d, family = binomial()); b <- coef(m); V <- sandwich::vcovCL(m, cluster = d$country) }
    tibble(term = names(b), est = b, se = sqrt(diag(V))) %>%
        mutate(p = 2*(1-pnorm(abs(est/se))), n_ev = sum(d[[all.vars(f)[1]]]), n = nrow(d))
}
mocFit <- function(dv, subset_aut, method) {
    f <- as.formula(glue("{dv} ~ {rhs}")); vars <- all.vars(f)
    prep <- function(d) { if (subset_aut) d <- d[!is.na(d$aut) & d$aut, ]; d[stats::complete.cases(d[, vars]), , drop = FALSE] }
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

message(glue("Fitting coup models on {N_DRAWS} draws ..."))
set.seed(313)
out <- list(
    `attempt [all] naive`  = fit_naive("coup_att", FALSE, "glm"),
    `attempt [all] moc`    = mocFit("coup_att", FALSE, "glm"),
    `attempt [aut] naive`  = fit_naive("coup_att", TRUE, "firth"),
    `attempt [aut] moc`    = mocFit("coup_att", TRUE, "firth"),
    `success [all] naive`  = fit_naive("coup_suc", FALSE, "firth"),
    `success [all] moc`    = mocFit("coup_suc", FALSE, "firth")
)
saveRDS(out, here("results_coup_robust.rds"))

# 4. Report --------------------------------------------------------------------
star <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*","")))
show <- c("disc_c","supp_c","disc_supp","tenure","lnGDP_l")
for (nm in names(out)) {
    r <- out[[nm]]
    cat("\n====", nm, "====\n")
    if (is.data.frame(r)) {
        cat("events:", r$n_ev[1], " obs:", r$n[1], "\n")
        print(r %>% filter(term %in% show) %>% mutate(across(c(est,se,p),~round(.x,3)), sig=star(p)) %>% select(term,est,se,p,sig))
    } else {
        m <- r$coef
        cat("events:", r$n_ev, " obs:", r$n_obs, " draws:", nrow(m), "\n")
        tb <- map_dfr(intersect(show, names(m)), \(tm) tibble(term=tm, est=mean(m[[tm]]), se=sd(m[[tm]]))) %>%
            mutate(p = 2*(1-pnorm(abs(est/se))), across(c(est,se,p), ~round(.x,3)), sig = star(p))
        print(tb)
    }
}
