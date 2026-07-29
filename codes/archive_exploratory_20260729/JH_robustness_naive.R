# ============================================================================
# Naive (point-estimate) counterparts of the robustness specifications, so the
# appendix can pair each MOC estimate with its point-estimate twin and show the
# naive->null significance loss is systematic across estimators / DVs:
#   (i)  discrete-time logit, ERT reg_trans DV  (pairs results_ert_interaction.rds)
#   (ii) Cox proportional-hazards survival       (pairs results_regime_survival.rds)
# Same parsimonious control set as those MOC files. Point estimates = posterior
# means of discontent, support, and controls.
# ============================================================================

rm(list = ls())
library(pacman)
p_load(tidyverse, rio, here, countrycode, sandwich, glue, ERT, survival)
source(here("data", "customFunctions.R")); select <- dplyr::select
BS_DIR <- "/Users/jeonghochoi/Library/CloudStorage/Dropbox/Research_Project/Published/2026 - Choi - Different Patterns of Democratic Backsliding/data"

# --- ERT: reg_trans events + reg_trans-spell age; is_dem spells + lead failure -
ert_cy <- ERT::get_eps() %>%
    select(country_id, year, v2x_regime, reg_trans) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>% group_by(iso3c) %>%
    mutate(trans_ev = if_else(is.na(reg_trans), 0L, as.integer(reg_trans != 0)),
           rt_spell = cumsum(replace_na(lag(trans_ev, 1L), 0L)),
           is_dem   = as.integer(v2x_regime >= 2),
           dem_chg  = replace_na(as.integer(is_dem != lag(is_dem)), 0L),
           dem_spell = cumsum(dem_chg),
           fail     = replace_na(as.integer(lead(is_dem) != is_dem), 0L)) %>%
    group_by(iso3c, rt_spell) %>% mutate(rt_age = row_number()) %>%
    group_by(iso3c, dem_spell) %>% mutate(age = row_number(), spell_state = first(is_dem)) %>%
    ungroup() %>%
    select(iso3c, year, v2x_regime, reg_trans, rt_age, age, fail, spell_state)

# --- point estimates (posterior means) --------------------------------------
load(here("data", "theta_results.rda")); theta_results <- rename(theta_results, discontent = theta)
disc_pt <- theta_results %>% group_by(country, year) %>% summarise(discontent = mean(discontent), .groups="drop")
supp_pt <- readRDS(file.path(BS_DIR, "exp_dcpo_theta.rds")) %>% bind_rows() %>%
    group_by(country, year) %>% summarise(support = mean(theta), .groups="drop")
ctrl_pt <- readRDS(here("data", "exp_ajps_cntrl_list.rds")) %>% bind_rows() %>%
    group_by(country, year) %>%
    summarise(across(c(lnGDP_imp, GDP_imp_grth, Libdem_regUN, Pr_Muslim, Res_cp_WDI_di),
                     ~ mean(.x, na.rm = TRUE)), .groups="drop")
cc_xwalk <- tibble(country = sort(unique(ctrl_pt$country))) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))

dat <- disc_pt %>% inner_join(supp_pt, by=c("country","year")) %>%
    left_join(ctrl_pt, by=c("country","year")) %>% left_join(cc_xwalk, by="country") %>%
    left_join(ert_cy, by=c("iso3c","year")) %>%
    arrange(country, year) %>% group_by(country) %>%
    mutate(disc_l=lag(discontent,1), supp_l=lag(support,1),
           lnGDP_l=lag(lnGDP_imp,1), grth_l=lag(GDP_imp_grth,1), regUN_l=lag(Libdem_regUN,1),
           musl=Pr_Muslim, res_l=lag(Res_cp_WDI_di,1), v2x_l=lag(v2x_regime,1),
           ev_break=as.integer(reg_trans==-1), ev_trans=as.integer(reg_trans==1),
           t1=rt_age, t2=rt_age^2, t3=rt_age^3, t0=age-1, t1c=age) %>% ungroup() %>%
    mutate(disc_c=disc_l-mean(disc_l,na.rm=TRUE), supp_c=supp_l-mean(supp_l,na.rm=TRUE),
           disc_supp=disc_c*supp_c)

ctr  <- "disc_c + supp_c + disc_supp + lnGDP_l + grth_l + regUN_l + musl + res_l"
grab <- function(tb) tb %>% filter(term %in% c("disc_c","supp_c","disc_supp")) %>%
    transmute(term, est, se, p = 2*(1-pnorm(abs(est/se))))

fit_logit <- function(dv, sub) {
    f <- as.formula(glue("{dv} ~ {ctr} + t1 + t2 + t3"))
    keep <- with(dat, eval(sub)); d <- dat[!is.na(keep) & keep, ]
    d <- d[complete.cases(d[, all.vars(f)]), ]
    m <- glm(f, data=d, family=binomial()); V <- sandwich::vcovCL(m, cluster=d$country)
    grab(tibble(term=names(coef(m)), est=coef(m), se=sqrt(diag(V)))) %>% mutate(n_ev=sum(d[[all.vars(f)[1]]]))
}
fit_cox <- function(state) {
    f <- as.formula(glue("Surv(t0, t1c, fail) ~ {ctr} + cluster(country)"))
    d <- dat %>% filter(spell_state==state, t1c>t0) %>% (\(x) x[complete.cases(x[, c("t0","t1c","fail","country", all.vars(as.formula(glue('~ {ctr}'))))]), ])()
    m <- survival::coxph(f, data=d)
    grab(tibble(term=names(coef(m)), est=coef(m), se=sqrt(diag(vcov(m))))) %>% mutate(n_ev=sum(d$fail))
}

naive <- list(
    `regtrans breakdown [dem]`  = fit_logit("ev_break", quote(v2x_l >= 2)),
    `regtrans transition [aut]` = fit_logit("ev_trans", quote(v2x_l < 2)),
    `cox breakdown [dem]`       = fit_cox(1),
    `cox democratization [aut]` = fit_cox(0)
)
saveRDS(naive, here("results_robust_naive.rds"))
for (nm in names(naive)) { cat("\n==", nm, "==\n"); print(naive[[nm]] %>% mutate(across(c(est,se,p),~round(.x,3)))) }
