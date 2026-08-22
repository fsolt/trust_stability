##############################################################################
# Political Discontent and Regime (In)stability
# Authors: Choi, Ma, Tai, Hu, Solt
# Input:  output/results_ctrlglv_{libdem,polyarchy,hazard}[_naive].rds;
#         output/results_canonicalglv_hazard[_naive].rds
# Output: output/tables_main.docx; output/table{1,2,3}_*.csv
##############################################################################

library(dplyr); library(purrr); library(tibble); library(glue)
library(flextable); library(officer); library(here)

star  <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "+", ""))))
fmt   <- function(b, p, d = 3) ifelse(is.na(b), "", sprintf(glue("%.{d}f%s"), b, star(p)))
fmt_p <- function(p) ifelse(is.na(p), "", sprintf("(%.3f)", p))
mocsum <- function(md) tibble(term = names(md), b = sapply(md, mean), se = sapply(md, sd)) %>%
    mutate(p = 2 * (1 - pnorm(abs(b / se)))) %>% filter(!grepl("rsq", term))

# ---------------------------------------------------------------------------
# Table 1: dynamic panel, full spec (sec. 8)
# All three tables share ONE skeleton (JC 2026-07-29):
#   Model | Variable | Estimator | Naive b | Naive p | MOC b | MOC se | MOC p | Events
# Model carries the design -- DV (all regimes / regime split / ... subsample) --
# exactly as in tables_appendix_all_models; Variable names are bare.
# ---------------------------------------------------------------------------
t1_vars_all <- list(
  c("plm::lag(disc_trim, 1)", "Discontent (t-1)"),
  c("plm::lag(supp_trim, 1)", "Support (t-1)"))
t1_vars_split <- list(
  c("plm::lag(disc_aut_trim, 1)", "Discontent in autocracies (t-1)"),
  c("plm::lag(disc_dem_trim, 1)", "Discontent in democracies (t-1)"),
  c("plm::lag(supp_aut_trim, 1)", "Support in autocracies (t-1)"),
  c("plm::lag(supp_dem_trim, 1)", "Support in democracies (t-1)"))
t1_design <- c(pooled = "all regimes", `pooled-regime` = "regime split",
               gmm = "all regimes", `gmm-regime` = "regime split")
t1_est    <- c(pooled = "Pooled OLS", `pooled-regime` = "Pooled OLS",
               gmm = "System GMM", `gmm-regime` = "System GMM (cautionary)")
t1 <- imap_dfr(list(`Liberal democracy` = "libdem", `Electoral democracy` = "polyarchy"),
  function(dv, dv_lab) {
    moc <- readRDS(here("output", glue("results_ctrlglv_{dv}.rds")))
    nv  <- readRDS(here("output", glue("results_ctrlglv_{dv}_naive.rds")))
    imap_dfr(moc, function(md, est) {
      m <- mocsum(as_tibble(md))
      n <- nv[[est]] %>% mutate(np = 2 * (1 - pnorm(abs(naive_est / naive_se))))
      rows <- if (t1_design[est] == "all regimes") t1_vars_all else t1_vars_split
      map_dfr(rows, function(r) {
        mi <- m %>% filter(term == r[1]); ni <- n %>% filter(term == r[1])
        if (nrow(mi) == 0) return(NULL)
        tibble(Model = glue("{dv_lab} ({t1_design[est]})"),
               Variable = r[2],
               Estimator = t1_est[est],
               `Naive b`  = fmt(ni$naive_est * 1000, ni$np),
               `Naive p`  = fmt_p(ni$np),
               `MOC b`    = fmt(mi$b * 1000, mi$p),
               `MOC se`   = sprintf("%.3f", mi$se * 1000),
               `MOC p`    = fmt_p(mi$p),
               Events     = NA_integer_)
      })
    })
  })
t1_order <- c("Liberal democracy (all regimes)", "Liberal democracy (regime split)",
              "Electoral democracy (all regimes)", "Electoral democracy (regime split)")
t1 <- t1 %>%
  mutate(Model = paste0(LETTERS[match(Model, t1_order)], ". ", Model)) %>%
  arrange(Model)

# ---------------------------------------------------------------------------
# Table 2: ERT episode onset (split IVs) + direction-specific onsets (sec. 6)
# ---------------------------------------------------------------------------
moc_h <- readRDS(here("output", "results_ctrlglv_hazard.rds"))
nv_h  <- readRDS(here("output", "results_ctrlglv_hazard_naive.rds"))
cell <- function(store, cellnm, mth, term) {
  md <- store[[glue("{cellnm} [{mth}]")]]
  if (is.data.frame(md) && "term" %in% names(md)) md %>% filter(term == !!term) else {
    m <- mocsum(as_tibble(md)); m %>% filter(term == !!term) }
}
t2_one <- function(cellnm, term, label, panel) {
  map_dfr(c("glm", "firth"), function(mth) {
    m <- cell(moc_h, cellnm, mth, term)
    n <- nv_h[[glue("{cellnm} [{mth}]")]] %>% filter(term == !!term) %>%
      mutate(np = 2 * (1 - pnorm(abs(naive_est / naive_se))))
    tibble(Model = panel, Variable = label, Estimator = ifelse(mth == "glm", "Logit", "Firth"),
           `Naive b` = fmt(n$naive_est, n$np), `Naive p` = fmt_p(n$np),
           `MOC b` = fmt(m$b, m$p), `MOC se` = sprintf("%.3f", m$se), `MOC p` = fmt_p(m$p),
           Events = n$n_events[1])
  })
}
asym <- function(store, cellnm, mth, t_hi, t_lo, label, panel) {
  md <- store[[glue("{cellnm} [{mth}]")]]
  d <- md[[t_hi]] - md[[t_lo]]
  p <- 2 * (1 - pnorm(abs(mean(d) / sd(d))))
  tibble(Model = panel, Variable = label, Estimator = ifelse(mth == "glm", "Logit", "Firth"),
         `Naive b` = "", `Naive p` = "",
         `MOC b` = fmt(mean(d), p), `MOC se` = sprintf("%.3f", sd(d)), `MOC p` = fmt_p(p),
         Events = NA_integer_)
}
t2_cells <- list(c("onset (split IVs)", "ERT any onset (regime split)"),
                 c("onset for democratization (split IVs)", "ERT democratization-episode onset (regime split)"),
                 c("onset for autocratization (split IVs)", "ERT autocratization-episode onset (regime split)"))
t2 <- purrr::map_dfr(t2_cells, function(cl) bind_rows(
  t2_one(cl[1], "disc_aut_trim_lag1", "Discontent in autocracies (t-1)", cl[2]),
  t2_one(cl[1], "disc_dem_trim_lag1", "Discontent in democracies (t-1)", cl[2]),
  t2_one(cl[1], "supp_aut_trim_lag1", "Support in autocracies (t-1)",    cl[2]),
  t2_one(cl[1], "supp_dem_trim_lag1", "Support in democracies (t-1)",    cl[2]))) %>%
  filter(Estimator == "Firth")

# ---------------------------------------------------------------------------
# Table 3: DD/BMR transition (split IVs), full spec (sec. 7)
# ---------------------------------------------------------------------------
moc_c <- readRDS(here("output", "results_canonicalglv_hazard.rds"))
nv_c  <- readRDS(here("output", "results_canonicalglv_hazard_naive.rds"))
t3_one <- function(cd, trim_stub, label, panel, cell = "split IVs") {
  cellnm <- glue("{cd} transition ({cell})")
  map_dfr(c("glm", "firth"), function(mth) {
    md <- moc_c[[glue("{cellnm} [{mth}]")]]
    m <- mocsum(as_tibble(md)) %>% filter(term == trim_stub)
    n <- nv_c[[glue("{cellnm} [{mth}]")]] %>% filter(term == trim_stub) %>%
      mutate(np = 2 * (1 - pnorm(abs(naive_est / naive_se))))
    tibble(Model = panel, Variable = label, Estimator = ifelse(mth == "glm", "Logit", "Firth"),
           `Naive b` = fmt(n$naive_est, n$np), `Naive p` = fmt_p(n$np),
           `MOC b` = fmt(m$b, m$p), `MOC se` = sprintf("%.3f", m$se), `MOC p` = fmt_p(m$p),
           Events = n$n_events[1])
  })
}
t3 <- bind_rows(
  t3_one("DD",  "disc_dd_aut_trim",  "Discontent in autocracies", "DD regime transition (dem vs auto) (regime split)"),
  t3_one("DD",  "disc_dd_dem_trim",  "Discontent in democracies", "DD regime transition (dem vs auto) (regime split)"),
  t3_one("DD",  "supp_dd_aut_trim",  "Support in autocracies",    "DD regime transition (dem vs auto) (regime split)"),
  t3_one("DD",  "supp_dd_dem_trim",  "Support in democracies",    "DD regime transition (dem vs auto) (regime split)"),
  t3_one("BMR", "disc_bmr_aut_trim", "Discontent in autocracies", "BMR regime transition (dem vs auto) (regime split)"),
  t3_one("BMR", "disc_bmr_dem_trim", "Discontent in democracies", "BMR regime transition (dem vs auto) (regime split)"),
  t3_one("BMR", "supp_bmr_aut_trim", "Support in autocracies",    "BMR regime transition (dem vs auto) (regime split)"),
  t3_one("BMR", "supp_bmr_dem_trim", "Support in democracies",    "BMR regime transition (dem vs auto) (regime split)")) %>%
  filter(Estimator == "Firth")

# ---------------------------------------------------------------------------
# write csvs + docx
# ---------------------------------------------------------------------------
write.csv(t1, here("output", "table1_dynamic.csv"),   row.names = FALSE)
write.csv(t2, here("output", "table2_onset.csv"),     row.names = FALSE)
write.csv(t3, here("output", "table3_canonical.csv"), row.names = FALSE)

ft <- function(d, title, note) {
  flextable(d) %>% autofit() %>% fontsize(size = 8, part = "all") %>%
    add_header_lines(title) %>% add_footer_lines(note) %>%
    theme_booktabs()
}
n1 <- paste("Coefficients x1000. DV = annual V-Dem index (0-100). All models: two DV lags,",
  "opinion IVs and nine controls at t-1 (Claassen 2020 specification + Gassebner et al. 2013",
  "robust determinants + inequality + population). GMM: system GMM, collapsed instruments lag 3-8.",
  "Naive = point-estimate data; MOC = method of composition over 900 posterior draws",
  "(estimate = draw mean, se = draw sd). + p<.10, ** p<.05, *** p<.01.",
  "Regime-split GMM is reported for completeness but rejected by its own",
  "diagnostics in every instrument window probed (Sargan and AR(2); explosive",
  "lagged-DV dynamics) -- cautionary only, not evidence.")
n2 <- paste("DV = first year of a V-Dem ERT episode (either direction); risk set excludes",
  "ongoing-episode years. Opinion of publics in democracies and autocracies enters as separate",
  "zero-filled regressors (classification at t-1) with a regime dummy, one shared duration",
  "baseline, and the full control set (all covariates at t-1). Firth penalized likelihood.",
  "Coefficients are log-odds per point of the 0-100 opinion scale.",
  "Plain-logit, pooled, subsample, and direction-specific variants (nearly identical): appendix.",
  "Naive = point-estimate data; MOC = method of composition over 900 posterior draws",
  "(estimate = draw mean, se = draw sd). + p<.10, ** p<.05, *** p<.01.")
n3 <- paste("DV = regime transition over each coding's own democracy boundary, either direction",
  "(lead-coded: final year of the outgoing regime); full specification with the coding's own",
  "transition-history control and duration clock; covariates current-year (events materialize",
  "at t+1). The in-autocracies slope is the democratization-side effect, the in-democracies",
  "slope the breakdown-side effect. Firth penalized likelihood; log-odds.",
  "Plain-logit and pooled variants, and the exploratory GWF cells (12 events, coverage ends",
  "2010): appendix. + p<.10, ** p<.05, *** p<.01.")

doc <- read_docx() %>%
  body_add_par("Table 2. Public opinion and the onset of V-Dem ERT regime-change episodes (discrete-time hazard, regime-split slopes)", style = "heading 2") %>%
  body_add_flextable(ft(t2, character(0), n2)) %>% body_add_par("") %>%
  body_add_par("Table 3. Public opinion and completed regime transitions, DD and BMR codings (discrete-time hazard, regime-split slopes)", style = "heading 2") %>%
  body_add_flextable(ft(t3, character(0), n3))
print(doc, target = here("output", "tables_main.docx"))
cat("written: output/tables_main.docx (slim: onset + DD/BMR transitions, Firth) + table1_dynamic.csv / table2_onset.csv / table3_canonical.csv\n")
