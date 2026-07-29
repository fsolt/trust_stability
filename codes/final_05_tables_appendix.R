# ============================================================================
# final_05_tables_appendix.R -- appendix docx with EVERY model in the battery:
# all focal opinion coefficients from output/results_main_combined.csv
# (built by sec. 9 of final_01_master.R), one table per block.
#   A1: dynamic panel (sec. 7b)      -- coefficients displayed x1000
#   A2: ERT hazard ladder (sec. 8b)  -- log-odds
#   A3: canonical codings (sec. 8c)  -- log-odds
# Output: output/tables_appendix_all_models.docx (+ csv)
# ============================================================================

library(dplyr); library(flextable); library(officer); library(here)

m <- read.csv(here("output", "results_main_combined.csv"), check.names = FALSE)

star <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "+", ""))))
fmt  <- function(b, p) ifelse(is.na(b), "", sprintf("%.3f%s", b, star(p)))
fmt_p <- function(p) ifelse(is.na(p), "", sprintf("(%.3f)", p))

pretty_term <- function(t) {
  v   <- ifelse(grepl("^disc", t), "Discontent", "Support")
  lag <- grepl("_lag1$", t)
  mid <- sub("_trim$", "", sub("^(disc|supp)_?", "", sub("_lag1$", "", t)))
  scope_map <- c(
    "dem"     = ", in democracies",       "aut"     = ", in autocracies",
    "dd_dem"  = ", in democracies (DD)",  "dd_aut"  = ", in autocracies (DD)",
    "bmr_dem" = ", in democracies (BMR)", "bmr_aut" = ", in autocracies (BMR)",
    "gwf_dem" = ", in democracies (GWF)", "gwf_aut" = ", in non-democracies (GWF)"
  )
  scope <- ifelse(mid == "", "", scope_map[mid])
  paste0(v, ifelse(is.na(scope), "", scope), ifelse(lag, " (t-1)", ""))
}

tab_block <- function(bl, scale = 1) {
  m %>%
    filter(block == bl) %>%
    mutate(model = factor(model, unique(model))) %>%
    arrange(model) %>%
    transmute(
      Model     = as.character(model),
      Variable  = pretty_term(term),
      `Naive b` = fmt(naive_est * scale, naive_p),
      `Naive p` = fmt_p(naive_p),
      `MOC b`   = fmt(moc_est * scale, moc_p),
      `MOC se`  = sprintf("%.3f", moc_se * scale),
      `MOC p`   = fmt_p(moc_p),
      Events    = n_events
    )
}

a1 <- tab_block("7b dynamic (+ctrl)", scale = 1000)
a2 <- tab_block("8b ERT ladder (+ctrl)")
a3 <- tab_block("8c canonical (+ctrl)")

write.csv(bind_rows(
  mutate(a1, Block = "A1 dynamic panel"),
  mutate(a2, Block = "A2 ERT ladder"),
  mutate(a3, Block = "A3 canonical codings")
), here("output", "tables_appendix_all_models.csv"), row.names = FALSE)

ft <- function(d, note) {
  flextable(d) %>% autofit() %>% fontsize(size = 7.5, part = "all") %>%
    merge_v(j = "Model") %>% valign(j = "Model", valign = "top") %>%
    add_footer_lines(note) %>% theme_booktabs()
}
common <- paste("Naive = point-estimate data; MOC = method of composition over 900",
  "posterior draws (estimate = draw mean, se = draw sd). + p<.10, ** p<.05, *** p<.01.")
nA1 <- paste("Coefficients x1000. DV = annual V-Dem index (0-100); two DV lags, opinion IVs and",
  "nine controls at t-1. [pooled] = pooled OLS; [gmm] = one-step system GMM, collapsed",
  "instruments lag 3-5; -regime = opinion slopes split by RoW democracy (zero-filled).",
  "The gmm-regime cells fail their own Sargan/AR(2) diagnostics in every instrument",
  "window probed: cautionary only, not evidence.", common)
nA2 <- paste("DV per row block: ERT episode onset, RoW category change, or RoW democracy-boundary",
  "regime transition; discrete-time hazard with duration clock, transition history, and nine",
  "controls (whole RHS at t-1 for onset cells; current-year covariates for lead-coded events).",
  "(all)/(democracies)/(autocracies) = estimation sample; (split IVs) = pooled sample with",
  "regime-split opinion slopes. [glm] = logit, country-clustered ses; [firth] = Firth",
  "penalized logit.", common)
nA3 <- paste("DV per row block: each coding's any-regime-change, direction-specific transition,",
  "or democracy-boundary crossing (lead-coded, final year of outgoing regime); full control",
  "specification with the coding's own clock and transition history. GWF coverage ends 2010:",
  "exploratory.", common)

doc <- read_docx() %>%
  body_add_par("Appendix Table A1. Dynamic panel: all estimated models (sec. 7b)", style = "heading 2") %>%
  body_add_flextable(ft(a1, nA1)) %>% body_add_par("") %>%
  body_add_par("Appendix Table A2. V-Dem ERT hazard ladder: all estimated models (sec. 8b)", style = "heading 2") %>%
  body_add_flextable(ft(a2, nA2)) %>% body_add_par("") %>%
  body_add_par("Appendix Table A3. Canonical regime codings: all estimated models (sec. 8c)", style = "heading 2") %>%
  body_add_flextable(ft(a3, nA3))
print(doc, target = here("output", "tables_appendix_all_models.docx"))
cat("written: output/tables_appendix_all_models.docx + .csv;",
    sprintf("rows A1=%d A2=%d A3=%d\n", nrow(a1), nrow(a2), nrow(a3)))
