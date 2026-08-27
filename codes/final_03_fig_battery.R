##############################################################################
# Political Discontent and Regime (In)stability
# Authors: Choi, Ma, Tai, Hu, Solt
# Input:  output/results_main_combined.csv
# Output: output/fig_battery.{png,pdf} (Fig 1: main onset model, all coefs);
#         output/fig_direction_outcome.{png,pdf} (Fig 2: direction+outcome, disc);
#         output/fig_battery_appendix.{png,pdf} (full battery)
##############################################################################
library(dplyr); library(ggplot2); library(here)

m <- read.csv(here("output", "results_main_combined.csv"), check.names = FALSE)

# naive se recovered by inverting the normal p-value (p was computed as
# 2*(1-pnorm(|b/se|)); guard the rounded-to-zero case)
m <- m %>% mutate(
  z_naive  = qnorm(1 - pmin(pmax(naive_p, 1e-4), .9999) / 2),
  naive_se = abs(naive_est) / pmax(z_naive, 1e-6)
)

# match on ASCII-normalized names so the direction arrows in the csv model
# names never break lookups under a non-UTF-8 locale
norm <- function(x) gsub(" +", " ", gsub("[^ -~]", "", x))
pick <- function(model_str, term_str, label, track, side, scale = 1) {
  r <- m %>% filter(norm(model) == norm(model_str), term == term_str)
  stopifnot(nrow(r) == 1)
  tibble(
    label = label, track = track, side = side,
    naive_b = r$naive_est * scale, naive_se = r$naive_se * scale,
    moc_b = r$moc_est * scale, moc_se = r$moc_se * scale
  )
}

to_long <- function(d, track_levels) {
  bind_rows(
    d %>% transmute(label, track, side, est = "Naive", b = naive_b, se = naive_se),
    d %>% transmute(label, track, side, est = "MOC",   b = moc_b,   se = moc_se)
  ) %>%
    mutate(
      lo = b - 1.96 * se, hi = b + 1.96 * se,
      side  = factor(side, c("disc", "supp"), c("Political discontent", "Democratic support")),
      track = factor(track, track_levels),
      est = factor(est, c("Naive", "MOC")),
      label = factor(label, rev(unique(d$label)))
    )
}

theme_batt <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = .25, color = "grey88"),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 8.5),
    strip.text.x = element_text(face = "bold", size = 10),
    legend.position = "top",
    plot.caption = element_text(size = 7, color = "grey35"),
    axis.text.y = element_text(size = 8.5)
  )

geoms_batt <- list(
  geom_vline(xintercept = 0, linewidth = .35, color = "grey55"),
  geom_linerange(aes(xmin = lo, xmax = hi),
                 position = position_dodge(width = .55), linewidth = .55),
  geom_point(position = position_dodge(width = .55), size = 1.9, stroke = .8,
             fill = "white"),
  scale_color_manual(values = c(Naive = "grey60", MOC = "#20558A")),
  scale_shape_manual(values = c(Naive = 21, MOC = 16))
)

# ============================================================================
# MAIN FIGURE -- the full `onset (split IVs)` model, every coefficient
# (ert_any_onset, Firth; opinion slopes split by regime + all controls)
# ============================================================================
CELL <- "onset (split IVs) [firth]"
moc_h <- readRDS(here("output", "results_ctrlglv_hazard.rds"))[[CELL]]
nv_h  <- readRDS(here("output", "results_ctrlglv_hazard_naive.rds"))[[CELL]]

TERMS <- tribble(
  ~term,                  ~label,                              ~group,
  "disc_aut_trim_lag1",   "Discontent in autocracies",         "Public opinion (t-1)",
  "disc_dem_trim_lag1",   "Discontent in democracies",         "Public opinion (t-1)",
  "supp_aut_trim_lag1",   "Support in autocracies",            "Public opinion (t-1)",
  "supp_dem_trim_lag1",   "Support in democracies",            "Public opinion (t-1)",
  "lnGDP_imp_lag1",       "log GDP per capita",                "Controls",
  "GDP_imp_grth_lag1",    "GDP growth",                        "Controls",
  "Libdem_regUN_lag1",    "Regional democracy",                "Controls",
  "Pr_Muslim_lag1",       "Muslim population share",           "Controls",
  "Res_cp_WDI_di_lag1",   "Resource dependence",               "Controls",
  "oecd_lag1",            "OECD member",                       "Controls",
  "gini_lag1",            "Income inequality",                 "Controls",
  "lnpop_lag1",           "log population",                    "Controls",
  "reg_regime_age",       "Regime age",                        "Controls",
  "I(reg_regime_age^2)",  "Regime age squared",                "Controls",
  "pre_reg_change_lag1",  "Prior regime changes",              "Controls"
)

moc_tab <- tibble(term = names(moc_h),
                  b = sapply(moc_h, mean), se = sapply(moc_h, sd)) %>%
  mutate(est = "MOC")
nv_tab <- nv_h %>% transmute(term, b = naive_est, se = naive_se, est = "Naive")

d_main <- bind_rows(nv_tab, moc_tab) %>%
  inner_join(TERMS, by = "term") %>%
  mutate(
    lo = b - 1.96 * se, hi = b + 1.96 * se,
    group = factor(group, c("Public opinion (t-1)", "Controls")),
    est   = factor(est, c("Naive", "MOC")),
    label = factor(label, rev(TERMS$label))
  )

p_main <- ggplot(d_main, aes(x = b, y = label, color = est, shape = est)) +
  geoms_batt +
  labs(x = "Log-odds coefficient (95% CI)", y = NULL, color = NULL, shape = NULL) +
  theme_batt

ggsave(here("output", "fig_battery.png"), p_main, width = 7, height = 4.8, dpi = 300)
ggsave(here("output", "fig_battery.pdf"), p_main, width = 7, height = 4.8)

# ============================================================================
# FIGURE 2 -- direction- and outcome-specific onsets, DISCONTENT slopes only
# (direction cells: regime-split slopes; outcome cells: single slope in the
#  at-risk regime subsample, so the regime is named in the row label)
# ============================================================================
TA <- "Autocracies"
TDm <- "Democracies"

d_dir <- bind_rows(
  pick("onset for democratization (regime split) [firth]", "disc_aut_trim_lag1", "Onset of democratization (upward) episodes",   TA,  "disc"),
  pick("onset for autocratization (regime split) [firth]", "disc_aut_trim_lag1", "Onset of autocratization (downward) episodes", TA,  "disc"),
  pick("onset for demo trans [firth]",   "disc_trim_lag1", "Episodes ending in democratic transition", TA, "disc"),
  pick("onset for auto deepen [firth]",  "disc_trim_lag1", "Episodes of autocratic deepening",         TA, "disc"),
  pick("onset for demo notrans [firth]", "disc_trim_lag1", "Episodes stalling short of transition",    TA, "disc"),
  pick("onset for democratization (regime split) [firth]", "disc_dem_trim_lag1", "Onset of democratization (upward) episodes",   TDm, "disc"),
  pick("onset for autocratization (regime split) [firth]", "disc_dem_trim_lag1", "Onset of autocratization (downward) episodes", TDm, "disc"),
  pick("onset for auto trans [firth]",   "disc_trim_lag1", "Episodes ending in democratic breakdown",  TDm, "disc"),
  pick("onset for demo deepen [firth]",  "disc_trim_lag1", "Episodes of democratic deepening",         TDm, "disc"),
  pick("onset for auto notrans [firth]", "disc_trim_lag1", "Episodes stalling short of breakdown",     TDm, "disc")
)

d_dir_long <- bind_rows(
  d_dir %>% transmute(label, track, est = "Naive", b = naive_b, se = naive_se),
  d_dir %>% transmute(label, track, est = "MOC",   b = moc_b,   se = moc_se)
) %>%
  mutate(
    lo = b - 1.96 * se, hi = b + 1.96 * se,
    track = factor(track, c(TA, TDm)),
    est = factor(est, c("Naive", "MOC")),
    label = factor(label, rev(unique(d_dir$label)))
  )

p_dir <- ggplot(d_dir_long, aes(x = b, y = label, color = est, shape = est)) +
  geoms_batt +
  facet_grid(track ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Log-odds coefficient of political discontent (95% CI)",
       y = NULL, color = NULL, shape = NULL) +
  theme_batt

ggsave(here("output", "fig_direction_outcome.png"), p_dir, width = 6.8, height = 4.2, dpi = 300)
ggsave(here("output", "fig_direction_outcome.pdf"), p_dir, width = 6.8, height = 4.2)

# ============================================================================
# FIGURE 3 -- completed transitions under DD / BMR / GWF, DISCONTENT only
# (split cells: regime-split slopes; direction cells: single slope in the
#  at-risk regime subsample)
# ============================================================================
d_can <- bind_rows(
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "disc_dd_aut_trim",  "Transition: in autocracies",        "DD",  "disc"),
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "disc_dd_dem_trim",  "Transition: in democracies",        "DD",  "disc"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "disc_bmr_aut_trim", "Transition: in autocracies",        "BMR", "disc"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "disc_bmr_dem_trim", "Transition: in democracies",        "BMR", "disc"),
  pick("GWF regime transition (dem vs auto) (regime split) [firth]", "disc_gwf_aut_trim", "Transition: in autocracies",        "GWF", "disc"),
  pick("GWF regime transition (dem vs auto) (regime split) [firth]", "disc_gwf_dem_trim", "Transition: in democracies",        "GWF", "disc")
)

d_can_long <- bind_rows(
  d_can %>% transmute(label, track, est = "Naive", b = naive_b, se = naive_se),
  d_can %>% transmute(label, track, est = "MOC",   b = moc_b,   se = moc_se)
) %>%
  mutate(
    lo = b - 1.96 * se, hi = b + 1.96 * se,
    track = factor(track, c("DD", "BMR", "GWF")),
    est = factor(est, c("Naive", "MOC")),
    label = factor(label, rev(unique(d_can$label)))
  )

p_can <- ggplot(d_can_long, aes(x = b, y = label, color = est, shape = est)) +
  geoms_batt +
  facet_grid(track ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Log-odds coefficient of political discontent (95% CI)",
       y = NULL, color = NULL, shape = NULL) +
  theme_batt

ggsave(here("output", "fig_codings.png"), p_can, width = 6.2, height = 3.0, dpi = 300)
ggsave(here("output", "fig_codings.pdf"), p_can, width = 6.2, height = 3.0)

# ============================================================================
# APPENDIX FIGURE -- V-Dem democracy levels + completed DD/BMR transitions
# ============================================================================
TRL <- "V-Dem democracy levels"
TR  <- "ERT episode onset (log-odds)"
TRC <- "Completed transitions:\nDD / BMR (log-odds)"

d_app <- bind_rows(
  pick("Liberal Democracy (all regimes) [pooled OLS]",     "disc_trim",     "Liberal dem.: all regimes",      TRL, "disc", 1000),
  pick("Liberal Democracy (regime split) [pooled OLS]",    "disc_aut_trim", "Liberal dem.: in autocracies",   TRL, "disc", 1000),
  pick("Liberal Democracy (regime split) [pooled OLS]",    "disc_dem_trim", "Liberal dem.: in democracies",   TRL, "disc", 1000),
  pick("Electoral Democracy (all regimes) [pooled OLS]",   "disc_trim",     "Electoral dem.: all regimes",    TRL, "disc", 1000),
  pick("Electoral Democracy (regime split) [pooled OLS]",  "disc_aut_trim", "Electoral dem.: in autocracies", TRL, "disc", 1000),
  pick("Electoral Democracy (regime split) [pooled OLS]",  "disc_dem_trim", "Electoral dem.: in democracies", TRL, "disc", 1000),
  pick("Liberal Democracy (all regimes) [pooled OLS]",     "supp_trim",     "Liberal dem.: all regimes",      TRL, "supp", 1000),
  pick("Liberal Democracy (regime split) [pooled OLS]",    "supp_aut_trim", "Liberal dem.: in autocracies",   TRL, "supp", 1000),
  pick("Liberal Democracy (regime split) [pooled OLS]",    "supp_dem_trim", "Liberal dem.: in democracies",   TRL, "supp", 1000),
  pick("Electoral Democracy (all regimes) [pooled OLS]",   "supp_trim",     "Electoral dem.: all regimes",    TRL, "supp", 1000),
  pick("Electoral Democracy (regime split) [pooled OLS]",  "supp_aut_trim", "Electoral dem.: in autocracies", TRL, "supp", 1000),
  pick("Electoral Democracy (regime split) [pooled OLS]",  "supp_dem_trim", "Electoral dem.: in democracies", TRL, "supp", 1000),

  pick("any onset (regime split) [firth]", "disc_aut_trim_lag1", "Any onset: in autocracies",          TR, "disc"),
  pick("any onset (regime split) [firth]", "disc_dem_trim_lag1", "Any onset: in democracies",          TR, "disc"),
  pick("onset for democratization (regime split) [firth]", "disc_aut_trim_lag1", "Dem. episode onset: in autocracies", TR, "disc"),
  pick("onset for democratization (regime split) [firth]", "disc_dem_trim_lag1", "Dem. episode onset: in democracies", TR, "disc"),
  pick("onset for autocratization (regime split) [firth]", "disc_aut_trim_lag1", "Aut. episode onset: in autocracies", TR, "disc"),
  pick("onset for autocratization (regime split) [firth]", "disc_dem_trim_lag1", "Aut. episode onset: in democracies", TR, "disc"),
  pick("any onset (regime split) [firth]", "supp_aut_trim_lag1", "Any onset: in autocracies",          TR, "supp"),
  pick("any onset (regime split) [firth]", "supp_dem_trim_lag1", "Any onset: in democracies",          TR, "supp"),
  pick("onset for democratization (regime split) [firth]", "supp_aut_trim_lag1", "Dem. episode onset: in autocracies", TR, "supp"),
  pick("onset for democratization (regime split) [firth]", "supp_dem_trim_lag1", "Dem. episode onset: in democracies", TR, "supp"),
  pick("onset for autocratization (regime split) [firth]", "supp_aut_trim_lag1", "Aut. episode onset: in autocracies", TR, "supp"),
  pick("onset for autocratization (regime split) [firth]", "supp_dem_trim_lag1", "Aut. episode onset: in democracies", TR, "supp"),

  pick("DD regime transition (dem vs auto) (pool) [firth]",          "disc_trim",         "DD: all regimes",     TRC, "disc"),
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "disc_dd_aut_trim",  "DD: in autocracies",  TRC, "disc"),
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "disc_dd_dem_trim",  "DD: in democracies",  TRC, "disc"),
  pick("BMR regime transition (dem vs auto) (pool) [firth]",         "disc_trim",         "BMR: all regimes",    TRC, "disc"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "disc_bmr_aut_trim", "BMR: in autocracies", TRC, "disc"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "disc_bmr_dem_trim", "BMR: in democracies", TRC, "disc"),
  pick("DD regime transition (dem vs auto) (pool) [firth]",          "supp_trim",         "DD: all regimes",     TRC, "supp"),
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "supp_dd_aut_trim",  "DD: in autocracies",  TRC, "supp"),
  pick("DD regime transition (dem vs auto) (regime split) [firth]",  "supp_dd_dem_trim",  "DD: in democracies",  TRC, "supp"),
  pick("BMR regime transition (dem vs auto) (pool) [firth]",         "supp_trim",         "BMR: all regimes",    TRC, "supp"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "supp_bmr_aut_trim", "BMR: in autocracies", TRC, "supp"),
  pick("BMR regime transition (dem vs auto) (regime split) [firth]", "supp_bmr_dem_trim", "BMR: in democracies", TRC, "supp")
)

p_app <- ggplot(to_long(d_app, c(TRL, TR, TRC)), aes(x = b, y = label, color = est, shape = est)) +
  geoms_batt +
  facet_grid(track ~ side, scales = "free", space = "free_y", switch = "y") +
  labs(
    x = "Coefficient (95% CI)", y = NULL, color = NULL, shape = NULL,
    caption = paste("Full control specification throughout; 'in autocracies/democracies' rows split the opinion slopes by regime type (zero-filled",
                    "regressors).\nFor regime-transition rows the in-autocracies slope is the democratization side, the in-democracies slope the",
                    "breakdown side.\nDemocracy-level rows: pooled OLS, coefficients x1000. Transition rows: Firth penalized logit.",
                    "Subsample, direction-specific, plain-logit, and exploratory GWF variants: appendix tables.")
  ) +
  theme_batt

ggsave(here("output", "fig_battery_appendix.png"), p_app, width = 9, height = 6.6, dpi = 300)
ggsave(here("output", "fig_battery_appendix.pdf"), p_app, width = 9, height = 6.6)
cat("written: output/fig_battery.{png,pdf} (main, onset only) + fig_battery_appendix.{png,pdf}\n")
