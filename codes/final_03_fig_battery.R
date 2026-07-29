# ============================================================================
# final_03_fig_battery.R -- "the battery at a glance": coefficient (forest) plot of
# the curated focal cells, naive vs MOC, discontent vs support panels.
# Reads output/results_main_combined.csv (sec. 9); no re-estimation.
# Output: output/fig_battery.png / .pdf
# ============================================================================
library(dplyr); library(ggplot2); library(here)

m <- read.csv(here("output", "results_main_combined.csv"), check.names = FALSE)

# naive se recovered by inverting the normal p-value (p was computed as
# 2*(1-pnorm(|b/se|)); guard the rounded-to-zero case)
m <- m %>% mutate(
  z_naive  = qnorm(1 - pmin(pmax(naive_p, 1e-4), .9999) / 2),
  naive_se = abs(naive_est) / pmax(z_naive, 1e-6)
)

pick <- function(model_str, term_str, label, track, side, scale = 1) {
  r <- m %>% filter(model == model_str, term == term_str)
  stopifnot(nrow(r) == 1)
  tibble(
    label = label, track = track, side = side,
    naive_b = r$naive_est * scale, naive_se = r$naive_se * scale,
    moc_b = r$moc_est * scale, moc_se = r$moc_se * scale
  )
}

d <- bind_rows(
  # ---- track 1: democracy levels (pooled OLS, regime-split slopes; x1000) ---
  # (system GMM regime split fails its own diagnostics -> appendix table only)
  pick("Liberal Democracy [pooled]",          "disc_trim",     "Liberal dem.: all regimes",      "V-Dem democracy levels", "disc", 1000),
  pick("Liberal Democracy [pooled-regime]",   "disc_aut_trim", "Liberal dem.: in autocracies",   "V-Dem democracy levels", "disc", 1000),
  pick("Liberal Democracy [pooled-regime]",   "disc_dem_trim", "Liberal dem.: in democracies",   "V-Dem democracy levels", "disc", 1000),
  pick("Electoral Democracy [pooled]",        "disc_trim",     "Electoral dem.: all regimes",    "V-Dem democracy levels", "disc", 1000),
  pick("Electoral Democracy [pooled-regime]", "disc_aut_trim", "Electoral dem.: in autocracies", "V-Dem democracy levels", "disc", 1000),
  pick("Electoral Democracy [pooled-regime]", "disc_dem_trim", "Electoral dem.: in democracies", "V-Dem democracy levels", "disc", 1000),
  pick("Liberal Democracy [pooled]",          "supp_trim",     "Liberal dem.: all regimes",      "V-Dem democracy levels", "supp", 1000),
  pick("Liberal Democracy [pooled-regime]",   "supp_aut_trim", "Liberal dem.: in autocracies",   "V-Dem democracy levels", "supp", 1000),
  pick("Liberal Democracy [pooled-regime]",   "supp_dem_trim", "Liberal dem.: in democracies",   "V-Dem democracy levels", "supp", 1000),
  pick("Electoral Democracy [pooled]",        "supp_trim",     "Electoral dem.: all regimes",    "V-Dem democracy levels", "supp", 1000),
  pick("Electoral Democracy [pooled-regime]", "supp_aut_trim", "Electoral dem.: in autocracies", "V-Dem democracy levels", "supp", 1000),
  pick("Electoral Democracy [pooled-regime]", "supp_dem_trim", "Electoral dem.: in democracies", "V-Dem democracy levels", "supp", 1000),

  # ---- track 2: episode onset (log-odds; Firth) -----------------------------
  pick("onset (all) [firth]",       "disc_trim_lag1",     "ERT onset: all regimes",    "ERT episode onset (log-odds)", "disc"),
  pick("onset (split IVs) [firth]", "disc_aut_trim_lag1", "ERT onset: in autocracies", "ERT episode onset (log-odds)", "disc"),
  pick("onset (split IVs) [firth]", "disc_dem_trim_lag1", "ERT onset: in democracies", "ERT episode onset (log-odds)", "disc"),
  pick("liberalization onset (autocracies) [firth]", "disc_trim_lag1", "ERT liberalization onset (aut.)", "ERT episode onset (log-odds)", "disc"),
  pick("erosion onset (democracies) [firth]",        "disc_trim_lag1", "ERT erosion onset (dem.)",        "ERT episode onset (log-odds)", "disc"),
  pick("onset (all) [firth]",       "supp_trim_lag1",     "ERT onset: all regimes",    "ERT episode onset (log-odds)", "supp"),
  pick("onset (split IVs) [firth]", "supp_aut_trim_lag1", "ERT onset: in autocracies", "ERT episode onset (log-odds)", "supp"),
  pick("onset (split IVs) [firth]", "supp_dem_trim_lag1", "ERT onset: in democracies", "ERT episode onset (log-odds)", "supp"),
  pick("liberalization onset (autocracies) [firth]", "supp_trim_lag1", "ERT liberalization onset (aut.)", "ERT episode onset (log-odds)", "supp"),
  pick("erosion onset (democracies) [firth]",        "supp_trim_lag1", "ERT erosion onset (dem.)",        "ERT episode onset (log-odds)", "supp"),

  # ---- track 3: completed transitions (log-odds; Firth) ---------------------
  pick("DD crossing (all) +ctrl [firth]",        "disc_trim",           "DD: all regimes",           "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("DD crossing (split IVs) +ctrl [firth]",  "disc_dd_aut_trim",  "DD: in autocracies",  "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("DD crossing (split IVs) +ctrl [firth]",  "disc_dd_dem_trim",  "DD: in democracies",        "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("BMR crossing (all) +ctrl [firth]",       "disc_trim",           "BMR: all regimes",          "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("BMR crossing (split IVs) +ctrl [firth]", "disc_bmr_aut_trim", "BMR: in autocracies", "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("BMR crossing (split IVs) +ctrl [firth]", "disc_bmr_dem_trim", "BMR: in democracies",       "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("GWF crossing (all) +ctrl [firth]",       "disc_trim",           "GWF: all regimes*",         "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("GWF crossing (split IVs) +ctrl [firth]", "disc_gwf_aut_trim", "GWF: in non-democracies*","Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("GWF crossing (split IVs) +ctrl [firth]", "disc_gwf_dem_trim", "GWF: in democracies*",      "Completed transitions:\nDD / BMR / GWF (log-odds)", "disc"),
  pick("DD crossing (all) +ctrl [firth]",        "supp_trim",           "DD: all regimes",           "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("DD crossing (split IVs) +ctrl [firth]",  "supp_dd_aut_trim",  "DD: in autocracies",  "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("DD crossing (split IVs) +ctrl [firth]",  "supp_dd_dem_trim",  "DD: in democracies",        "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("BMR crossing (all) +ctrl [firth]",       "supp_trim",           "BMR: all regimes",          "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("BMR crossing (split IVs) +ctrl [firth]", "supp_bmr_aut_trim", "BMR: in autocracies", "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("BMR crossing (split IVs) +ctrl [firth]", "supp_bmr_dem_trim", "BMR: in democracies",       "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("GWF crossing (all) +ctrl [firth]",       "supp_trim",           "GWF: all regimes*",         "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("GWF crossing (split IVs) +ctrl [firth]", "supp_gwf_aut_trim", "GWF: in non-democracies*","Completed transitions:\nDD / BMR / GWF (log-odds)", "supp"),
  pick("GWF crossing (split IVs) +ctrl [firth]", "supp_gwf_dem_trim", "GWF: in democracies*",      "Completed transitions:\nDD / BMR / GWF (log-odds)", "supp")
)

d_long <- bind_rows(
  d %>% transmute(label, track, side, est = "Naive", b = naive_b, se = naive_se),
  d %>% transmute(label, track, side, est = "MOC",   b = moc_b,   se = moc_se)
) %>%
  mutate(
    lo = b - 1.96 * se, hi = b + 1.96 * se,
    side  = factor(side, c("disc", "supp"), c("Political discontent", "Democratic support")),
    track = factor(track, c("V-Dem democracy levels", "ERT episode onset (log-odds)",
                            "Completed transitions:\nDD / BMR / GWF (log-odds)")),
    est = factor(est, c("Naive", "MOC")),
    label = factor(label, rev(unique(d$label)))
  )

p <- ggplot(d_long, aes(x = b, y = label, color = est, shape = est)) +
  geom_vline(xintercept = 0, linewidth = .35, color = "grey55") +
  geom_linerange(aes(xmin = lo, xmax = hi),
                 position = position_dodge(width = .55), linewidth = .55) +
  geom_point(position = position_dodge(width = .55), size = 1.9, stroke = .8,
             fill = "white") +
  facet_grid(track ~ side, scales = "free", space = "free_y", switch = "y") +
  scale_color_manual(values = c(Naive = "grey60", MOC = "#20558A")) +
  scale_shape_manual(values = c(Naive = 21, MOC = 16)) +
  labs(
    x = "Coefficient (95% CI)", y = NULL, color = NULL, shape = NULL,
    title = "Public opinion and regime (in)stability: the full battery",
    subtitle = "Naive (posterior-mean data) vs. method of composition (900 draws); 95% CIs",
    caption = paste("Full control specification throughout; 'in autocracies/democracies' rows split the opinion slopes by regime type (zero-filled",
                    "regressors).\nFor boundary-crossing rows the in-autocracies slope is the democratization side, the in-democracies slope the",
                    "breakdown side.\nDemocracy-level rows: pooled OLS, coefficients x1000. Hazard rows: Firth penalized logit.",
                    "* GWF ends in 2010 (5-16 events): exploratory.")
  ) +
  theme_minimal(base_size = 10) +
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

ggsave(here("output", "fig_battery.png"), p, width = 9, height = 6.6, dpi = 300)
ggsave(here("output", "fig_battery.pdf"), p, width = 9, height = 6.6)
cat("written: output/fig_battery.png / .pdf\n")
