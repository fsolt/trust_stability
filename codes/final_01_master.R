# ############################################################################
# Political Discontent and Regime (In)stability -- MASTER compendium
# ----------------------------------------------------------------------------
# Authors: Ma, Choi, Tai, Hu, Solt   (analysis file maintained by Jeongho Choi)
# TABLE OF CONTENTS
#   0.  Setup: packages, cores policy (ncores = 4), MOC helpers, N_DRAWS = 900
#   1.  Data sources & variable inventory (documentation, this header)
#   2.  Load opinion IVs: discontent + support posterior draws, first-survey trims
#   3.  DVs: V-Dem levels; ERT episodes / RoW boundaries; DD; GWF; BMR
#   4.  Controls: Claassen/AJPS list, OECD accession, SWIID gini, WB population
#   5.  Combine: build_one() x 900 draws; naive (posterior-mean) dataset
#   7.  Dynamic panel, base              (Tai et al. form; pooled + system GMM)
#   7b. Dynamic panel + 9 controls       (full published specification)
#   7c. Dynamic panel interaction        [COMMENTED OUT -- archived generation]
#   8.  Hazard ladder, base              (14 cells: onsets incl. direction-
#                                         specific, RoW change, transitions,
#                                         regime-split IVs)
#   8b. Hazard ladder + 9 controls       (onset cells: fully lagged RHS)
#   8c. Canonical battery                (DD/GWF/BMR breakdown, democratization,
#                                         any-change, crossing split IVs; 22 cells)
#   8d. Hazard interactions              [COMMENTED OUT -- archived generation]
#   9.  Combine results -> output/results_main_combined.{rds,csv} + console table
# ############################################################################

# ============================================================================
# 0.  SETUP
# ============================================================================
rm(list = ls())
library(pacman)
p_load(
  tidyverse, here, rio, countrycode, glue, parallel, vdemdata,
  MASS, sandwich, lmtest, plm, ERT, logistf, survival, democracyData
)

# final_00_functions.R: method-of-composition helpers (methodComposition1/2,
# MOCsumm, vcovBK, plmHC, pNorm, mocParGLM) used throughout.
source(here("codes", "final_00_functions.R"))
N_DRAWS <- 900L
ncores <- 4L # required by mocParGLM; cap at 4 (Apple M5 heat policy)

# ============================================================================
# 1.  DATA SOURCES & VARIABLE INVENTORY
# ============================================================================
# (a) DISCONTENT IV -- DCPO latent public political discontent (PPD), 0-100.
#     data/discontent_theta_results.rda; 900 draws
#     First-survey trim years from data/discontent_dcpo_input_raw.csv.
#
# (b) SUPPORT IV -- DCPO democratic-support mood, 0-100.
#     data/supdem_exp_dcpo_theta.rds; 900 draws
#
# (c) DVs. Shared conventions: every event is coded in the OUTGOING regime's FINAL year
#     Lead-coded where the source stamps the new regime's first year, so event-year covariates precede the transition;
#     Each coding has its own clock (*_regime_age) and history count
#
#     - V-Dem LEVELS (sec. 7 track): v2x_libdem / v2x_polyarchy -- annual
#       0-100 democracy-quality indices (continuous DV, dynamic panel).
#
#     - ERT / RoW (V-Dem Episodes of Regime Transformation):
#       ONSET:
#           ert_any_onset       = 1 in the FIRST year of an ERT episode of either
#                                 direction (sustained, substantial EDI movement)
#           ert_dem_onset       = liberalization-episode onset
#           ert_aut_onset       = erosion/autocratization-episode onset
#
#       CHANGE between ROW 4 categories:
#           ert_any_change      = RoW 4-category class changes next year; _down =
#                                 to a lower category (autocratization), _up =
#                                 higher (democratization)
#
#       Cross-Regime (Dem or Auto) change:
#           ert_dem_break       = last democracy year before crossing the dem/aut
#                                 boundary downward (RoW >=2 -> <=1)
#           ert_democratization = last autocracy year before crossing upward
#           ert_any_regime_trans= either boundary crossing
#
#     - DD / ACLP (democracyData::pacl_update, Bjornskov-Rode update to ~2025):
#       dd_democracy        = contestation + alternation dichotomy (1 = dem)
#       dd_any_change       = any change in the 6-category DD_regime next year
#                             (parl/mixed/pres democracy; civ/mil/royal dict.)
#       dd_demo_break       = democracy case ends into non-democracy
#       dd_democratization  = non-democracy case ends into democracy
#       dd_cross            = dd_demo_break OR dd_democratization
#
#     - GWF (democracyData::gwf_all; ends 2010 -> supplementary/exploratory):
#       gwf_democracy       = 1 if gwf_nonautocracy == "democracy" (0 =
#                             autocratic cases AND provisional/warlord/occupied)
#       gwf_any_change      = the GWF regime CASE ends this year (any ending,
#                             incl. same-type successions)
#       gwf_demo_break      = democracy case ends into non-democracy
#       gwf_democratization = non-democracy case ends into democracy
#       gwf_cross           = either
#
#     - BMR (democracyData::bmr, Boix-Miller-Rosato dichotomy 1800-2020):
#       bmr_democracy       = BMR dichotomy (1 = democracy)
#       bmr_demo_break      = democracy case ends into non-democracy
#       bmr_democratization = non-democracy case ends into democracy
#       bmr_cross           = either demo_break or democratization
#
# (d) CONTROLS -- nine covariates in every full-spec model, plus the duration clock.
#     NOTE coverage: the Tai et al. control list runs 1987-2019, so every +ctrl cell is bounded there
#                    Base cells run to ~2022/23 on the survey trim alone.
#     Timing: dynamic panel all at t-1; hazard boundary cells current-year (events materialize at t+1); hazard onset cells at t-1 (_lag1).
#
#     1) lnGDP_imp      log GDP per capita. Drawn from Tai et al.'s replication data
#                       (supdem_exp_ajps_cntrl_list.rds)
#     2) GDP_imp_grth   annual GDP p.c. growth, %; same source (Tai et al.)
#     3) Libdem_regUN   mean V-Dem liberal-democracy level in the country's
#                       UN region; same source (Tai et al.)
#     4) Pr_Muslim      percent Muslim population; same source (Tai et al.)
#     5) Res_cp_WDI_di  natural-resource dependence dummy (WDI-based,
#                       dichotomized); same source (Tai et al.)
#     6) *_any_pre_change  count past transitions before the current
#                       regime spell for each DV's coding of spells
#     7) oecd           OECD membership dummy, 1 from the accession year
#     8) gini           net (disposable) income inequality, SWIID 9.8
#                       gini_disp (auto-downloaded, data-raw/swiid9_8/).
#     9) lnpop          log total population, WB SP.POP.TOTL
#                       (auto-downloaded, data-raw/wb_population/).
#     10) *_regime_age (and its square): the discrete-time hazard's duration
#       baseline -- age of the current regime spell under the relevant coding
#
# (e) MODELS RUN (each model twice: NAIVE on posterior-mean data + MOC over
#     the 900 paired draws
#     sec. 7  Dynamic panel, base: DV_t ~ DV_{t-1,t-2} + disc_{t-1} + supp_{t-1};
#             pooled OLS (Beck-Katz PCSE) + system GMM (collapsed lag 3:5,
#             one-step 'ld'); + regime-split zero-fill variant (pooled & GMM).
#     sec. 7b Base + 9 lag-1 controls (full published spec)
#     sec. 7c [commented out] dynamic-panel interaction (+ lag-1 double-
#             centered product disc_supp) -- archived; directional, MOC-null.
#     sec. 8  Discrete-time hazard logits -- 14 base cells: two groups + 2
#             direction-onset cells (liberalization onset in autocracies,
#             erosion onset in democracies):
#             (i) severity ladder, 9 cells = 3 DVs x 3 samples:
#                 DVs {episode onset, RoW 4-category change, dem/aut boundary
#                 transition} x samples {all, democracies-only, autocracies-
#                 only};
#             (ii) split-IV cells, 3 cells: same 3 DVs on the FULL sample,
#                 but discontent/support each enter as TWO regressors
#                 (democratic-public slope + autocratic-public slope,
#                 zero-filled) + regime dummy -- i.e., the (dem)/(aut)
#                 subsample pair merged into one model that shares the
#                 duration clock (and in 8b the control block).
#
#             TIMING (one rule, two implementations): opinion always precedes
#             the outcome by one year.
#             (i)  Onset events happen DURING year t -> IVs enter at t-1.
#             (ii) Boundary events are lead-coded (flagged in the outgoing regime's last year t, change realized t->t+1) -> year-t covariates already precede the change, so no lag.
#
#     sec. 8b Same 14 cells + 9 controls (onset cells: whole RHS at t-1).
#     sec. 8c Canonical battery, 22 cells: {DD, GWF, BMR} x {breakdown,
#             democratization, any-change (DD/GWF), crossing split-IVs} x
#             {base, +ctrl}, each with the coding's own clock/history/dummy.
#     sec. 8d [commented out] hazard interactions -- archived; MOC-null.
#     sec. 9  Assembles all +ctrl cells -> results_main_combined.{rds,csv}.
#     Asymmetry tests (aut - dem slope, draw-wise differences with covariance)
#     are computed in codes/final_02_tables_main.R for the paper tables.

# ============================================================================
# 2.  LOAD OPINION IVs (900 posterior draws + first-survey trims)
# ============================================================================
# discontent + interest + support: 900 draws each, sampled with seed 123.
load(here("data", "discontent_theta_results.rda")) # Distribution
load(here("data", "discontent_theta_summary.rda")) # Point-estimate (mean)

disc_raw <- theta_results %>% rename(discontent = theta)
disc_list <- split(disc_raw, disc_raw$draw)
set.seed(123)
disc_list <- disc_list[sample(length(disc_list), N_DRAWS)] # Sample 900 draws
disc_list <- disc_list[order(as.numeric(names(disc_list)))]

supp_list <- readRDS(here("data", "supdem_exp_dcpo_theta.rds")) # SupDem draws in list format
supp_list <- map(supp_list, ~ mutate(.x, supdem = theta * 100)) # to align with the range of discontent

# first ACTUAL survey year per country -- used to trim away DCPO backward extrapolation (model-only estimates before any survey exists).
disc_first <- read.csv(here("data", "discontent_dcpo_input_raw.csv")) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(disc_fy = min(year), .groups = "drop")
supp_first <- readRDS(here("data", "supdem_exp_ajps_cntrl_list.rds"))[[1]] %>% # firstyear
  dplyr::distinct(country, firstyear) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c), !is.na(firstyear)) %>%
  group_by(iso3c) %>%
  summarise(supp_fy = min(firstyear), .groups = "drop")

# ============================================================================
# 3.  DVs -- REGIME OUTCOMES (V-Dem levels; ERT/RoW; DD; GWF; BMR)
# ============================================================================

# V-Dem - 2 DVs
dem <- vdem %>%
  mutate(
    iso3c = countrycode(country_id, "vdem", "iso3c"),
    year = year,
    libdem = v2x_libdem, polyarchy = v2x_polyarchy,
    .keep = "none"
  ) %>%
  filter(!is.na(iso3c))

# ERT three regime outcomes + regime-age clock---------
ert_cy <- ERT::get_eps() %>%
  dplyr::select(
    country_id, year, v2x_regime, reg_trans,
    dem_ep, dem_ep_start_year, aut_ep, aut_ep_start_year
  ) %>%
  mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
  filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    new_spell = replace_na(as.integer(v2x_regime != dplyr::lag(v2x_regime)), 0L), # Change = 1/ Same or else = 0
    ert_spell = cumsum(new_spell) + 1, # regime ID
    ert_any_pre_change = ert_spell - 1L,
    ert_any_change = replace_na(as.integer(dplyr::lead(v2x_regime) != v2x_regime), 0L), # mark the end of the regime (change in a four-category of Row)
    ert_change_down = replace_na(as.integer(dplyr::lead(v2x_regime) < v2x_regime), 0L), # mark the end of the regime leading to a LOWER category (autocratization)
    ert_change_up = replace_na(as.integer(dplyr::lead(v2x_regime) > v2x_regime), 0L)
  ) %>% # mark the end of the regime leading to a HIGHER category (democratization)
  group_by(iso3c, ert_spell) %>%
  mutate(ert_regime_age = row_number()) %>%
  ungroup() %>%
  # Do NOT use ERT's reg_trans rows directly as events: ERT calls the transition year the old regime's "final year", but V-Dem's year-end v2x_regime at that row already shows the NEW regime
  # Using them as they are puts breakdowns in the AUTOCRACY subsample, transitions in the DEMOCRACY subsample, with covariates measured post-transition.
  # Thus lead function is used to fix this
  group_by(iso3c) %>%
  mutate(
    ert_dem_break = replace_na(as.integer(v2x_regime >= 2 & dplyr::lead(v2x_regime) <= 1), 0L), # last dem year before crossing down
    ert_democratization = replace_na(as.integer(v2x_regime <= 1 & dplyr::lead(v2x_regime) >= 2), 0L), # last aut year before crossing up
    ert_any_regime_trans = pmax(ert_dem_break, ert_democratization)
  ) %>%
  ungroup() %>%
  mutate(
    # episode ONSET events: first year of an ERT episode (dem_ep/aut_ep are multi-year state dummies -> only the start year is the event)
    ert_dem_onset = as.integer(replace_na(dem_ep, 0) == 1 & year == dem_ep_start_year),
    ert_aut_onset = as.integer(replace_na(aut_ep, 0) == 1 & year == aut_ep_start_year),
    ert_any_onset = pmax(ert_dem_onset, ert_aut_onset), # either kind begins
    # risk sets: drop ongoing-episode years (can't newly 'start' mid-episode; sample filter for survival analysis)
    ert_dem_risk  = as.integer(!(replace_na(dem_ep, 0) == 1 & ert_dem_onset == 0)), # 1 for years outside an ONGOING democratization episode (episode start year stays at risk)
    ert_aut_risk  = as.integer(!(replace_na(aut_ep, 0) == 1 & ert_aut_onset == 0)), # 1 for years outside an ONGOING autocratization episode (episode start year stays at risk)
    ert_any_risk  = as.integer(ert_dem_risk == 1 & ert_aut_risk == 1)
  ) # outside BOTH episode types (episode start year stays at risk)

# ACLP/DD dichotomous regime events -- Bjornskov-Rode update of Cheibub-Gandhi-Vreeland (coverage to ~2025, fixing the 2008 truncation of the original).
dd_cy <- democracyData::pacl_update %>%
  mutate(iso3c = countrycode(pacl_update_country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c), !is.na(Democracy)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    # demo track: dichotomy boundary crossings (lead-coded to mark last year of the old side) + dichotomy spell/clock
    dd_democracy = Democracy,
    dd_demo_break = replace_na(as.integer(dd_democracy == 1 & dplyr::lead(dd_democracy) == 0), 0L),
    dd_democratization = replace_na(as.integer(dd_democracy == 0 & dplyr::lead(dd_democracy) == 1), 0L),
    dd_cross = pmax(dd_demo_break, dd_democratization), # either crossing (split-IV DV)

    # any track: ANY regime change on the 6-category DD_regime
    dd_spell = cumsum(replace_na(as.integer(DD_regime != dplyr::lag(DD_regime)), 0L)) + 1L,
    dd_any_change = replace_na(as.integer(DD_regime != dplyr::lead(DD_regime)), 0L),

    # transitions before the current regime spell (= spell ID - 1)
    dd_any_pre_change = dd_spell - 1L
  ) %>%
  group_by(iso3c, dd_spell) %>%
  mutate(dd_regime_age = row_number()) %>%
  ungroup() %>%
  dplyr::select(
    iso3c, year, dd_spell, dd_any_change, dd_regime_age, dd_any_pre_change,
    dd_democracy, dd_demo_break, dd_democratization, dd_cross
  ) %>%
  distinct(iso3c, year, .keep_all = TRUE)

# GWF regimes -- ALL regime spells (gwf_all) (ends 2010 -> ALL GWF cells remain supplementary.)
gwf_cy <- democracyData::gwf_all %>%
  mutate(iso3c = countrycode(gwf_cowcode, "cown", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    # any track: ANY GWF regime change. gwf_fail is END-coded (old regime's last year; so no lead function is needed);
    # the gwf_casename variable can generate spell ID
    gwf_spell = cumsum(replace_na(as.integer(gwf_casename != dplyr::lag(gwf_casename)), 0L)) + 1L,
    gwf_any_change = replace_na(gwf_fail, 0L),
    gwf_regime_age = gwf_duration,

    # transitions before the current regime spell
    gwf_any_pre_change = gwf_spell - 1L,

    # demo track: gwf_nonautocracy labels non-autocratic spells directly (democracy/provisional/warlord/occupied; NA = autocratic)
    gwf_democracy = replace_na(as.integer(gwf_nonautocracy == "democracy"), 0L), # NA rows (autocracies) -> 0
    gwf_democratization = replace_na(as.integer(gwf_any_change == 1 & gwf_democracy == 0 &
      gwf_next == "democracy"), 0L),
    gwf_demo_break = replace_na(as.integer(gwf_any_change == 1 & gwf_democracy == 1 &
      gwf_next != "democracy"), 0L),
    gwf_cross = pmax(gwf_demo_break, gwf_democratization)
  ) %>% # either crossing (split-IV DV; aut->aut fails excluded)
  ungroup() %>%
  dplyr::select(
    iso3c, year,
    gwf_spell, gwf_any_change, gwf_regime_age, gwf_any_pre_change,
    gwf_democracy, gwf_democratization, gwf_demo_break, gwf_cross
  ) %>%
  distinct(iso3c, year, .keep_all = TRUE)

# Boix-Miller-Rosato dichotomous democracy (1800-2020)
bmr_cy <- democracyData::bmr %>%
  mutate(iso3c = countrycode(cown, "cown", "iso3c")) %>%
  filter(!is.na(iso3c), !is.na(democracy)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    # demo track only (BMR is a pure dichotomy, so demo == any)
    bmr_democracy = democracy,
    bmr_demo_break = replace_na(as.integer(democracy == 1 & dplyr::lead(democracy) == 0), 0L),
    bmr_democratization = replace_na(as.integer(democracy == 0 & dplyr::lead(democracy) == 1), 0L),
    bmr_regime_age = democracy_duration,
    bmr_spell = cumsum(replace_na(as.integer(democracy != dplyr::lag(democracy)), 0L)) + 1L,
    bmr_any_pre_change = bmr_spell - 1L,
    bmr_cross = pmax(bmr_demo_break, bmr_democratization)
  ) %>%
  ungroup() %>% # either crossing (split-IV DV)
  dplyr::select(
    iso3c, year, bmr_spell, bmr_regime_age, bmr_any_pre_change,
    bmr_democracy, bmr_democratization, bmr_demo_break, bmr_cross
  ) %>%
  distinct(iso3c, year, .keep_all = TRUE)

# ############################################################################
# 4. CONTROLS
# ############################################################################
ctrl_raw <- readRDS(here("data", "supdem_exp_ajps_cntrl_list.rds"))
ctrl_key <- tibble(country = unique(ctrl_raw[[1]]$country)) %>% # name->iso3c map computed once
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c))
# Variable names kept as in the source (Tai et al. replication data; the "_imp" suffix marks THEIR multiple imputation)).
# The source's pre-built lag columns are dropped: our lags are built within OUR merged panel in build_one, so panel-entry rows are consistent.
ctrl_list <- map(ctrl_raw, ~ .x %>%
  inner_join(ctrl_key, by = "country") %>%
  dplyr::select(
    iso3c, year, lnGDP_imp, GDP_imp_grth,
    Libdem_regUN,
    Pr_Muslim, Res_cp_WDI_di
  ))
rm(ctrl_raw)

# OECD membership (Gassebner-Lamla-Vreeland 2013 robust determinant of democratic transition). Accession years hard-coded from OECD records;
# oecd dummy = 1 from the accession year onward (built in build_one).
oecd_acc <- tibble(
  iso3c = c(
    "AUT", "BEL", "CAN", "DNK", "FRA", "DEU", "GRC", "ISL", "IRL", "ITA",
    "LUX", "NLD", "NOR", "PRT", "ESP", "SWE", "CHE", "TUR", "GBR", "USA",
    "JPN", "FIN", "AUS", "NZL", "MEX", "CZE", "HUN", "KOR", "POL", "SVK",
    "CHL", "SVN", "ISR", "EST", "LVA", "LTU", "COL", "CRI"
  ),
  oecd_year = c(
    rep(1961, 20),
    1964, 1969, 1971, 1973, 1994, 1995, 1996, 1996, 1996, 2000,
    2010, 2010, 2010, 2010, 2016, 2018, 2020, 2021
  )
)

# Inequality + population
# Gini: SWIID 9.8, auto-downloaded from Dataverse
if (!file.exists(here(
  "data-raw",
  "swiid9_8",
  "swiid9_8_summary.csv"
))) {
  download.file(
    "https://dataverse.harvard.edu/api/access/datafile/10797566",
    here("data-raw", "swiid9_8.zip")
  )
  unzip(here("data-raw", "swiid9_8.zip"), exdir = here("data-raw"))
  file.remove(here("data-raw", "swiid9_8.zip"))
}

swiid_summary <- read_csv(
  here(
    "data-raw",
    "swiid9_8",
    "swiid9_8_summary.csv"
  ),
  col_types = "cddddddddd"
) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c",
    warn = FALSE
  )) %>%
  filter(!is.na(iso3c)) %>%
  dplyr::select(iso3c, year, gini_disp, gini_disp_se) %>%
  distinct(iso3c, year, .keep_all = TRUE)

# Population: World Bank SP.POP.TOTL
if (!length(list.files(here("data-raw", "wb_population"),
  pattern = "^API_SP\\.POP\\.TOTL.*\\.csv$"
))) {
  dir.create(here("data-raw", "wb_population"), showWarnings = FALSE)
  download.file("https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv",
    here("data-raw", "wb_population.zip"),
    mode = "wb"
  )
  unzip(here("data-raw", "wb_population.zip"),
    exdir = here("data-raw", "wb_population")
  )
  file.remove(here("data-raw", "wb_population.zip"))
}

pop_cy <- read_csv(
  list.files(here("data-raw", "wb_population"),
    pattern = "^API_SP\\.POP\\.TOTL.*\\.csv$",
    full.names = TRUE
  ),
  skip = 4, show_col_types = FALSE
) %>%
  pivot_longer(matches("^\\d{4}$"),
    names_to = "year", values_to = "pop"
  ) %>%
  mutate(
    iso3c = `Country Code`,
    year = as.integer(year),
    lnpop = log(pop),
    .keep = "none"
  ) %>%
  filter(!is.na(lnpop))

# gini + lnpop panel joined into build_one
cd_cy <- swiid_summary %>%
  dplyr::select(iso3c, year, gini = gini_disp) %>%
  full_join(pop_cy, by = c("iso3c", "year"))

# ############################################################################
# 5. COMBINE DATA -- build_one() x 900 draws + naive_dat
# ############################################################################
build_one <- function(dsc, supp, ctrl) {
  dsc <- dsc %>% mutate(iso3c = countrycode(country, "country.name", "iso3c"))
  supp <- supp %>% mutate(iso3c = countrycode(country, "country.name", "iso3c"))

  dem1 <- dem %>%
    inner_join(dplyr::select(dsc, iso3c, year, discontent), by = c("iso3c", "year")) %>%
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(d_disc = discontent - dplyr::lag(discontent)) %>%
    ungroup()

  dem2 <- dem1 %>%
    left_join(dplyr::select(supp, iso3c, year, supdem), by = c("iso3c", "year")) %>% # supdem has a shorter coverage
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(d_supp = supdem - dplyr::lag(supdem)) %>%
    ungroup()

  dem2 %>%
    left_join(dplyr::select(
      ert_cy, iso3c, year, v2x_regime, reg_trans, new_spell, ert_spell, ert_any_pre_change,
      ert_any_change, ert_change_down, ert_change_up,
      ert_regime_age, ert_dem_break, ert_democratization, ert_any_regime_trans,
      ert_dem_onset, ert_aut_onset, ert_any_onset,
      ert_dem_risk, ert_aut_risk, ert_any_risk
    ), by = c("iso3c", "year")) %>%
    left_join(ctrl, by = c("iso3c", "year")) %>% # AJPS controls
    left_join(dd_cy, by = c("iso3c", "year")) %>% # ACLP/DD events
    left_join(gwf_cy, by = c("iso3c", "year")) %>% # GWF breakdown
    left_join(bmr_cy, by = c("iso3c", "year")) %>% # BMR events
    left_join(disc_first, by = "iso3c") %>% # First year of the discontent survey
    left_join(supp_first, by = "iso3c") %>% # First year of the supdem survey
    left_join(oecd_acc, by = "iso3c") %>% # OECD control
    left_join(cd_cy, by = c("iso3c", "year")) %>% # gini + lnpop
    mutate(
      oecd = as.integer(!is.na(oecd_year) & year >= oecd_year),

      # first-year trim: NA before each country's first actual survey to drop DCPO backward extrapolation
      disc_trim = ifelse(!is.na(disc_fy) & year >= disc_fy, discontent, NA_real_),
      supp_trim = ifelse(!is.na(supp_fy) & year >= supp_fy, supdem, NA_real_),

      # regime-split trims:theta as it is in own-regime years, 0 elsewhere
      # regime-specific slopes estimated jointly in ONE pooled model (equivalent to a full theta x regime-dummy interaction)
      vdem_democracy = as.integer(v2x_regime >= 2), # RoW democracy (== published Regime_VD > 1)
      disc_dem_trim = case_when(
        is.na(disc_trim) | is.na(vdem_democracy) ~ NA_real_,
        vdem_democracy == 1 ~ disc_trim, TRUE ~ 0
      ),
      disc_aut_trim = case_when(
        is.na(disc_trim) | is.na(vdem_democracy) ~ NA_real_,
        vdem_democracy == 0 ~ disc_trim, TRUE ~ 0
      ),
      supp_dem_trim = case_when(
        is.na(supp_trim) | is.na(vdem_democracy) ~ NA_real_,
        vdem_democracy == 1 ~ supp_trim, TRUE ~ 0
      ),
      supp_aut_trim = case_when(
        is.na(supp_trim) | is.na(vdem_democracy) ~ NA_real_,
        vdem_democracy == 0 ~ supp_trim, TRUE ~ 0
      ),

      # regime-split trims for each coding rule: each coding splits by ITS OWN regime dummy (the V-Dem-based trims above would misclassify rows where codings disagree)
      disc_dd_dem_trim = case_when(
        is.na(disc_trim) | is.na(dd_democracy) ~ NA_real_,
        dd_democracy == 1 ~ disc_trim, TRUE ~ 0
      ),
      disc_dd_aut_trim = case_when(
        is.na(disc_trim) | is.na(dd_democracy) ~ NA_real_,
        dd_democracy == 0 ~ disc_trim, TRUE ~ 0
      ),
      supp_dd_dem_trim = case_when(
        is.na(supp_trim) | is.na(dd_democracy) ~ NA_real_,
        dd_democracy == 1 ~ supp_trim, TRUE ~ 0
      ),
      supp_dd_aut_trim = case_when(
        is.na(supp_trim) | is.na(dd_democracy) ~ NA_real_,
        dd_democracy == 0 ~ supp_trim, TRUE ~ 0
      ),
      disc_gwf_dem_trim = case_when(
        is.na(disc_trim) | is.na(gwf_democracy) ~ NA_real_,
        gwf_democracy == 1 ~ disc_trim, TRUE ~ 0
      ),
      disc_gwf_aut_trim = case_when(
        is.na(disc_trim) | is.na(gwf_democracy) ~ NA_real_,
        gwf_democracy == 0 ~ disc_trim, TRUE ~ 0
      ),
      supp_gwf_dem_trim = case_when(
        is.na(supp_trim) | is.na(gwf_democracy) ~ NA_real_,
        gwf_democracy == 1 ~ supp_trim, TRUE ~ 0
      ),
      supp_gwf_aut_trim = case_when(
        is.na(supp_trim) | is.na(gwf_democracy) ~ NA_real_,
        gwf_democracy == 0 ~ supp_trim, TRUE ~ 0
      ),
      disc_bmr_dem_trim = case_when(
        is.na(disc_trim) | is.na(bmr_democracy) ~ NA_real_,
        bmr_democracy == 1 ~ disc_trim, TRUE ~ 0
      ),
      disc_bmr_aut_trim = case_when(
        is.na(disc_trim) | is.na(bmr_democracy) ~ NA_real_,
        bmr_democracy == 0 ~ disc_trim, TRUE ~ 0
      ),
      supp_bmr_dem_trim = case_when(
        is.na(supp_trim) | is.na(bmr_democracy) ~ NA_real_,
        bmr_democracy == 1 ~ supp_trim, TRUE ~ 0
      ),
      supp_bmr_aut_trim = case_when(
        is.na(supp_trim) | is.na(bmr_democracy) ~ NA_real_,
        bmr_democracy == 0 ~ supp_trim, TRUE ~ 0
      ),
      # Interaction product:
      # DOUBLE-CENTERED product on RAW main effects. It is because the fully raw  product is numerically fatal at the 0-100 theta scale
      # Thus, the raw product number has long decimal numbers which pgmm model consider as "computationally singular"
      # Centering the product can solve this problem (but the interpretation should be done carefully
      disc_supp = (disc_trim - mean(disc_trim, na.rm = TRUE)) *
        (supp_trim - mean(supp_trim, na.rm = TRUE))
    ) %>%
    # lagged IVs + CONTROLS
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      disc_trim_lag1          = dplyr::lag(disc_trim),
      supp_trim_lag1          = dplyr::lag(supp_trim),
      disc_supp_lag1          = dplyr::lag(disc_supp),
      disc_dem_trim_lag1      = dplyr::lag(disc_dem_trim),
      disc_aut_trim_lag1      = dplyr::lag(disc_aut_trim),
      supp_dem_trim_lag1      = dplyr::lag(supp_dem_trim),
      supp_aut_trim_lag1      = dplyr::lag(supp_aut_trim),
      vdem_democracy_lag1     = dplyr::lag(vdem_democracy),
      lnGDP_imp_lag1          = dplyr::lag(lnGDP_imp),
      GDP_imp_grth_lag1       = dplyr::lag(GDP_imp_grth),
      Libdem_regUN_lag1       = dplyr::lag(Libdem_regUN),
      Pr_Muslim_lag1          = dplyr::lag(Pr_Muslim),
      Res_cp_WDI_di_lag1      = dplyr::lag(Res_cp_WDI_di),
      ert_any_pre_change_lag1 = dplyr::lag(ert_any_pre_change),
      oecd_lag1               = dplyr::lag(oecd),
      gini_lag1               = dplyr::lag(gini),
      lnpop_lag1              = dplyr::lag(lnpop)
    ) %>%
    ungroup()
}

data <- pmap(list(disc_list, supp_list, ctrl_list), build_one)

# Dataset for naive point-estimate analysis
naive_dat <- data[[1]]

# Posterior means for every numeric SOURCE column: draw-varying columns need it, draw-constant columns average to themselves (no-op)
derived <- c("disc_supp", grep("_lag1$", names(naive_dat), value = TRUE))
for (v in setdiff(names(naive_dat)[sapply(naive_dat, is.numeric)], derived)) {
  naive_dat[[v]] <- rowMeans(vapply(data, function(d) d[[v]], numeric(nrow(naive_dat))), na.rm = TRUE)
}

# the interaction product must be REBUILT from the averaged components (E[XY] != E[X]E[Y]; averaging per-draw products is not the naive estimator)
naive_dat$disc_supp <- (naive_dat$disc_trim - mean(naive_dat$disc_trim, na.rm = TRUE)) * (naive_dat$supp_trim - mean(naive_dat$supp_trim, na.rm = TRUE))
naive_dat <- naive_dat %>%
  group_by(iso3c) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    disc_trim_lag1          = dplyr::lag(disc_trim),
    supp_trim_lag1          = dplyr::lag(supp_trim),
    disc_supp_lag1          = dplyr::lag(disc_supp),
    disc_dem_trim_lag1      = dplyr::lag(disc_dem_trim),
    disc_aut_trim_lag1      = dplyr::lag(disc_aut_trim),
    supp_dem_trim_lag1      = dplyr::lag(supp_dem_trim),
    supp_aut_trim_lag1      = dplyr::lag(supp_aut_trim),
    vdem_democracy_lag1     = dplyr::lag(vdem_democracy),
    lnGDP_imp_lag1          = dplyr::lag(lnGDP_imp),
    GDP_imp_grth_lag1       = dplyr::lag(GDP_imp_grth),
    Libdem_regUN_lag1       = dplyr::lag(Libdem_regUN),
    Pr_Muslim_lag1          = dplyr::lag(Pr_Muslim),
    Res_cp_WDI_di_lag1      = dplyr::lag(Res_cp_WDI_di),
    ert_any_pre_change_lag1 = dplyr::lag(ert_any_pre_change),
    oecd_lag1               = dplyr::lag(oecd),
    gini_lag1               = dplyr::lag(gini),
    lnpop_lag1              = dplyr::lag(lnpop)
  ) %>%
  ungroup()

# ############################################################################
# 7. Base model (without controls) -- Tai et al. dynamic-panel form
#    y_t = rho1*y_{t-1} + rho2*y_{t-2} + b1*disc_{t-1} + b2*supp_{t-1}
#    DV = regime-stability measure (liberal democracy / polyarchy)
#    pooled OLS  -> methodComposition1 (vcovBK: Beck-Katz PCSE, cluster=time)
#    system GMM  -> methodComposition2 (plmHC:  Arellano,       cluster=group)
#    Two approaches: 1) naive point est (posterior means) and 2) MOC (900 draws).
# ############################################################################

data_plm <- purrr::map(1:N_DRAWS, function(num) {
  data[[num]] %>%
    as.data.frame() %>%
    plm::pdata.frame(index = c("iso3c", "year"))
})
data_plm[[1]] %>%
  count(iso3c, year) %>%
  filter(n > 1)

# --- SPECS [ACTIVE] ---------------------------------------------------------
# IV -- two specs, mirroring the published Final_analysis:
#  [1] pooled:        first-year-trimmed discontent + support
#  [2] pooled-regime: regime-split trims (dem/aut slopes in one model)
ls_iv <- c(
  "plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1)",
  "plm::lag(disc_dem_trim, 1) + plm::lag(disc_aut_trim, 1) + plm::lag(supp_dem_trim, 1) + plm::lag(supp_aut_trim, 1)"
)

# DV
ls_eq1 <-
  glue(
    "libdem ~ plm::lag(libdem, 1:2) + {ls_iv}"
  )

ls_eq2 <-
  glue(
    "polyarchy ~ plm::lag(polyarchy, 1:2) + {ls_iv}"
  )

# Model
# 1. Pooling Model
# 2. GMM model:GMM instruments: lag(DV, 3:5), collapse=TRUE
# DIAGNOSTICS, read as follows:
#   - Sargan/Hansen: are the instruments uncorrelated with the error?
#     LARGE p = pass. But p ~= 1.000 is NOT a pass -- with too many instruments the test loses all power and pins to 1 (the test is dead, not passed).
#     Uncollapsed 3:5 gave 214 instruments and Sargan p=1.000 -- the classic over-instrumentation artifact.
#   - AR(2) of the differenced errors must be INSIGNIFICANT: if second-order serial correlation survives, the t-3+ instruments are not exogenous and the whole design fails.
#   - Roodman rule of thumb: #instruments < #countries.
#     Setting collapse=TRUE folds the per-period instrument columns into one columnv per lag (Roodman 2009), cutting the count from hundreds to ~11.
ls_mod_base1 <- c(
  glue("plm({ls_eq1}, model = 'pooling', data = aData)"),
  glue(
    "pgmm({ls_eq1} | plm::lag(libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))"
  )
)

ls_mod_base2 <- c(
  glue("plm({ls_eq2}, model = 'pooling', data = aData)"),
  glue(
    "pgmm({ls_eq2} | plm::lag(polyarchy, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))"
  )
)

# --- NAIVE INFRA ----
naive_p <- naive_dat %>%
  as.data.frame() %>%
  plm::pdata.frame(index = c("iso3c", "year"))

naive_fit <- function(aMod, gmm = FALSE) {
  aData <- naive_p
  m <- eval(parse(text = aMod))
  V <- if (gmm) plmHC(m) else vcovBK(m) # same vcov convention as the MOC
  tibble(
    term = names(coef(m)),
    naive_est = as.numeric(coef(m)),
    naive_se = sqrt(diag(V)),
    nobs = stats::nobs(m)
  )
}

# --- ESTIMATE & SAVE [ran 2026-07-29]
#[1] plm pooled, [2] plm pooled-regime, [3] pgmm, [4] pgmm-regime
set.seed(313)
  result_base1 <-
    map(ls_mod_base1[1:2], function(aMod){
      result <- methodComposition1(data_plm, aMod)
    })
  names(result_base1) <- c("pooled", "pooled-regime")

  result_base_gmm1 <-
    map(ls_mod_base1[3:4], function(aMod){
      result <- methodComposition2(data_plm, aMod)
    })
  names(result_base_gmm1) <- c("gmm", "gmm-regime")

  #Electoral democracy (polyarchy)
  result_base2 <-
    map(ls_mod_base2[1:2], function(aMod){
      result <- methodComposition1(data_plm, aMod)
    })
  names(result_base2) <- c("pooled", "pooled-regime")

  result_base_gmm2 <-
    map(ls_mod_base2[3:4], function(aMod){
      result <- methodComposition2(data_plm, aMod)
    })
  names(result_base_gmm2) <- c("gmm", "gmm-regime")

  result_base_libdem    <- append(result_base1, result_base_gmm1)
  result_base_polyarchy <- append(result_base2, result_base_gmm2)

  saveRDS(result_base_libdem,    here("output", "results_base_libdem.rds"))
  saveRDS(result_base_polyarchy, here("output", "results_base_polyarchy.rds"))
  # Naive (posterior-mean point estimates)
  naive_base_libdem <- list(
    `pooled`        = naive_fit(ls_mod_base1[1]),
    `pooled-regime` = naive_fit(ls_mod_base1[2]),
    `gmm`           = naive_fit(ls_mod_base1[3], gmm = TRUE),
    `gmm-regime`    = naive_fit(ls_mod_base1[4], gmm = TRUE)
  )
  naive_base_polyarchy <- list(
    `pooled`        = naive_fit(ls_mod_base2[1]),
    `pooled-regime` = naive_fit(ls_mod_base2[2]),
    `gmm`           = naive_fit(ls_mod_base2[3], gmm = TRUE),
    `gmm-regime`    = naive_fit(ls_mod_base2[4], gmm = TRUE)
  )

  saveRDS(naive_base_libdem,    here("output", "results_base_libdem_naive.rds"))
  saveRDS(naive_base_polyarchy, here("output", "results_base_polyarchy_naive.rds"))

# --- PRESENT ---------------------------------
result_base_libdem <- readRDS(here("output", "results_base_libdem.rds"))
result_base_polyarchy <- readRDS(here("output", "results_base_polyarchy.rds"))
naive_base_libdem <- readRDS(here("output", "results_base_libdem_naive.rds"))
naive_base_polyarchy <- readRDS(here("output", "results_base_polyarchy_naive.rds"))

combine_one <- function(moc, naive, submodel, type) {
  # NOTE: coefficients are ~1e-4 (DV 0-1, IVs 0-100),
  # so (a) MOCsumm must run at digits=6 (its default 3 rounds est AND se to 0.000 -> p = 0/0 = NaN),
  # and (b) p-values must be computed BEFORE rounding the estimates.
  MOCsumm(moc, digits = 6) %>%
    left_join(naive, by = "term") %>%
    filter(term != "(Intercept)") %>%
    transmute(
      submodel = submodel, type = type, term = term,
      naive_p = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
      naive_est = signif(naive_est, 3), # signif, not round: interaction coefs are ~1e-6
      moc_p = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
      moc_est = signif(estimate, 3),
      moc_se = signif(std.error, 3),
      nobs = nobs
    ) %>%
    dplyr::select(
      submodel, type, term, naive_est, naive_p,
      moc_est, moc_se, moc_p, nobs
    )
}

result_base <- bind_rows(
  combine_one(result_base_libdem[["pooled"]], naive_base_libdem[["pooled"]], "Liberal Democracy", "Pooled~OLS"),
  combine_one(result_base_libdem[["pooled-regime"]], naive_base_libdem[["pooled-regime"]], "Liberal Democracy", "Pooled~OLS (regime)"),
  combine_one(result_base_libdem[["gmm"]], naive_base_libdem[["gmm"]], "Liberal Democracy", "System~GMM"),
  combine_one(result_base_libdem[["gmm-regime"]], naive_base_libdem[["gmm-regime"]], "Liberal Democracy", "System~GMM (regime)"),
  combine_one(result_base_polyarchy[["pooled"]], naive_base_polyarchy[["pooled"]], "Electoral Democracy", "Pooled~OLS"),
  combine_one(result_base_polyarchy[["pooled-regime"]], naive_base_polyarchy[["pooled-regime"]], "Electoral Democracy", "Pooled~OLS (regime)"),
  combine_one(result_base_polyarchy[["gmm"]], naive_base_polyarchy[["gmm"]], "Electoral Democracy", "System~GMM"),
  combine_one(result_base_polyarchy[["gmm-regime"]], naive_base_polyarchy[["gmm-regime"]], "Electoral Democracy", "System~GMM (regime)")
)

star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))

# gmm-regime rows: cautionary only -- diagnostics reject this cell.
# Window probe (draw 1; 3:5/4:6/4:8/5:8 collapsed): all estimate, but AR(2) rejects in ALL EIGHT cells (.004-.031)
# No instrument depth rescues it
# Thus, the results of gmm-regime should not be accepted.

cat("\n====== BASE: naive vs MOC ======\n")
print(result_base %>%
  mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>%
  as.data.frame())

# --- DIAGNOSTICS --------
# per DV: first row = pooled gmm, second row = gmm-regime
cat("\n------ system-GMM diagnostics (naive) ------\n")
for (aMod in c(ls_mod_base1[3:4], ls_mod_base2[3:4])) {
  aData <- naive_p
  g <- tryCatch(eval(parse(text = aMod)), error = function(e) NULL)
  if (is.null(g)) {
    cat(substr(aMod, 6, 30), ": pgmm failed\n")
    next
  }
  s <- summary(g, robust = TRUE)
  n_inst <- dim(g$W[[1]])[2]
  n_grp <- pdim(g)$nT$n
  cat(sprintf(
    "%-30s | #inst=%d  #countries=%d (%s) | Sargan p=%.3f | AR(2) p=%.3f\n",
    substr(aMod, 6, 35), n_inst, n_grp,
    ifelse(n_inst <= n_grp, "Roodman OK", "OVER-INSTRUMENTED"),
    s$sargan$p.value, plm::mtest(g, order = 2)$p.value
  ))
}

# ############################################################################
# 7b. Base + controls -- the full published specification
# ############################################################################

ls_ctrl <- paste(
  "plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) +",
  "plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) +",
  "plm::lag(Res_cp_WDI_di, 1) + plm::lag(ert_any_pre_change, 1) +",
  "plm::lag(oecd, 1) + plm::lag(gini, 1) + plm::lag(lnpop, 1)"
)

ls_eq1c <- glue("libdem ~ plm::lag(libdem, 1:2) + {ls_iv} + {ls_ctrl}")
ls_eq2c <- glue("polyarchy ~ plm::lag(polyarchy, 1:2) + {ls_iv} + {ls_ctrl}")

ls_mod_ctrl1 <- c(
  glue("plm({ls_eq1c}, model = 'pooling', data = aData)"),
  glue("pgmm({ls_eq1c} | plm::lag(libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
)
ls_mod_ctrl2 <- c(
  glue("plm({ls_eq2c}, model = 'pooling', data = aData)"),
  glue("pgmm({ls_eq2c} | plm::lag(polyarchy, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
)

# --- ESTIMATE & SAVE [Ran 2026-07-29; ---------------------------------
# [1] pooled, [2] pooled-regime, [3] gmm, [4] gmm-regime
set.seed(313)
result_ctrl1 <- map(ls_mod_ctrl1[1:2], function(aMod) methodComposition1(data_plm, aMod))
names(result_ctrl1) <- c("pooled", "pooled-regime")
result_ctrl_gmm1 <- map(ls_mod_ctrl1[3:4], function(aMod) methodComposition2(data_plm, aMod))
names(result_ctrl_gmm1) <- c("gmm", "gmm-regime")

result_ctrl2 <- map(ls_mod_ctrl2[1:2], function(aMod) methodComposition1(data_plm, aMod))
names(result_ctrl2) <- c("pooled", "pooled-regime")
result_ctrl_gmm2 <- map(ls_mod_ctrl2[3:4], function(aMod) methodComposition2(data_plm, aMod))
names(result_ctrl_gmm2) <- c("gmm", "gmm-regime")

result_ctrl_libdem    <- append(result_ctrl1, result_ctrl_gmm1)
result_ctrl_polyarchy <- append(result_ctrl2, result_ctrl_gmm2)

saveRDS(result_ctrl_libdem,    here("output", "results_ctrlglv_libdem.rds"))
saveRDS(result_ctrl_polyarchy, here("output", "results_ctrlglv_polyarchy.rds"))

# Naive (same models on posterior means, same robust vcov) ------------------
naive_ctrl_libdem <- list(
  `pooled`        = naive_fit(ls_mod_ctrl1[1]),
  `pooled-regime` = naive_fit(ls_mod_ctrl1[2]),
  `gmm`           = naive_fit(ls_mod_ctrl1[3], gmm = TRUE),
  `gmm-regime`    = naive_fit(ls_mod_ctrl1[4], gmm = TRUE)
)
naive_ctrl_polyarchy <- list(
  `pooled`        = naive_fit(ls_mod_ctrl2[1]),
  `pooled-regime` = naive_fit(ls_mod_ctrl2[2]),
  `gmm`           = naive_fit(ls_mod_ctrl2[3], gmm = TRUE),
  `gmm-regime`    = naive_fit(ls_mod_ctrl2[4], gmm = TRUE)
)
saveRDS(naive_ctrl_libdem,    here("output", "results_ctrlglv_libdem_naive.rds"))
saveRDS(naive_ctrl_polyarchy, here("output", "results_ctrlglv_polyarchy_naive.rds"))

# --- PRESENT ---------------------------------
result_ctrl_libdem <- readRDS(here("output", "results_ctrlglv_libdem.rds"))
result_ctrl_polyarchy <- readRDS(here("output", "results_ctrlglv_polyarchy.rds"))
naive_ctrl_libdem <- readRDS(here("output", "results_ctrlglv_libdem_naive.rds"))
naive_ctrl_polyarchy <- readRDS(here("output", "results_ctrlglv_polyarchy_naive.rds"))

result_ctrl <- bind_rows(
  combine_one(result_ctrl_libdem[["pooled"]], naive_ctrl_libdem[["pooled"]], "Liberal Democracy", "Pooled~OLS"),
  combine_one(result_ctrl_libdem[["pooled-regime"]], naive_ctrl_libdem[["pooled-regime"]], "Liberal Democracy", "Pooled~OLS (regime)"),
  combine_one(result_ctrl_libdem[["gmm"]], naive_ctrl_libdem[["gmm"]], "Liberal Democracy", "System~GMM"),
  combine_one(result_ctrl_libdem[["gmm-regime"]], naive_ctrl_libdem[["gmm-regime"]], "Liberal Democracy", "System~GMM (regime)"),
  combine_one(result_ctrl_polyarchy[["pooled"]], naive_ctrl_polyarchy[["pooled"]], "Electoral Democracy", "Pooled~OLS"),
  combine_one(result_ctrl_polyarchy[["pooled-regime"]], naive_ctrl_polyarchy[["pooled-regime"]], "Electoral Democracy", "Pooled~OLS (regime)"),
  combine_one(result_ctrl_polyarchy[["gmm"]], naive_ctrl_polyarchy[["gmm"]], "Electoral Democracy", "System~GMM"),
  combine_one(result_ctrl_polyarchy[["gmm-regime"]], naive_ctrl_polyarchy[["gmm-regime"]], "Electoral Democracy", "System~GMM (regime)")
)

# gmm-regime rows: cautionary only -- diagnostics reject this cell.
# Window probe (draw 1; 3:5/4:6/4:8/5:8 collapsed): all estimate, but AR(2)
# rejects in ALL EIGHT cells (.004-.031). No instrument depth rescues it.
# Thus, the results of gmm-regime should not be accepted.

cat("\n====== BASE + CONTROLS: naive vs MOC ======\n")
print(result_ctrl %>%
  mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>%
  as.data.frame())

# --- DIAGNOSTICS ------------------------------
cat("\n------ system-GMM diagnostics, controls spec (naive) ------\n")
for (aMod in c(ls_mod_ctrl1[3:4], ls_mod_ctrl2[3:4])) {
  aData <- naive_p
  g <- tryCatch(eval(parse(text = aMod)), error = function(e) NULL)
  if (is.null(g)) {
    cat(substr(aMod, 6, 30), ": pgmm failed\n")
    next
  }
  s <- summary(g, robust = TRUE)
  n_inst <- dim(g$W[[1]])[2]
  n_grp <- pdim(g)$nT$n
  cat(sprintf(
    "%-30s | #inst=%d  #countries=%d (%s) | Sargan p=%.3f | AR(2) p=%.3f\n",
    substr(aMod, 6, 35), n_inst, n_grp,
    ifelse(n_inst <= n_grp, "Roodman OK", "OVER-INSTRUMENTED"),
    s$sargan$p.value, plm::mtest(g, order = 2)$p.value
  ))
}

# ############################################################################
# 7c. Discontent x Support interaction  [COMMENTED OUT -- archived generation]
# ############################################################################

# ls_iv_int <- "plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1) + plm::lag(disc_supp, 1)"
#
# ls_eq1i <- glue("libdem ~ plm::lag(libdem, 1:2) + {ls_iv_int} + {ls_ctrl}")
# ls_eq2i <- glue("polyarchy ~ plm::lag(polyarchy, 1:2) + {ls_iv_int} + {ls_ctrl}")
#
# ls_mod_int1 <- c(
#   glue("plm({ls_eq1i}, model = 'pooling', data = aData)"),
#   glue("pgmm({ls_eq1i} | plm::lag(libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
# )
# ls_mod_int2 <- c(
#   glue("plm({ls_eq2i}, model = 'pooling', data = aData)"),
#   glue("pgmm({ls_eq2i} | plm::lag(polyarchy, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
# )
#
# #Run the model
# set.seed(313)
# result_int_libdem <- list(
#   pooled = methodComposition1(data_plm, ls_mod_int1[1]),
#   gmm    = methodComposition2(data_plm, ls_mod_int1[2]))
# result_int_polyarchy <- list(
#   pooled = methodComposition1(data_plm, ls_mod_int2[1]),
#   gmm    = methodComposition2(data_plm, ls_mod_int2[2]))
# saveRDS(result_int_libdem,    here("output", "results_intglv_libdem.rds"))
# saveRDS(result_int_polyarchy, here("output", "results_intglv_polyarchy.rds"))
#
# # Naive
# naive_int_libdem <- list(
#   pooled = naive_fit(ls_mod_int1[1]),
#   gmm    = naive_fit(ls_mod_int1[2], gmm = TRUE))
# naive_int_polyarchy <- list(
#   pooled = naive_fit(ls_mod_int2[1]),
#   gmm    = naive_fit(ls_mod_int2[2], gmm = TRUE))
# saveRDS(naive_int_libdem,    here("output", "results_intglv_libdem_naive.rds"))
# saveRDS(naive_int_polyarchy, here("output", "results_intglv_polyarchy_naive.rds"))
#
# # Present: focal coefficients, naive vs MOC ----------------------------------
# result_int <- bind_rows(
#   combine_one(result_int_libdem[["pooled"]],    naive_int_libdem[["pooled"]],    "Liberal Democracy",   "Pooled~OLS"),
#   combine_one(result_int_libdem[["gmm"]],       naive_int_libdem[["gmm"]],       "Liberal Democracy",   "System~GMM"),
#   combine_one(result_int_polyarchy[["pooled"]], naive_int_polyarchy[["pooled"]], "Electoral Democracy", "Pooled~OLS"),
#   combine_one(result_int_polyarchy[["gmm"]],    naive_int_polyarchy[["gmm"]],    "Electoral Democracy", "System~GMM")
# )
# cat("\n====== DISCONTENT x SUPPORT (full spec): naive vs MOC ======\n")
# cat("Prediction: interaction POSITIVE (support buffers discontent's harm).\n\n")
# print(result_int %>%
#         filter(grepl("disc_trim|supp_trim|disc_supp", term)) %>%
#         mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>%
#         as.data.frame())
#
# # Marginal effect of discontent at low/high support (from MOC draws) --------
# # product is mean-centered -> beta_disc is already the at-mean effect;
# # ME at supp = mean -/+ 1SD is beta_disc -/+ sd * beta_int
# sd_s <- sd(naive_dat$supp_trim, na.rm = TRUE)
# TD <- "plm::lag(disc_trim, 1)"; TI <- "plm::lag(disc_supp, 1)"
# moc_me <- function(moc, nm) {
#   lo <- moc[[TD]] - sd_s * moc[[TI]]
#   hi <- moc[[TD]] + sd_s * moc[[TI]]
#   tibble(model = nm,
#          quantity = c("disc @ LOW support (-1SD)", "disc @ HIGH support (+1SD)"),
#          moc_est = signif(c(mean(lo), mean(hi)), 3),
#          moc_se  = signif(c(sd(lo),  sd(hi)),  3),
#          moc_p   = round(2 * (1 - pnorm(abs(c(mean(lo)/sd(lo), mean(hi)/sd(hi))))), 3))
# }
# out_me <- bind_rows(
#   moc_me(result_int_libdem[["pooled"]],    "libdem pooled"),
#   moc_me(result_int_libdem[["gmm"]],       "libdem gmm"),
#   moc_me(result_int_polyarchy[["pooled"]], "polyarchy pooled"),
#   moc_me(result_int_polyarchy[["gmm"]],    "polyarchy gmm")
# )
# cat("\n--- marginal effect of discontent by support level (MOC draws) ---\n")
# print(out_me %>% mutate(sig = star(moc_p)) %>% as.data.frame())


# ############################################################################
# 8. Base hazard model (without controls) -- DV = regime-change events
#    Discrete-time (grouped-duration) hazard logit. The LDV dynamic panel does NOT apply to a binary rare event:
#    persistence is handled by the ert_regime_age duration baseline, not lagged DVs.
#    THREE event families, broadest to narrowest in a different sense:
#      (a) RoW 4-category change (ert_any_change/down/up; hand-coded, lead-coded --
#          same 'final year of regime' convention as ERT's reg_trans)
#      (b) ERT reg_trans (ert_dem_break/ert_democratization; dem/aut boundary only)
#      (c) ERT episode ONSET (ert_any_onset; first year of a substantial
#          sustained EDI movement -- sits between continuous erosion (sec. 7)
#          and regime change; risk set drops ongoing-episode years)
# ############################################################################

# SEVERITY LADDER x SAMPLE: three direction-neutral outcomes, three risk sets.
#   outcomes (mild -> severe):
#     ert_any_onset        = an ERT episode (either direction) BEGINS   [process starts]
#     ert_any_change       = RoW 4-category classification CHANGES      [category slips]
#     ert_any_regime_trans = dem/aut boundary crossed                   [regime changes]
#   samples: all / democracies (v2x_regime >= 2) / autocracies (< 2)

ls_haz <- list(
  # (1) episode onset -- risk set: outside any ongoing episode
  # onset events stay at the episode's first year; IVs are LAGGED (_l) so opinion at t-1 precedes movement during t
  `onset (all)` = list(
    f = ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_any_risk == 1)
  ),
  `onset (democracies)` = list(
    f = ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_any_risk == 1 & v2x_regime >= 2)
  ),
  `onset (autocracies)` = list(
    f = ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_any_risk == 1 & v2x_regime < 2)
  ),

  # (1b) DIRECTION-SPECIFIC onsets:
  # liberalization onset in autocracies = the direct "discontent starts movement TOWARD democracy" test
  # erosion onset in democracies = the direct doom-narrative test.
  `liberalization onset (autocracies)` = list(
    f = ert_dem_onset ~ disc_trim_lag1 + supp_trim_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_dem_risk == 1 & v2x_regime < 2)
  ),
  `erosion onset (democracies)` = list(
    f = ert_aut_onset ~ disc_trim_lag1 + supp_trim_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_aut_risk == 1 & v2x_regime >= 2)
  ),

  # (2) RoW category change
  `RoW change (all)` = list(f = ert_any_change ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = NULL),
  `RoW change (democracies)` = list(f = ert_any_change ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = quote(v2x_regime >= 2)),
  `RoW change (autocracies)` = list(f = ert_any_change ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = quote(v2x_regime < 2)),

  # (3) regime transition proper
  `regime transition (all)` = list(f = ert_any_regime_trans ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = NULL),
  `regime transition (democracies)` = list(f = ert_any_regime_trans ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = quote(v2x_regime >= 2)),
  `regime transition (autocracies)` = list(f = ert_any_regime_trans ~ disc_trim + supp_trim + ert_regime_age + I(ert_regime_age^2), sub = quote(v2x_regime < 2)),

  # (4) REGIME-SPLIT IVs in ONE pooled hazard
  # Regime-specific opinion slopes estimated jointly on the FULL risk set
  # Democracies and autocracies share one baseline hazard while vdem_democracy main effect absorbs the regime-level hazard difference.
  `onset (split IVs)` = list(
    f = ert_any_onset ~ disc_dem_trim_lag1 + disc_aut_trim_lag1 +
      supp_dem_trim_lag1 + supp_aut_trim_lag1 + vdem_democracy_lag1 + ert_regime_age + I(ert_regime_age^2),
    sub = quote(ert_any_risk == 1)
  ),
  `RoW change (split IVs)` = list(
    f = ert_any_change ~ disc_dem_trim + disc_aut_trim + supp_dem_trim +
      supp_aut_trim + vdem_democracy + ert_regime_age + I(ert_regime_age^2),
    sub = NULL
  ),
  `regime transition (split IVs)` = list(
    f = ert_any_regime_trans ~ disc_dem_trim + disc_aut_trim + supp_dem_trim +
      supp_aut_trim + vdem_democracy + ert_regime_age + I(ert_regime_age^2),
    sub = NULL
  )
)
METHODS <- c("glm", "firth")

# naive_haz: one fit on the posterior-mean data.
naive_haz <- function(spec, method = c("glm", "firth")) {
  method <- match.arg(method)
  d <- naive_dat
  if (!is.null(spec$sub)) d <- d[which(with(d, eval(spec$sub))), ]
  d <- d[complete.cases(d[, all.vars(spec$f)]), ]
  if (method == "firth") {
    m <- logistf::logistf(spec$f,
      data = d, pl = FALSE,
      control = logistf::logistf.control(maxit = 200)
    )
    V <- vcov(m)
  } else {
    m <- glm(spec$f, data = d, family = binomial())
    V <- sandwich::vcovCL(m, cluster = d$iso3c)
  }
  tibble(
    term = names(coef(m)), naive_est = as.numeric(coef(m)),
    naive_se = sqrt(diag(V)), nobs = nrow(d),
    n_events = sum(d[[all.vars(spec$f)[1]]])
  )
}

# --- ESTIMATE & SAVE [Ran 2026-07-29; uncomment to re-run] ------
set.seed(313)
moc_haz <- list()
for (nm in names(ls_haz)) {
  s <- ls_haz[[nm]]
  # pre-subset the 900 draws once per spec (shared by both estimators)
  d_sub <- if (is.null(s$sub)) data else
      lapply(data, function(d) d[which(with(d, eval(s$sub))), ])
  for (mth in METHODS) {
    message(glue("hazard MOC: {nm} [{mth}] ..."))
    moc_haz[[glue("{nm} [{mth}]")]] <-
        mocParGLM(d_sub, s$f, method = mth, cluster_col = "iso3c")
  }
}
saveRDS(moc_haz, here("output", "results_base_hazard.rds"))
# Naive
naive_haz_results <- list()
for (nm in names(ls_haz)) for (mth in METHODS){
    naive_haz_results[[glue("{nm} [{mth}]")]] <- naive_haz(ls_haz[[nm]], mth)
}
saveRDS(naive_haz_results, here("output", "results_base_hazard_naive.rds"))

# --- PRESENT ----------
moc_haz <- readRDS(here("output", "results_base_hazard.rds"))
naive_haz_results <- readRDS(here("output", "results_base_hazard_naive.rds"))
out_haz <- imap_dfr(moc_haz, function(moc, nm) {
  MOCsumm(moc, digits = 6) %>%
    left_join(naive_haz_results[[nm]], by = "term") %>%
    filter(term %in% c(
      "disc_trim", "supp_trim", "disc_trim_lag1", "supp_trim_lag1",
      "disc_dem_trim", "disc_aut_trim", "supp_dem_trim", "supp_aut_trim",
      "disc_dd_dem_trim", "disc_dd_aut_trim", "supp_dd_dem_trim", "supp_dd_aut_trim",
      "disc_gwf_dem_trim", "disc_gwf_aut_trim", "supp_gwf_dem_trim", "supp_gwf_aut_trim",
      "disc_bmr_dem_trim", "disc_bmr_aut_trim", "supp_bmr_dem_trim", "supp_bmr_aut_trim",
      "disc_dem_trim_lag1", "disc_aut_trim_lag1",
      "supp_dem_trim_lag1", "supp_aut_trim_lag1"
    )) %>%
    transmute(
      model = sub(" \\[.*$", "", nm),
      estimator = sub("^.*\\[(.*)\\]$", "\\1", nm),
      term = term,
      naive_p = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
      naive_est = round(naive_est, 4),
      moc_p = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
      moc_est = round(estimate, 4), moc_se = round(std.error, 4),
      nobs = nobs, n_events = n_events
    ) %>%
    dplyr::select(
      model, estimator, term, naive_est, naive_p,
      moc_est, moc_se, moc_p, nobs, n_events
    )
})
cat("\n====== ERT BASE HAZARD (no controls): discontent + support ======\n")
cat("Coefficients are LOG-ODDS of a transition event (not comparable to the\n")
cat("continuous-DV coefficients in section 7).\n\n")
print(out_haz %>% mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>% as.data.frame())

# ############################################################################
# 8b. Hazard + controls
# ############################################################################

ls_haz_c <- map(ls_haz, function(s) {
  if (any(grepl("_lag1$", all.vars(s$f)))) { # any lagged-IV cell (incl. split IVs)
    s$f <- update(s$f, . ~ . + lnGDP_imp_lag1 + GDP_imp_grth_lag1 + Libdem_regUN_lag1 +
      Pr_Muslim_lag1 + Res_cp_WDI_di_lag1 + ert_any_pre_change_lag1 +
      oecd_lag1 + gini_lag1 + lnpop_lag1)
  } else {
    s$f <- update(s$f, . ~ . + lnGDP_imp + GDP_imp_grth + Libdem_regUN +
      Pr_Muslim + Res_cp_WDI_di + ert_any_pre_change +
      oecd + gini + lnpop)
  }
  s
})

# --- ESTIMATE & SAVE [Ran 2026-07-29; uncomment to re-run] ------
set.seed(313)
moc_hazc <- list()
for (nm in names(ls_haz_c)) {
  s <- ls_haz_c[[nm]]
  d_sub <- if (is.null(s$sub)) data else
      lapply(data, function(d) d[which(with(d, eval(s$sub))), ])
  for (mth in METHODS) {
    message(glue("hazard+ctrl MOC: {nm} [{mth}] ..."))
    moc_hazc[[glue("{nm} [{mth}]")]] <-
        mocParGLM(d_sub, s$f, method = mth, cluster_col = "iso3c")
  }
}
saveRDS(moc_hazc, here("output", "results_ctrlglv_hazard.rds"))
#Naive
naive_hazc_results <- list()
for (nm in names(ls_haz_c)) for (mth in METHODS){
    naive_hazc_results[[glue("{nm} [{mth}]")]] <- naive_haz(ls_haz_c[[nm]], mth)
}
saveRDS(naive_hazc_results, here("output", "results_ctrlglv_hazard_naive.rds"))

# --- PRESENT ----------
moc_hazc <- readRDS(here("output", "results_ctrlglv_hazard.rds"))
naive_hazc_results <- readRDS(here("output", "results_ctrlglv_hazard_naive.rds"))
out_hazc <- imap_dfr(moc_hazc, function(moc, nm) {
  MOCsumm(moc, digits = 6) %>%
    left_join(naive_hazc_results[[nm]], by = "term") %>%
    filter(term %in% c(
      "disc_trim", "supp_trim", "disc_trim_lag1", "supp_trim_lag1",
      "disc_dem_trim", "disc_aut_trim", "supp_dem_trim", "supp_aut_trim",
      "disc_dd_dem_trim", "disc_dd_aut_trim", "supp_dd_dem_trim", "supp_dd_aut_trim",
      "disc_gwf_dem_trim", "disc_gwf_aut_trim", "supp_gwf_dem_trim", "supp_gwf_aut_trim",
      "disc_bmr_dem_trim", "disc_bmr_aut_trim", "supp_bmr_dem_trim", "supp_bmr_aut_trim",
      "disc_dem_trim_lag1", "disc_aut_trim_lag1",
      "supp_dem_trim_lag1", "supp_aut_trim_lag1"
    )) %>%
    transmute(
      model = sub(" \\[.*$", "", nm),
      estimator = sub("^.*\\[(.*)\\]$", "\\1", nm),
      term = term,
      naive_p = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
      naive_est = signif(naive_est, 3),
      moc_p = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
      moc_est = signif(estimate, 3), moc_se = signif(std.error, 3),
      nobs = nobs, n_events = n_events
    ) %>%
    dplyr::select(
      model, estimator, term, naive_est, naive_p,
      moc_est, moc_se, moc_p, nobs, n_events
    )
})
cat("\n====== ERT HAZARD + CONTROLS: discontent + support ======\n")
cat("Log-odds coefficients; compare against the no-controls ladder.\n\n")
print(out_hazc %>% mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>% as.data.frame())

# ############################################################################
# 8c. Various-DV battery -- ACLP/DD, GWF, and BMR, the classic regime-event\ datasets of the stability literature.
#       - DD (Bjornskov-Rode update of Cheibub-Gandhi-Vreeland, to ~2025):
#         dd_demo_break (democratic breakdown; risk set DD democracies, ~54 events post-1988 pre-merge)
#         dd_democratization (democratization; DD dictatorships, ~93 events pre-merge)
#         dd_any_change (6-category DD_regime, all countries)
#
#       - GWF (gwf_all, ALL regime spells but ends 2010 -> SUPPLEMENTARY ONLY):
#         gwf_any_change (any regime failure),
#         gwf_demo_break (democracies),
#         gwf_democratization (non-democracies; ~11-12 events in the survey)
#
#       - BMR (Boix-Miller-Rosato dichotomy, 1800-2020):
#         bmr_demo_break (democratic breakdown; risk set BMR democracies)
#         bmr_democratization (democratization; BMR autocracies, ~16 events
#         Note: There is no any_change for BMR since all regimes are classified either democracy or autocracy
#
#     Duration clocks are dataset-native (dd_regime_age from DD_regime spells;
#     gwf_regime_age = gwf_duration; bmr_regime_age = democracy_duration).
# ############################################################################

RHS_BASE_C <- "disc_trim + supp_trim"
RHS_CTRL_C0 <- "disc_trim + supp_trim + lnGDP_imp + GDP_imp_grth + Libdem_regUN + Pr_Muslim + Res_cp_WDI_di + oecd + gini + lnpop"
RHS_CTRL_C0X <- "lnGDP_imp + GDP_imp_grth + Libdem_regUN + Pr_Muslim + Res_cp_WDI_di + oecd + gini + lnpop" # controls only (split-IV cells carry their own IVs)

# transition-history control is CODING-SPECIFIC
RHS_CTRL_DD <- glue("{RHS_CTRL_C0} + dd_any_pre_change")
RHS_CTRL_GWF <- glue("{RHS_CTRL_C0} + gwf_any_pre_change")
RHS_CTRL_BMR <- glue("{RHS_CTRL_C0} + bmr_any_pre_change")

ls_canon <- list(
  # ---- DD --------------------------------------------------------------
  `DD any regime change (all) base` = list(f = as.formula(glue("dd_any_change ~ {RHS_BASE_C} + dd_regime_age + I(dd_regime_age^2)")), sub = NULL),
  `DD any regime change (all) +ctrl` = list(f = as.formula(glue("dd_any_change ~ {RHS_CTRL_DD} + dd_regime_age + I(dd_regime_age^2)")), sub = NULL),
  `DD any regime change (split IVs) base` = list(
    f = as.formula(glue("dd_any_change ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy + dd_regime_age + I(dd_regime_age^2)")),
    sub = NULL
  ),
  `DD any regime change (split IVs) +ctrl` = list(
    f = as.formula(glue("dd_any_change ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy + {RHS_CTRL_C0X} + dd_any_pre_change + dd_regime_age + I(dd_regime_age^2)")),
    sub = NULL
  ),
  
  `DD breakdown (democracies) base` = list(f = as.formula(glue("dd_demo_break ~ {RHS_BASE_C} + dd_regime_age + I(dd_regime_age^2)")), sub = quote(dd_democracy == 1)),
  `DD breakdown (democracies) +ctrl` = list(f = as.formula(glue("dd_demo_break ~ {RHS_CTRL_DD} + dd_regime_age + I(dd_regime_age^2)")), sub = quote(dd_democracy == 1)),
  `DD democratization (autocracies) base` = list(f = as.formula(glue("dd_democratization ~ {RHS_BASE_C} + dd_regime_age + I(dd_regime_age^2)")), sub = quote(dd_democracy == 0)),
  `DD democratization (autocracies) +ctrl` = list(f = as.formula(glue("dd_democratization ~ {RHS_CTRL_DD} + dd_regime_age + I(dd_regime_age^2)")), sub = quote(dd_democracy == 0)),

  `DD crossing (split IVs) base` = list(
    f = as.formula(glue("dd_cross ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy + dd_regime_age + I(dd_regime_age^2)")),
    sub = NULL
  ),
  `DD crossing (split IVs) +ctrl` = list(
    f = as.formula(glue("dd_cross ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy + {RHS_CTRL_C0X} + dd_any_pre_change + dd_regime_age + I(dd_regime_age^2)")),
    sub = NULL
  ),
  # crossing (all): single pooled opinion slope (constrained beta_dem = beta_aut),
  # the canonical-coding analogue of the ladder's `regime transition (all)` cell
  `DD crossing (all) +ctrl` = list(
    f = as.formula(glue("dd_cross ~ {RHS_CTRL_DD} + dd_democracy + dd_regime_age + I(dd_regime_age^2)")),
    sub = NULL
  ),

  # ---- GWF (16 boundary events -> exploratory only) --------------------
  `GWF any regime change (all) base` = list(f = as.formula(glue("gwf_any_change ~ {RHS_BASE_C} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(!is.na(gwf_any_change))),
  `GWF any regime change (all) +ctrl` = list(f = as.formula(glue("gwf_any_change ~ {RHS_CTRL_GWF} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(!is.na(gwf_any_change))),
  `GWF any regime change (split IVs) base` = list(
    f = as.formula(glue("gwf_any_change ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy + gwf_regime_age + I(gwf_regime_age^2)")),
    sub = quote(!is.na(gwf_any_change))
  ),
  `GWF any change (split IVs) +ctrl` = list(
    f = as.formula(glue("gwf_any_change ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy + {RHS_CTRL_C0X} + gwf_any_pre_change + gwf_regime_age + I(gwf_regime_age^2)")),
    sub = quote(!is.na(gwf_any_change))
  ),
  
  `GWF breakdown (democracies) base` = list(f = as.formula(glue("gwf_demo_break ~ {RHS_BASE_C} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(gwf_democracy == 1)),
  `GWF breakdown (democracies) +ctrl` = list(f = as.formula(glue("gwf_demo_break ~ {RHS_CTRL_GWF} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(gwf_democracy == 1)),
  `GWF democratization (non-democracies) base` = list(f = as.formula(glue("gwf_democratization ~ {RHS_BASE_C} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(gwf_democracy == 0)),
  `GWF democratization (non-democracies) +ctrl` = list(f = as.formula(glue("gwf_democratization ~ {RHS_CTRL_GWF} + gwf_regime_age + I(gwf_regime_age^2)")), sub = quote(gwf_democracy == 0)),
  
  `GWF crossing (split IVs) base` = list(
    f = as.formula(glue("gwf_cross ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy + gwf_regime_age + I(gwf_regime_age^2)")),
    sub = NULL
  ),
  `GWF crossing (split IVs) +ctrl` = list(
    f = as.formula(glue("gwf_cross ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy + {RHS_CTRL_C0X} + gwf_any_pre_change + gwf_regime_age + I(gwf_regime_age^2)")),
    sub = NULL
  ),
  `GWF crossing (all) +ctrl` = list(
    f = as.formula(glue("gwf_cross ~ {RHS_CTRL_GWF} + gwf_democracy + gwf_regime_age + I(gwf_regime_age^2)")),
    sub = NULL
  ),

  # ---- BMR -------------------------------------------------------------
  `BMR breakdown (democracies) base` = list(f = as.formula(glue("bmr_demo_break ~ {RHS_BASE_C} + bmr_regime_age + I(bmr_regime_age^2)")), sub = quote(bmr_democracy == 1)),
  `BMR breakdown (democracies) +ctrl` = list(f = as.formula(glue("bmr_demo_break ~ {RHS_CTRL_BMR} + bmr_regime_age + I(bmr_regime_age^2)")), sub = quote(bmr_democracy == 1)),
  `BMR democratization (autocracies) base` = list(f = as.formula(glue("bmr_democratization ~ {RHS_BASE_C} + bmr_regime_age + I(bmr_regime_age^2)")), sub = quote(bmr_democracy == 0)),
  `BMR democratization (autocracies) +ctrl` = list(f = as.formula(glue("bmr_democratization ~ {RHS_CTRL_BMR} + bmr_regime_age + I(bmr_regime_age^2)")), sub = quote(bmr_democracy == 0)),
  `BMR crossing (split IVs) base` = list(
    f = as.formula(glue("bmr_cross ~ disc_bmr_dem_trim + disc_bmr_aut_trim + supp_bmr_dem_trim + supp_bmr_aut_trim + bmr_democracy + bmr_regime_age + I(bmr_regime_age^2)")),
    sub = NULL
  ),
  `BMR crossing (split IVs) +ctrl` = list(
    f = as.formula(glue("bmr_cross ~ disc_bmr_dem_trim + disc_bmr_aut_trim + supp_bmr_dem_trim + supp_bmr_aut_trim + bmr_democracy + {RHS_CTRL_C0X} + bmr_any_pre_change + bmr_regime_age + I(bmr_regime_age^2)")),
    sub = NULL
  ),
  `BMR crossing (all) +ctrl` = list(
    f = as.formula(glue("bmr_cross ~ {RHS_CTRL_BMR} + bmr_democracy + bmr_regime_age + I(bmr_regime_age^2)")),
    sub = NULL
  )
)

# --- ESTIMATE & SAVE [Ran 2026-07-29; uncomment to re-run] ------
set.seed(313)
moc_canon <- list()
for (nm in names(ls_canon)) {
    s <- ls_canon[[nm]]
    d_sub <- if (is.null(s$sub)) data else
        lapply(data, function(d) d[which(with(d, eval(s$sub))), ])
    for (mth in METHODS) {
        message(glue("canonical MOC: {nm} [{mth}] ..."))
        moc_canon[[glue("{nm} [{mth}]")]] <-
            mocParGLM(d_sub, s$f, method = mth, cluster_col = "iso3c")
    }
}
saveRDS(moc_canon, here("output", "results_canonicalglv_hazard.rds"))
#Naive
naive_canon <- list()
for (nm in names(ls_canon)) for (mth in METHODS){
    naive_canon[[glue("{nm} [{mth}]")]] <- naive_haz(ls_canon[[nm]], mth)
}
saveRDS(naive_canon, here("output", "results_canonicalglv_hazard_naive.rds"))

# --- PRESENT----------
star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))) # redefined so this block runs standalone
moc_canon <- readRDS(here("output", "results_canonicalglv_hazard.rds"))
naive_canon <- readRDS(here("output", "results_canonicalglv_hazard_naive.rds"))
out_canon <- imap_dfr(moc_canon, function(moc, nm) {
  MOCsumm(moc, digits = 6) %>%
    left_join(naive_canon[[nm]], by = "term") %>%
    filter(term %in% c(
      "disc_trim", "supp_trim", "disc_trim_lag1", "supp_trim_lag1",
      "disc_dem_trim", "disc_aut_trim", "supp_dem_trim", "supp_aut_trim",
      "disc_dd_dem_trim", "disc_dd_aut_trim", "supp_dd_dem_trim", "supp_dd_aut_trim",
      "disc_gwf_dem_trim", "disc_gwf_aut_trim", "supp_gwf_dem_trim", "supp_gwf_aut_trim",
      "disc_bmr_dem_trim", "disc_bmr_aut_trim", "supp_bmr_dem_trim", "supp_bmr_aut_trim",
      "disc_dem_trim_lag1", "disc_aut_trim_lag1",
      "supp_dem_trim_lag1", "supp_aut_trim_lag1"
    )) %>%
    transmute(
      model = sub(" \\[.*$", "", nm),
      estimator = sub("^.*\\[(.*)\\]$", "\\1", nm),
      term = term,
      naive_p = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
      naive_est = signif(naive_est, 3),
      moc_p = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
      moc_est = signif(estimate, 3), moc_se = signif(std.error, 3),
      nobs = nobs, n_events = n_events
    ) %>%
    dplyr::select(
      model, estimator, term, naive_est, naive_p,
      moc_est, moc_se, moc_p, nobs, n_events
    )
})
cat("\n====== Various DVs (DD + GWF + BMR): discontent + support ======\n")
cat("Log-odds; DD = Bjornskov-Rode update of Cheibub-Gandhi-Vreeland;\n")
cat("BMR = Boix-Miller-Rosato; GWF supplementary (thin survey overlap).\n\n")
print(out_canon %>% mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>% as.data.frame())


# ############################################################################
# 8d. Hazard INTERACTION models (disc x supp) + controls -- completes the
#     battery symmetry with the dynamic panel (sec. 7c). Uses the RAW trims
#     and the PRECOMPUTED double-centered product disc_supp (sec. 5 note).
#     Specs: all 9 ladder cells (from ls_haz_c) + the 6 canonical +ctrl cells,
#     each glm + Firth. EPV WARNING: rare cells now carry ~13 parameters
#     (e.g. regime transition dem: 29 events -> EPV ~2.2); read Firth and glm
#     jointly and treat rare-cell interactions as exploratory.
#     Marginal effects of discontent at -1SD/+1SD support are computed from
#     the MOC coefficient draws, as in sec. 7c.
# ############################################################################

# swap_int <- function(s) {
#   if ("disc_trim_lag1" %in% all.vars(s$f)) {
#     # onset cells use lagged IVs -> lagged interaction term
#     s$f <- update(s$f, . ~ . + disc_supp_lag1)
#   } else {
#     s$f <- update(s$f, . ~ . + disc_supp)
#   }
#   s
# }
# ls_int_haz <- c(
#   map(ls_haz_c, swap_int),
#   map(ls_canon[grepl("\\+ctrl", names(ls_canon))], swap_int)
# )
#
# # naive: every spec x estimator ---------------------------------------------
# naive_inthaz <- list()
# for (nm in names(ls_int_haz)) for (mth in METHODS)
#   naive_inthaz[[glue("{nm} [{mth}]")]] <- naive_haz(ls_int_haz[[nm]], mth)
# saveRDS(naive_inthaz, here("output", "results_inthaz_glv_naive.rds"))
#
# # MOC over 900 draws --------------------------------------------------------
# set.seed(313)
# moc_inthaz <- list()
# for (nm in names(ls_int_haz)) {
#   s <- ls_int_haz[[nm]]
#   d_sub <- if (is.null(s$sub)) data else
#       lapply(data, function(d) d[which(with(d, eval(s$sub))), ])
#   for (mth in METHODS) {
#     message(glue("hazard-interaction MOC: {nm} [{mth}] ..."))
#     moc_inthaz[[glue("{nm} [{mth}]")]] <-
#         mocParGLM(d_sub, s$f, method = mth, cluster_col = "iso3c")
#   }
# }
# saveRDS(moc_inthaz, here("output", "results_inthaz_glv.rds"))
#
# # present: focal terms + marginal effects -----------------------------------
# star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
# moc_inthaz   <- readRDS(here("output", "results_inthaz_glv.rds"))
# naive_inthaz <- readRDS(here("output", "results_inthaz_glv_naive.rds"))
# out_inthaz <- imap_dfr(moc_inthaz, function(moc, nm) {
#   MOCsumm(moc, digits = 6) %>%
#     left_join(naive_inthaz[[nm]], by = "term") %>%
#     filter(term %in% c("disc_trim", "supp_trim", "disc_supp",
#                        "disc_trim_lag1", "supp_trim_lag1", "disc_supp_lag1")) %>%
#     transmute(model     = sub(" \\[.*$", "", nm),
#               estimator = sub("^.*\\[(.*)\\]$", "\\1", nm),
#               term = term,
#               naive_p   = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
#               naive_est = signif(naive_est, 3),
#               moc_p     = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
#               moc_est   = signif(estimate, 3), moc_se = signif(std.error, 3),
#               nobs = nobs, n_events = n_events) %>%
#     dplyr::select(model, estimator, term, naive_est, naive_p,
#                   moc_est, moc_se, moc_p, nobs, n_events)
# })
# cat("\n====== HAZARD INTERACTION (disc x supp, +controls): naive vs MOC ======\n")
# cat("Prediction (critical citizens): in democracies the interaction NEGATIVE\n")
# cat("for breakdown-type events (support buffers); log-odds coefficients.\n\n")
# print(out_inthaz %>% mutate(naive_sig = star(naive_p), moc_sig = star(moc_p)) %>% as.data.frame())
#
# # marginal effect of discontent at low/high support (MOC draws) -------------
# # product is mean-centered -> beta_disc is already the at-mean effect;
# # ME at supp = mean -/+ 1SD is beta_disc -/+ sd * beta_int
# sd_s2 <- sd(naive_dat$supp_trim, na.rm = TRUE)
# moc_me_haz <- function(moc, nm) {
#   # onset cells store lagged coefficient names
#   dcol <- if ("disc_trim_lag1" %in% names(moc)) "disc_trim_lag1" else "disc_trim"
#   icol <- if ("disc_supp_lag1" %in% names(moc)) "disc_supp_lag1" else "disc_supp"
#   lo <- moc[[dcol]] - sd_s2 * moc[[icol]]
#   hi <- moc[[dcol]] + sd_s2 * moc[[icol]]
#   tibble(model = nm,
#          quantity = c("disc @ LOW supp (-1SD)", "disc @ HIGH supp (+1SD)"),
#          moc_est = signif(c(mean(lo), mean(hi)), 3),
#          moc_se  = signif(c(sd(lo),  sd(hi)),  3),
#          moc_p   = round(2 * (1 - pnorm(abs(c(mean(lo)/sd(lo), mean(hi)/sd(hi))))), 3))
# }
# out_me_haz <- imap_dfr(moc_inthaz, moc_me_haz)
# cat("\n--- marginal effect of discontent by support level (MOC draws) ---\n")
# print(out_me_haz %>% mutate(sig = star(moc_p)) %>% as.data.frame())

# ============================================================================
# 9. Combine Results -- one table of ALL main (controls) models:
#    7b dynamic panel (+ctrl)
#    8b ERT hazard ladder (+ctrl)
#    8c canonical battery (+ctrl cells only)
#    Reads the saved rds -> can be run standalone after a full run.
# ============================================================================

star9 <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .01, "***",
    ifelse(p < .05, "**", ifelse(p < .1, "+", ""))
  ))
}
moc_summ9 <- function(md) {
  tibble(
    term = names(md),
    moc_est = sapply(md, mean), moc_se = sapply(md, sd)
  ) %>%
    mutate(moc_p = 2 * (1 - pnorm(abs(moc_est / moc_se)))) %>%
    filter(!grepl("rsq", term))
}
FOCAL9 <- c(
  "disc_trim", "supp_trim", "disc_trim_lag1", "supp_trim_lag1",
  "disc_dem_trim", "disc_aut_trim", "supp_dem_trim", "supp_aut_trim",
  "disc_dem_trim_lag1", "disc_aut_trim_lag1",
  "supp_dem_trim_lag1", "supp_aut_trim_lag1",
  "disc_dd_dem_trim", "disc_dd_aut_trim", "supp_dd_dem_trim", "supp_dd_aut_trim",
  "disc_gwf_dem_trim", "disc_gwf_aut_trim", "supp_gwf_dem_trim", "supp_gwf_aut_trim",
  "disc_bmr_dem_trim", "disc_bmr_aut_trim", "supp_bmr_dem_trim", "supp_bmr_aut_trim",
  "plm::lag(disc_trim, 1)", "plm::lag(supp_trim, 1)",
  "plm::lag(disc_dem_trim, 1)", "plm::lag(disc_aut_trim, 1)",
  "plm::lag(supp_dem_trim, 1)", "plm::lag(supp_aut_trim, 1)"
)

# --- 7b dynamic panel (+ctrl): pooled / pooled-regime / gmm ----------------
dyn9 <- purrr::imap_dfr(
  list(`Liberal Democracy` = "libdem", `Electoral Democracy` = "polyarchy"),
  function(dv, dv_lab) {
    moc <- readRDS(here("output", glue("results_ctrlglv_{dv}.rds")))
    nv <- readRDS(here("output", glue("results_ctrlglv_{dv}_naive.rds")))
    purrr::imap_dfr(moc, function(md, est) {
      m <- moc_summ9(as_tibble(md)) %>% filter(term %in% FOCAL9)
      n <- nv[[est]] %>%
        filter(term %in% m$term) %>%
        mutate(naive_p = 2 * (1 - pnorm(abs(naive_est / naive_se)))) %>%
        dplyr::select(term, naive_est, naive_p)
      left_join(m, n, by = "term") %>%
        mutate(
          block = "7b dynamic (+ctrl)",
          model = glue("{dv_lab} [{est}]"), n_events = NA_integer_
        )
    })
  }
)

# --- 8b + 8c hazard cells --------------------------------------------------
haz9_one <- function(moc_f, nv_f, block, keep = NULL) {
  moc <- readRDS(here("output", moc_f))
  nv <- readRDS(here("output", nv_f))
  if (!is.null(keep)) moc <- moc[grepl(keep, names(moc))]
  purrr::imap_dfr(moc, function(md, nm) {
    m <- moc_summ9(as_tibble(md)) %>% filter(term %in% FOCAL9)
    n <- nv[[nm]] %>%
      filter(term %in% m$term) %>%
      mutate(naive_p = 2 * (1 - pnorm(abs(naive_est / naive_se)))) %>%
      dplyr::select(term, naive_est, naive_p, n_events)
    left_join(m, n, by = "term") %>% mutate(block = block, model = nm)
  })
}
haz9 <- bind_rows(
  haz9_one(
    "results_ctrlglv_hazard.rds", "results_ctrlglv_hazard_naive.rds",
    "8b ERT ladder (+ctrl)"
  ),
  haz9_one("results_canonicalglv_hazard.rds", "results_canonicalglv_hazard_naive.rds",
    "8c canonical (+ctrl)",
    keep = "\\+ctrl"
  )
)

main_results <- bind_rows(dyn9, haz9) %>%
  transmute(block, model,
    term = gsub(", 1)", "", gsub("plm::lag(", "", term, fixed = TRUE), fixed = TRUE),
    naive_est = signif(naive_est, 3), naive_p = round(naive_p, 3),
    naive_sig = star9(naive_p),
    moc_est = signif(moc_est, 3), moc_se = signif(moc_se, 3),
    moc_p = round(moc_p, 3), moc_sig = star9(moc_p),
    n_events
  ) %>%
  # group by VARIABLE first (all discontent rows, then all support rows);
  # arrange() is stable, so block/model order is preserved within each group
  mutate(variable = ifelse(grepl("^disc", term), "discontent", "support")) %>%
  arrange(variable)

saveRDS(main_results, here("output", "results_main_combined.rds"))
write.csv(main_results, here("output", "results_main_combined.csv"),
  row.names = FALSE
)

cat("\n====== 9. MAIN MODELS (controls generation only): naive vs MOC ======\n")
for (v in c("discontent", "support")) {
  cat("\n------------------", toupper(v), "------------------\n")
  print(as.data.frame(main_results %>% filter(variable == v) %>% dplyr::select(-variable)),
    row.names = FALSE, max = 1e5
  )
}

