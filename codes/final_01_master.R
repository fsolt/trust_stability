##############################################################################
# Political Discontent and Regime (In)stability
# Authors: Choi, Ma, Tai, Hu, Solt
# Input:  data/theta_results.rda (PPD discontent, 900 draws);
#         data/supdem_exp_dcpo_theta.rds (democratic support draws);
#         data/supdem_exp_ajps_cntrl_list.rds (control draws);
#         data-raw/swiid9_8/ (gini); data-raw/wb_population/ (population);
#         vdemdata / ERT / democracyData packages (regime DVs)
# Output: output/results_ctrlglv_{libdem,polyarchy,hazard}[_naive].rds;
#         output/results_canonicalglv_hazard[_naive].rds;
#         output/results_main_combined.{rds,csv}
#         output/naive_dat.rds
##############################################################################

##############################################################################
# 0.  SETUP
##############################################################################
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

##############################################################################
# 1.  DATA SOURCES & VARIABLE INVENTORY
##############################################################################
# (a) DISCONTENT IV -- DCPO latent public political discontent (PPD), 0-100.
#     data/discontent_theta_results.rda; 900 draws
#     First-survey trim years from data/discontent_dcpo_input_raw.csv.
#
# (b) SUPPORT IV -- DCPO democratic-support mood, 0-100.
#     data/supdem_exp_dcpo_theta.rds; 900 draws
#
# (c) DVs.
#     - V-Dem LEVELS (sec. 7 track): v2x_libdem / v2x_polyarchy -- annual 0-100 democracy-quality indices
#
#     - ERT / RoW (V-Dem Episodes of Regime Transformation):
#       ONSET:
#           ert_any_onset       = 1 in the FIRST year of an ERT episode of either direction
#           ert_dem_onset       = liberalization-episode onset
#           ert_aut_onset       = autocratization-episode onset
#
#       CHANGE between ROW 4 categories:
#           row_any_change      = last year before any change in RoW 4-category class
#           row_auto_trans      = last democracy year before autocracy
#           row_demo_trans      = last autocracy year before democracy
#           row_any_trans= either boundary crossing
#
#   #Supplementary DVs
#     - DD / ACLP (democracyData::pacl_update, Bjornskov-Rode update to ~2025):
#       dd_any_change       = last year before any change in the 6-category DD_regime
#       dd_auto_trans       = last democracy year before non-democracy
#       dd_demo_trans       = last non-democracy year before democracy
#       dd_any_trans        = dd_auto_trans OR dd_demo_trans
#
#     - GWF (democracyData::gwf_all; ends 2010 -> supplementary/exploratory):
#       gwf_any_change      = last year before any change in regime of GWF
#       gwf_auto_trans      = last democracy year before non-democracy
#       gwf_demo_trans      = last non-democracy year before democracy
#       gwf_any_trans       = gwf_auto_trans or gwf_demo_trans
#
#     - BMR (democracyData::bmr, Boix-Miller-Rosato dichotomy 1800-2020):
#       bmr_auto_trans      = last democracy year before non-democracy
#       bmr_demo_trans      = last non-democracy year before democracy
#       bmr_any_trans       = either bmr_auto_trans or bmr_demo_trans
#
# (d) CONTROLS -- nine covariates in every full-spec model, plus the duration clock.
#     1) lnGDP_imp      log GDP per capita. (Tai et al.)
#     2) GDP_imp_grth   annual GDPpc growth rate (Tai et al.)
#     3) Libdem_regUN   mean V-Dem liberal-democracy level in the country's UN region (Tai et al.)
#     4) Pr_Muslim      Muslim population percent (Tai et al.)
#     5) Res_cp_WDI_di  natural-resource dependence dummy (Tai et al.)
#     6) *_any_pre_change  Num of past transitions before the current regime spell for each DV's coding of spells
#     7) oecd           OECD membership dummy
#     8) gini           net (disposable) income inequality, (SWIID 9.8)
#     9) lnpop          log total population, WB SP.POP.TOTL
#     10) *_regime_age (and its square): the discrete-time hazard's duration baseline
#
##############################################################################
# 2.  LOAD OPINION IVs (900 posterior draws + first-survey trims)
##############################################################################
#Discontent
load(here("data", "discontent_theta_results.rda")) 
load(here("data", "discontent_theta_summary.rda"))

disc_raw <- theta_results %>% rename(discontent = theta)
disc_list <- split(disc_raw, disc_raw$draw)
set.seed(123)
disc_list <- disc_list[sample(length(disc_list), N_DRAWS)]
disc_list <- disc_list[order(as.numeric(names(disc_list)))]

#Democratic Support
supp_list <- readRDS(here("data", "supdem_exp_dcpo_theta.rds")) 
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

##############################################################################
# 3.  DVs -- REGIME OUTCOMES (V-Dem levels; ERT/RoW; DD; GWF; BMR)
##############################################################################

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
        country_id, year, v2x_regime, reg_id, reg_start_year, reg_trans, reg_type,
        dem_ep, dem_ep_id, dem_ep_start_year, dem_ep_end_year, dem_ep_outcome, dem_ep_outcome_agg,
        aut_ep, aut_ep_id, aut_ep_start_year, aut_ep_end_year, aut_ep_outcome, aut_ep_outcome_agg
        ) %>%
    mutate(iso3c = countrycode(country_id, "vdem", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(v2x_regime)) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(
        #Row-based measure
        row_spell = replace_na(as.integer(v2x_regime != dplyr::lag(v2x_regime)), 0L), # Change = 1/ Same or else = 0
        row_spell_id = cumsum(row_spell) + 1,
        row_any_pre_change = row_spell_id - 1,
        row_any_change = replace_na(as.integer(dplyr::lead(v2x_regime) != v2x_regime), 0L), # Mark the end of them regime
        row_change_down = replace_na(as.integer(dplyr::lead(v2x_regime) < v2x_regime), 0L), # Mark the end of the regime leading to a LOWER category
        row_change_up = replace_na(as.integer(dplyr::lead(v2x_regime) > v2x_regime), 0L), # Mark the end of the regime leading to a HIGHER category
        
        row_auto_trans = replace_na(as.integer(v2x_regime >= 2 & dplyr::lead(v2x_regime) <= 1), 0L), # last dem year before crossing down
        row_demo_trans = replace_na(as.integer(v2x_regime <= 1 & dplyr::lead(v2x_regime) >= 2), 0L), # last aut year before crossing up
        row_any_trans = pmax(row_auto_trans, row_demo_trans),
        
        #Regime transition based on the regime founding criteria
        reg_spell_id = reg_id,
        reg_demo_trans = replace_na(as.integer(dplyr::lead(reg_trans) ==  1), 0L),
        reg_auto_trans = replace_na(as.integer(dplyr::lead(reg_trans) == -1), 0L),
        reg_any_trans = pmax(reg_demo_trans, reg_auto_trans)
        ) %>% 
    mutate(
        reg_demo_trans = ifelse(reg_type == 1, NA_integer_, reg_demo_trans),
        reg_auto_trans = ifelse(reg_type == 0, NA_integer_, reg_auto_trans)
        ) %>% 
    mutate(
        # episode ONSET events
        ert_dem_onset = replace_na(as.integer(year == dem_ep_start_year), 0L),
        ert_aut_onset = replace_na(as.integer(year == aut_ep_start_year), 0L),
        ert_any_onset = pmax(ert_dem_onset, ert_aut_onset),
        
        # outcome-classified onsets
        ert_dem_onset_trans   = as.integer(ert_dem_onset == 1 & dem_ep_outcome_agg == 1),
        ert_dem_onset_notrans = as.integer(ert_dem_onset == 1 & dem_ep_outcome_agg == 2),
        ert_dem_onset_deepen  = as.integer(ert_dem_onset == 1 & dem_ep_outcome_agg == 3),
        ert_aut_onset_break   = as.integer(ert_aut_onset == 1 & aut_ep_outcome_agg == 1),
        ert_aut_onset_nobreak = as.integer(ert_aut_onset == 1 & aut_ep_outcome_agg == 2),
        ert_aut_onset_regress  = as.integer(ert_aut_onset == 1 & aut_ep_outcome_agg == 3),
        
        #Risk set
        ert_dem_risk  = as.integer(is.na(dem_ep_id) | year == dem_ep_start_year), # 1 for years outside an democratization episode (episode start year stays at risk)
        ert_aut_risk  = as.integer(is.na(aut_ep_id) | year == aut_ep_start_year), # 1 for years outside an ONGOING autocratization episode (episode start year stays at risk)
        ert_any_risk  = as.integer(ert_dem_risk == 1 & ert_aut_risk == 1),
        
        ) %>%
    mutate(reg_regime_age = year - reg_start_year + 1L,
           pre_reg_change = cumsum(replace_na(as.integer(reg_id != dplyr::lag(reg_id)), 0L))
           ) %>%
    group_by(iso3c, row_spell_id) %>%          
    mutate(row_regime_age = row_number()) %>% 
    ungroup()

# ACLP/DD dichotomous regime events -- Bjornskov-Rode update of Cheibub-Gandhi-Vreeland (coverage to ~2025, fixing the 2008 truncation of the original).
# Regime type: Parliamentary democracies = 0, Mixed democracies (with weak presidents) = 1, 
#              Presidential democracies = 2, Civilian autocracies = 3,
#              'Military dictatorships = 4, and Royal dictatorships = 5

dd_cy <- democracyData::pacl_update %>%
    mutate(iso3c = countrycode(pacl_update_country, "country.name", "iso3c")) %>%
    filter(!is.na(iso3c), !is.na(DD_regime), DD_regime <= 5, !is.na(Democracy)) %>% 
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(
        # any track: ANY regime change on the 6-category DD_regime
        dd_spell = cumsum(replace_na(as.integer(DD_regime != dplyr::lag(DD_regime)), 0L)) + 1L,
        dd_any_change = replace_na(as.integer(DD_regime != dplyr::lead(DD_regime)), 0L),
        
        # demo track: dichotomy boundary crossings
        dd_democracy = Democracy,
        dd_auto_trans = replace_na(as.integer(dd_democracy == 1 & dplyr::lead(dd_democracy) == 0), 0L),
        dd_demo_trans = replace_na(as.integer(dd_democracy == 0 & dplyr::lead(dd_democracy) == 1), 0L),
        dd_any_trans = pmax(dd_demo_trans, dd_auto_trans), # either crossing (split-IV DV)
        
        # transitions before the current regime spell (= spell ID - 1)
        dd_pre_change = dd_spell - 1L
        ) %>%
    group_by(iso3c, dd_spell) %>%
    mutate(dd_regime_age = row_number()) %>%
    ungroup() %>%
    dplyr::select(
        iso3c, year, dd_spell, dd_any_change, dd_regime_age, dd_pre_change,
        dd_democracy, dd_auto_trans, dd_demo_trans, dd_any_trans
        ) %>%
    distinct(iso3c, year, .keep_all = TRUE)

# GWF regimes -- ALL regime spells (gwf_all) (ends 2010)
# Regime type: indirect military, military, military-personal, monarchy,
#              oligarchy, party, party-military, party-military-personal,
#              party-personal, personal.
# Else is Democratic, Provisional, Foreign-occupied or warlord/occupied

gwf_cy <- democracyData::gwf_all %>%
    mutate(iso3c = countrycode(gwf_cowcode, "cown", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(
        # any track: ANY GWF regime change. gwf_fail is END-coded (includes demo, non-demo, wa)
        gwf_spell = cumsum(replace_na(as.integer(gwf_casename != dplyr::lag(gwf_casename)), 0L)) + 1L,
        gwf_any_change = replace_na(gwf_fail, 0L),
        gwf_regime_age = gwf_duration,
        gwf_pre_change = gwf_spell - 1L,
        
        # demo track: gwf_nonautocracy labels non-autocratic spells directly (democracy/provisional/warlord/occupied; NA = autocratic)
        # democracy = 1; autocracy = 0; neither demo nor auto (provisional / warlord / foreign-occupied) = NA
        gwf_democracy = dplyr::case_when(
            gwf_nonautocracy == "democracy" ~ 1L,
            is.na(gwf_nonautocracy)         ~ 0L,
            TRUE                            ~ NA_integer_
            ),
        gwf_demo_trans = replace_na(as.integer(gwf_any_change == 1 & gwf_democracy == 0 & gwf_next == "democracy"), 0L),
        gwf_auto_trans = replace_na(as.integer(gwf_any_change == 1 & gwf_democracy == 1 & gwf_next != "democracy"), 0L),
        gwf_any_trans = pmax(gwf_demo_trans, gwf_auto_trans)
        ) %>%
    ungroup() %>%
    dplyr::select(
        iso3c, year,
        gwf_spell, gwf_any_change, gwf_regime_age, gwf_pre_change,
        gwf_democracy, gwf_demo_trans, gwf_auto_trans, gwf_any_trans
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
        bmr_auto_trans = replace_na(as.integer(democracy == 1 & dplyr::lead(democracy) == 0), 0L),
        bmr_demo_trans = replace_na(as.integer(democracy == 0 & dplyr::lead(democracy) == 1), 0L),
        
        bmr_regime_age = democracy_duration,
        bmr_spell = cumsum(replace_na(as.integer(democracy != dplyr::lag(democracy)), 0L)) + 1L,
        bmr_pre_change = bmr_spell - 1L,
        bmr_any_trans = pmax(bmr_auto_trans, bmr_demo_trans)
        ) %>%
    ungroup() %>% 
    dplyr::select(
        iso3c, year, bmr_spell, bmr_regime_age, bmr_pre_change,
        bmr_democracy, bmr_demo_trans, bmr_auto_trans, bmr_any_trans
        ) %>%
    distinct(iso3c, year, .keep_all = TRUE)

##############################################################################
# 4. CONTROLS
##############################################################################
ctrl_raw <- readRDS(here("data", "supdem_exp_ajps_cntrl_list.rds"))
ctrl_key <- tibble(country = unique(ctrl_raw[[1]]$country)) %>% 
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
    filter(!is.na(iso3c))
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
    "swiid9_8.rda"
    ))) {
    download.file(
        "https://dataverse.harvard.edu/api/access/datafile/10797566",
        here("data-raw", "swiid9_8.zip")
        )
    unzip(here("data-raw", "swiid9_8.zip"), exdir = here("data-raw"))
    file.remove(here("data-raw", "swiid9_8.zip"))
    }

# SWIID
load(here("data-raw", "swiid9_8", "swiid9_8.rda")) # objects: swiid, swiid_summary
swiid_list <- map(swiid, ~ .x %>%
                      mutate(iso3c = countrycode(country, "country.name", "iso3c", warn = FALSE)) %>%
                      filter(!is.na(iso3c)) %>%
                      transmute(iso3c, year = as.integer(year), gini = gini_disp / 100) %>%
                      distinct(iso3c, year, .keep_all = TRUE))
rm(swiid, swiid_summary)

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
cd_cy <- pop_cy # population only; gini joins per draw in build_one

##############################################################################
# 5. COMBINE DATA -- build_one() x 900 draws + naive_dat
##############################################################################
build_one <- function(dsc, supp, ctrl, gini_d) {
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
            ert_cy, iso3c, year, v2x_regime, reg_type, reg_spell_id,
            row_spell_id, row_any_pre_change, row_regime_age,
            row_any_change, row_change_down, row_change_up,
            row_auto_trans, row_demo_trans, row_any_trans,
            reg_demo_trans, reg_auto_trans, reg_any_trans, reg_regime_age, pre_reg_change,
            ert_dem_onset, ert_aut_onset, ert_any_onset,
            ert_dem_onset_trans, ert_dem_onset_notrans, ert_dem_onset_deepen,
            ert_aut_onset_break, ert_aut_onset_nobreak, ert_aut_onset_regress,
            ert_dem_risk, ert_aut_risk, ert_any_risk
            ), by = c("iso3c", "year")) %>%
        left_join(ctrl, by = c("iso3c", "year")) %>% # AJPS controls
        left_join(dd_cy, by = c("iso3c", "year")) %>% # ACLP/DD events
        left_join(gwf_cy, by = c("iso3c", "year")) %>% # GWF breakdown
        left_join(bmr_cy, by = c("iso3c", "year")) %>% # BMR events
        left_join(disc_first, by = "iso3c") %>% # First year of the discontent survey
        left_join(supp_first, by = "iso3c") %>% # First year of the supdem survey
        left_join(oecd_acc, by = "iso3c") %>% # OECD control
        left_join(cd_cy, by = c("iso3c", "year")) %>% # lnpop
        left_join(gini_d, by = c("iso3c", "year")) %>% # gini (SWIID imputation for this draw)
        mutate(
            oecd = as.integer(!is.na(oecd_year) & year >= oecd_year),
            
            # first-year trim: NA before each country's first actual survey to drop DCPO backward extrapolation
            disc_trim = ifelse(!is.na(disc_fy) & year >= disc_fy, discontent, NA_real_),
            supp_trim = ifelse(!is.na(supp_fy) & year >= supp_fy, supdem, NA_real_),
            
            # regime-split trims:theta as it is in own-regime years, 0 elsewhere
            #Reg Version
            reg_democracy = reg_type,
            disc_dem_trim = case_when(
                is.na(disc_trim) | is.na(reg_democracy) ~ NA_real_,
                reg_democracy == 1 ~ disc_trim, TRUE ~ 0
            ),
            disc_aut_trim = case_when(
                is.na(disc_trim) | is.na(reg_democracy) ~ NA_real_,
                reg_democracy == 0 ~ disc_trim, TRUE ~ 0
            ),
            supp_dem_trim = case_when(
                is.na(supp_trim) | is.na(reg_democracy) ~ NA_real_,
                reg_democracy == 1 ~ supp_trim, TRUE ~ 0
            ),
            supp_aut_trim = case_when(
                is.na(supp_trim) | is.na(reg_democracy) ~ NA_real_,
                reg_democracy == 0 ~ supp_trim, TRUE ~ 0
            ),
            
            #Row Version
            vdem_democracy = as.integer(v2x_regime >= 2),
            disc_dem_trim_row = case_when(
                is.na(disc_trim) | is.na(vdem_democracy) ~ NA_real_,
                vdem_democracy == 1 ~ disc_trim, TRUE ~ 0
            ),
            disc_aut_trim_row = case_when(
                is.na(disc_trim) | is.na(vdem_democracy) ~ NA_real_,
                vdem_democracy == 0 ~ disc_trim, TRUE ~ 0
            ),
            supp_dem_trim_row = case_when(
                is.na(supp_trim) | is.na(vdem_democracy) ~ NA_real_,
                vdem_democracy == 1 ~ supp_trim, TRUE ~ 0
            ),
            supp_aut_trim_row = case_when(
                is.na(supp_trim) | is.na(vdem_democracy) ~ NA_real_,
                vdem_democracy == 0 ~ supp_trim, TRUE ~ 0
            ),
            
            # regime-split trims for each coding rule
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
                )) %>%
        # lagged IVs + CONTROLS
        group_by(iso3c) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(
            disc_trim_lag1          = dplyr::lag(disc_trim),
            supp_trim_lag1          = dplyr::lag(supp_trim),
            disc_dem_trim_lag1      = dplyr::lag(disc_dem_trim),
            disc_aut_trim_lag1      = dplyr::lag(disc_aut_trim),
            supp_dem_trim_lag1      = dplyr::lag(supp_dem_trim),
            supp_aut_trim_lag1      = dplyr::lag(supp_aut_trim),
            vdem_democracy_lag1     = dplyr::lag(vdem_democracy),
            reg_democracy_lag1      = dplyr::lag(reg_democracy),
            lnGDP_imp_lag1          = dplyr::lag(lnGDP_imp),
            GDP_imp_grth_lag1       = dplyr::lag(GDP_imp_grth),
            Libdem_regUN_lag1       = dplyr::lag(Libdem_regUN),
            Pr_Muslim_lag1          = dplyr::lag(Pr_Muslim),
            Res_cp_WDI_di_lag1      = dplyr::lag(Res_cp_WDI_di),
            row_any_pre_change_lag1 = dplyr::lag(row_any_pre_change),
            pre_reg_change_lag1     = dplyr::lag(pre_reg_change),
            oecd_lag1               = dplyr::lag(oecd),
            gini_lag1               = dplyr::lag(gini),
            lnpop_lag1              = dplyr::lag(lnpop)
            ) %>%
        ungroup()
    }

gini_idx <- ((seq_len(N_DRAWS) - 1) %% length(swiid_list)) + 1
data <- pmap(list(disc_list, supp_list, ctrl_list, swiid_list[gini_idx]), build_one)

# Dataset for naive point-estimate analysis
naive_dat <- data[[1]]

# Posterior means for every numeric SOURCE column: draw-varying columns need it, draw-constant columns average to themselves (no-op)
derived <- grep("_lag1$", names(naive_dat), value = TRUE)
for (v in setdiff(names(naive_dat)[sapply(naive_dat, is.numeric)], derived)) {
    naive_dat[[v]] <- rowMeans(vapply(data, function(d) d[[v]], numeric(nrow(naive_dat))), na.rm = TRUE)
    }

naive_dat <- naive_dat %>%
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
        disc_trim_lag1          = dplyr::lag(disc_trim),
        supp_trim_lag1          = dplyr::lag(supp_trim),
        disc_dem_trim_lag1      = dplyr::lag(disc_dem_trim),
        disc_aut_trim_lag1      = dplyr::lag(disc_aut_trim),
        supp_dem_trim_lag1      = dplyr::lag(supp_dem_trim),
        supp_aut_trim_lag1      = dplyr::lag(supp_aut_trim),
        vdem_democracy_lag1     = dplyr::lag(vdem_democracy),
        reg_democracy_lag1      = dplyr::lag(reg_democracy),
        lnGDP_imp_lag1          = dplyr::lag(lnGDP_imp),
        GDP_imp_grth_lag1       = dplyr::lag(GDP_imp_grth),
        Libdem_regUN_lag1       = dplyr::lag(Libdem_regUN),
        Pr_Muslim_lag1          = dplyr::lag(Pr_Muslim),
        Res_cp_WDI_di_lag1      = dplyr::lag(Res_cp_WDI_di),
        row_any_pre_change_lag1 = dplyr::lag(row_any_pre_change),
        pre_reg_change_lag1     = dplyr::lag(pre_reg_change),
        oecd_lag1               = dplyr::lag(oecd),
        gini_lag1               = dplyr::lag(gini),
        lnpop_lag1              = dplyr::lag(lnpop)
        ) %>%
    ungroup()

saveRDS(naive_dat, here("output", "naive_dat.rds"))

##############################################################################
# 6. ERT hazard Model
##############################################################################

RHS_CTRL_LAG <- "lnGDP_imp_lag1 + GDP_imp_grth_lag1 + Libdem_regUN_lag1 + Pr_Muslim_lag1 + Res_cp_WDI_di_lag1 + oecd_lag1 + gini_lag1 + lnpop_lag1"
RHS_CTRL_CUR <- "lnGDP_imp + GDP_imp_grth + Libdem_regUN + Pr_Muslim + Res_cp_WDI_di + oecd + gini + lnpop"
METHODS <- c("glm", "firth")

ls_haz <- list(
  # ---- main onset battery
    `onset (split IVs)` = list(
        f = as.formula(glue("ert_any_onset ~ disc_dem_trim_lag1 + disc_aut_trim_lag1 + supp_dem_trim_lag1 + supp_aut_trim_lag1 + reg_democracy_lag1 +
                            reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
        sub = quote(ert_any_risk == 1)),
    `onset for democratization (split IVs)` = list(
        f = as.formula(glue("ert_dem_onset ~ disc_dem_trim_lag1 + disc_aut_trim_lag1 + supp_dem_trim_lag1 + supp_aut_trim_lag1 + reg_democracy_lag1 + 
                            reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
        sub = quote(ert_dem_risk == 1)),
    `onset for autocratization (split IVs)` = list(
        f = as.formula(glue("ert_aut_onset ~ disc_dem_trim_lag1 + disc_aut_trim_lag1 + supp_dem_trim_lag1 + supp_aut_trim_lag1 + reg_democracy_lag1 + 
                            reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
        sub = quote(ert_aut_risk == 1)),
  
  #Pool approach (single IV)
  `onset (pool)` = list(
      f = as.formula(glue("ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_any_risk == 1)),
  `onset (in democracies)` = list(
      f = as.formula(glue("ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_any_risk == 1 & reg_democracy_lag1 == 1)),
  `onset (in autocracies)` = list(
      f = as.formula(glue("ert_any_onset ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_any_risk == 1 & reg_democracy_lag1 == 0)),

  # ---- outcome-classified onsets (single IV because of the sub-sample)
  `onset for demo trans` = list(
      f = as.formula(glue("ert_dem_onset_trans ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_dem_risk == 1 & reg_democracy_lag1 == 0)),
  `onset for demo notrans` = list(
      f = as.formula(glue("ert_dem_onset_notrans ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_dem_risk == 1 & reg_democracy_lag1 == 0)),
  `onset for demo deepen` = list(
      f = as.formula(glue("ert_dem_onset_deepen ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_dem_risk == 1 & reg_democracy_lag1 == 1)),
  
  `onset for auto trans` = list(
      f = as.formula(glue("ert_aut_onset_break ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_aut_risk == 1 & reg_democracy_lag1 == 1)),
  `onset for auto notrans` = list(
      f = as.formula(glue("ert_aut_onset_nobreak ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_aut_risk == 1 & reg_democracy_lag1 == 1)),
  `onset for auto deepen` = list(     
      f = as.formula(glue("ert_aut_onset_regress ~ disc_trim_lag1 + supp_trim_lag1 + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change_lag1 + {RHS_CTRL_LAG}")),
      sub = quote(ert_aut_risk == 1 & reg_democracy_lag1 == 0)),   
  
  # ---- Regime change (founding election; lead-coded so no lag is used)
  `reg transition (split IVs)` = list(
      f = as.formula(glue("reg_any_trans ~ disc_dem_trim + disc_aut_trim + supp_dem_trim + supp_aut_trim + reg_democracy +
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change + {RHS_CTRL_CUR}")),
      sub = NULL),
  `reg for democratization` = list(
      f = as.formula(glue("reg_demo_trans ~ disc_trim + supp_trim + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change + {RHS_CTRL_CUR}")),
      sub = quote(reg_democracy == 0)), 
  `reg for autocratization` = list(
      f = as.formula(glue("reg_auto_trans ~ disc_trim + supp_trim + 
                          reg_regime_age + I(reg_regime_age^2) + pre_reg_change + {RHS_CTRL_CUR}")),
      sub = quote(reg_democracy == 1)),
  
  # ---- RoW change
  `RoW change (split IVs)` = list(
      f = as.formula(glue("row_any_change ~ disc_dem_trim_row + disc_aut_trim_row + supp_dem_trim_row + supp_aut_trim_row + vdem_democracy + 
                          row_regime_age + I(row_regime_age^2) + row_any_pre_change + {RHS_CTRL_CUR}")),
      sub = NULL
  ),
  `RoW transition (split IVs)` = list(
      f = as.formula(glue("row_any_trans ~ disc_dem_trim_row + disc_aut_trim_row + supp_dem_trim_row + supp_aut_trim_row + 
                          vdem_democracy + row_regime_age + I(row_regime_age^2) + row_any_pre_change + {RHS_CTRL_CUR}")),
      sub = NULL
  ),
  `RoW for demo trans` = list(
      f = as.formula(glue("row_demo_trans ~ disc_trim + supp_trim + 
                          row_regime_age + I(row_regime_age^2) + row_any_pre_change + {RHS_CTRL_CUR}")),
      sub = quote(vdem_democracy == 0)
  ),
  `RoW for auto trans` = list(
      f = as.formula(glue("row_auto_trans ~ disc_trim + supp_trim + 
                          row_regime_age + I(row_regime_age^2) + row_any_pre_change + {RHS_CTRL_CUR}")),
      sub = quote(vdem_democracy == 1)
  )
)


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

# --- Run Model and Save
set.seed(313)
moc_hazc <- list(); naive_hazc_results <- list()
for (nm in names(ls_haz)) {
  s <- ls_haz[[nm]]
  d_sub <- if (is.null(s$sub)) data else
    lapply(data, function(d) d[which(with(d, eval(s$sub))), ])
  for (mth in METHODS) {
    message(glue("hazard MOC: {nm} [{mth}] ..."))
    moc_hazc[[glue("{nm} [{mth}]")]] <- mocParGLM(d_sub, s$f, method = mth, cluster_col = "iso3c")
    naive_hazc_results[[glue("{nm} [{mth}]")]] <- naive_haz(s, mth)
  }
}
saveRDS(moc_hazc, here("output", "results_ctrlglv_hazard.rds"))
saveRDS(naive_hazc_results, here("output", "results_ctrlglv_hazard_naive.rds"))

# --- SAMPLE STATISTICS for substantive-effect quantities (read by the qmd) --
# Base rates and predictor spreads in the MAIN model's exact estimation sample
# (posterior-mean data, same complete-case rule as estimation).
s <- ls_haz[["onset (split IVs)"]]
d_stat <- naive_dat[which(with(naive_dat, eval(s$sub))), ]
d_stat <- d_stat[complete.cases(d_stat[, all.vars(s$f)]), ]
aut_stat <- d_stat[d_stat$reg_democracy_lag1 == 0, ]
dem_stat <- d_stat[d_stat$reg_democracy_lag1 == 1, ]
onset_sample_stats <- list(
    n = nrow(d_stat), events = sum(d_stat$ert_any_onset),
    n_aut = nrow(aut_stat), events_aut = sum(aut_stat$ert_any_onset),
    base_aut = mean(aut_stat$ert_any_onset),
    base_dem = mean(dem_stat$ert_any_onset),
    sd_disc_aut = sd(aut_stat$disc_aut_trim_lag1),
    q_disc_aut = quantile(aut_stat$disc_aut_trim_lag1, c(.1, .5, .9)),
    sd_supp_dem = sd(dem_stat$supp_dem_trim_lag1)
)
saveRDS(onset_sample_stats, here("output", "onset_sample_stats.rds"))

# --- Presentation
moc_hazc <- readRDS(here("output", "results_ctrlglv_hazard.rds"))
naive_hazc_results <- readRDS(here("output", "results_ctrlglv_hazard_naive.rds"))
FOCAL8 <- c("disc_trim", "supp_trim", 
            "disc_trim_lag1", "supp_trim_lag1",
            "disc_dem_trim", "disc_aut_trim", "supp_dem_trim", "supp_aut_trim",
            "disc_dem_trim_lag1", "disc_aut_trim_lag1", "supp_dem_trim_lag1", "supp_aut_trim_lag1",
            "disc_dem_trim_row", "disc_aut_trim_row", "supp_dem_trim_row", "supp_aut_trim_row")

for (nm in names(moc_hazc)) {
    m <- MOCsumm(as_tibble(moc_hazc[[nm]]), digits = 6) %>%
        filter(term %in% FOCAL8) %>%
        mutate(
            estimate  = as.numeric(estimate),
            std.error = as.numeric(std.error),
            moc_p     = round(2 * (1 - pnorm(abs(estimate / std.error))), 3)
        )
    n <- naive_hazc_results[[nm]] %>%
        filter(term %in% FOCAL8) %>%
        transmute(term,
                  naive_est = round(naive_est, 4),
                  naive_p   = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3))
    m <- left_join(m, n, by = "term")
    cat("\n--", nm, "( events:", naive_hazc_results[[nm]]$n_events[1],
        "/ N:", naive_hazc_results[[nm]]$nobs[1], ")\n")
    print(as.data.frame(m), row.names = FALSE)
}

##############################################################################
# 7. Various-DV battery -- ACLP/DD, GWF, and BMR
#       - DD (Cheibub, Gandhi and Vreeland, to ~2025):
#         dd_any_change (any regime failure: 6-category DD_regime)
#         dd_auto_trans (democratic breakdown)
#         dd_demo_trans (democratization)
#         
#       - GWF (Geddes, Wright and Frantz; to ~ 2010):
#         gwf_any_change (any regime failure),
#         gwf_auto_trans (democratic breakdown),
#         gwf_demo_trans (democratization)
#
#       - BMR (Boix-Miller, and Rosato dichotomy; to ~2020):
#         bmr_auto_trans (democratic breakdown)
#         bmr_demo_trans (democratization)
##############################################################################
ls_canon <- list(
    # ---- DD: Any regime Change
    `DD any regime change (split IVs)` = list(
    f = as.formula(glue("dd_any_change ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy +
                         dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `DD any regime change (pool)` = list(
    f = as.formula(glue("dd_any_change ~ disc_trim + supp_trim +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `DD any regime change (in democracies)` = list(
    f = as.formula(glue("dd_any_change ~ disc_trim + supp_trim +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(dd_democracy == 1)),
    `DD any regime change (in autocracies)` = list(
    f = as.formula(glue("dd_any_change ~ disc_trim + supp_trim +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(dd_democracy == 0)),
    
    # ---- DD: Demo-Auto Regime Transition
    `DD transition (split IVs)` = list(
    f = as.formula(glue("dd_any_trans ~ disc_dd_dem_trim + disc_dd_aut_trim + supp_dd_dem_trim + supp_dd_aut_trim + dd_democracy +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `DD transition (pool)` = list(
    f = as.formula(glue("dd_any_trans ~ disc_trim + supp_trim + dd_democracy +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `DD for democratization` = list(
    f = as.formula(glue("dd_demo_trans ~ disc_trim + supp_trim +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(dd_democracy == 0)),
    `DD for autocratization` = list(
    f = as.formula(glue("dd_auto_trans ~ disc_trim + supp_trim +
                        dd_regime_age + I(dd_regime_age^2) + dd_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(dd_democracy == 1)),
    
    # ---- GWF: Any regime change
    `GWF any regime change (split IVs)` = list(
    f = as.formula(glue("gwf_any_change ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `GWF any regime change (pool)` = list(
    f = as.formula(glue("gwf_any_change ~ disc_trim + supp_trim +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(!is.na(gwf_democracy))),
    `GWF any regime change (in democracies)` = list(
    f = as.formula(glue("gwf_any_change ~ disc_trim + supp_trim +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(gwf_democracy == 1)),
    `GWF any regime change (in autocracies)` = list(
    f = as.formula(glue("gwf_any_change ~ disc_trim + supp_trim +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(gwf_democracy == 0)),

    # ---- GWF: Demo-Auto Regime Transition
    `GWF transition (split IVs)` = list(
    f = as.formula(glue("gwf_any_trans ~ disc_gwf_dem_trim + disc_gwf_aut_trim + supp_gwf_dem_trim + supp_gwf_aut_trim + gwf_democracy +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `GWF transition (pool)` = list(
    f = as.formula(glue("gwf_any_trans ~ disc_trim + supp_trim + gwf_democracy +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `GWF for democratization` = list(
    f = as.formula(glue("gwf_demo_trans ~ disc_trim + supp_trim +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(gwf_democracy == 0)),
    `GWF for autocratization` = list(
    f = as.formula(glue("gwf_auto_trans ~ disc_trim + supp_trim +
                        gwf_regime_age + I(gwf_regime_age^2) + gwf_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(gwf_democracy == 1)),

    # ---- BMR: Demo-Auto Regime Transition
    `BMR transition (split IVs)` = list(
    f = as.formula(glue("bmr_any_trans ~ disc_bmr_dem_trim + disc_bmr_aut_trim + supp_bmr_dem_trim + supp_bmr_aut_trim + bmr_democracy +
                        bmr_regime_age + I(bmr_regime_age^2) + bmr_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `BMR transition (pool)` = list(
    f = as.formula(glue("bmr_any_trans ~ disc_trim + supp_trim + bmr_democracy +
                        bmr_regime_age + I(bmr_regime_age^2) + bmr_pre_change + {RHS_CTRL_CUR}")),
    sub = NULL),
    `BMR for democratization` = list(
    f = as.formula(glue("bmr_demo_trans ~ disc_trim + supp_trim +
                        bmr_regime_age + I(bmr_regime_age^2) + bmr_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(bmr_democracy == 0)),
    `BMR for autocratization` = list(
    f = as.formula(glue("bmr_auto_trans ~ disc_trim + supp_trim +
                        bmr_regime_age + I(bmr_regime_age^2) + bmr_pre_change + {RHS_CTRL_CUR}")),
    sub = quote(bmr_democracy == 1))
    )

# --- Run Model and Save
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

# --- Presentation (same format as sec. 7: one block per cell, MOC vs naive)
star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))) # used by sec. 9
moc_canon <- readRDS(here("output", "results_canonicalglv_hazard.rds"))
naive_canon <- readRDS(here("output", "results_canonicalglv_hazard_naive.rds"))
FOCAL8C <- c("disc_trim", "supp_trim",
             "disc_dd_dem_trim",  "disc_dd_aut_trim",  "supp_dd_dem_trim",  "supp_dd_aut_trim",
             "disc_gwf_dem_trim", "disc_gwf_aut_trim", "supp_gwf_dem_trim", "supp_gwf_aut_trim",
             "disc_bmr_dem_trim", "disc_bmr_aut_trim", "supp_bmr_dem_trim", "supp_bmr_aut_trim")
for (nm in names(moc_canon)) {
  m <- MOCsumm(as_tibble(moc_canon[[nm]]), digits = 6) %>%
    filter(term %in% FOCAL8C) %>%
    mutate(
      estimate  = as.numeric(estimate),
      std.error = as.numeric(std.error),
      moc_p     = round(2 * (1 - pnorm(abs(estimate / std.error))), 3)
    )
  n <- naive_canon[[nm]] %>%
    filter(term %in% FOCAL8C) %>%
    transmute(term,
              naive_est = round(naive_est, 4),
              naive_p   = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3))
  m <- left_join(m, n, by = "term")
  cat("\n--", nm, "( events:", naive_canon[[nm]]$n_events[1],
      "/ N:", naive_canon[[nm]]$nobs[1], ")\n")
  print(as.data.frame(m), row.names = FALSE)
}

# ############################################################################
# 8. Dynamic panel -- Tai et al. form, full controls (V-Dem levels)
#    DV = libdem / polyarchy (continuous democracy quality).
# ############################################################################

data_plm <- purrr::map(1:N_DRAWS, function(num) {
    data[[num]] %>%
        as.data.frame() %>%
        plm::pdata.frame(index = c("iso3c", "year"))
})

RHS_CTRL_PLM <- paste(
    "plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) +",
    "plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) +",
    "plm::lag(Res_cp_WDI_di, 1) + plm::lag(row_any_pre_change, 1) +",
    "plm::lag(oecd, 1) + plm::lag(gini, 1) + plm::lag(lnpop, 1)"
)

ls_dyn <- list(
  libdem = list(
    `pooled` = glue("plm(libdem ~ plm::lag(libdem, 1:2) + plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1) +
                     {RHS_CTRL_PLM}, model = 'pooling', data = aData)"),
    `pooled-regime` = glue("plm(libdem ~ plm::lag(libdem, 1:2) + plm::lag(disc_dem_trim, 1) + plm::lag(disc_aut_trim, 1) + plm::lag(supp_dem_trim, 1) + plm::lag(supp_aut_trim, 1) +
                     {RHS_CTRL_PLM}, model = 'pooling', data = aData)"),
    `gmm` = glue("pgmm(libdem ~ plm::lag(libdem, 1:2) + plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1) +
                     {RHS_CTRL_PLM} | plm::lag(libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))"),
    `gmm-regime` = glue("pgmm(libdem ~ plm::lag(libdem, 1:2) + plm::lag(disc_dem_trim, 1) + plm::lag(disc_aut_trim, 1) + plm::lag(supp_dem_trim, 1) + plm::lag(supp_aut_trim, 1) +
                     {RHS_CTRL_PLM} | plm::lag(libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
  ),
  polyarchy = list(
    `pooled` = glue("plm(polyarchy ~ plm::lag(polyarchy, 1:2) + plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1) +
                     {RHS_CTRL_PLM}, model = 'pooling', data = aData)"),
    `pooled-regime` = glue("plm(polyarchy ~ plm::lag(polyarchy, 1:2) + plm::lag(disc_dem_trim, 1) + plm::lag(disc_aut_trim, 1) + plm::lag(supp_dem_trim, 1) + plm::lag(supp_aut_trim, 1) +
                     {RHS_CTRL_PLM}, model = 'pooling', data = aData)"),
    `gmm` = glue("pgmm(polyarchy ~ plm::lag(polyarchy, 1:2) + plm::lag(disc_trim, 1) + plm::lag(supp_trim, 1) +
                     {RHS_CTRL_PLM} | plm::lag(polyarchy, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))"),
    `gmm-regime` = glue("pgmm(polyarchy ~ plm::lag(polyarchy, 1:2) + plm::lag(disc_dem_trim, 1) + plm::lag(disc_aut_trim, 1) + plm::lag(supp_dem_trim, 1) + plm::lag(supp_aut_trim, 1) +
                     {RHS_CTRL_PLM} | plm::lag(polyarchy, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', collapse=TRUE, indexes=c('iso3c', 'year'))")
  )
)

# naive: same models on the posterior-mean data, same robust vcov as the MOC
naive_p <- naive_dat %>%
    as.data.frame() %>%
    plm::pdata.frame(index = c("iso3c", "year"))
naive_dyn <- function(aMod, gmm = FALSE) {
    aData <- naive_p
    m <- eval(parse(text = aMod))
    V <- if (gmm) plmHC(m) else vcovBK(m)
    tibble(
        term = names(coef(m)),
        naive_est = as.numeric(coef(m)),
        naive_se = sqrt(diag(V)),
        nobs = stats::nobs(m)
    )
}

# --- Run Model and Save
set.seed(313)
for (dv in names(ls_dyn)) {
    mods <- ls_dyn[[dv]]
    moc_dyn <- list()
    naive_dyn_results <- list()
    for (nm in names(mods)) {
        message(glue("dynamic MOC: {dv} [{nm}] ..."))
        is_gmm <- grepl("^gmm", nm)
        moc_dyn[[nm]] <- if (is_gmm) methodComposition2(data_plm, mods[[nm]]) else
            methodComposition1(data_plm, mods[[nm]])
        naive_dyn_results[[nm]] <- naive_dyn(mods[[nm]], gmm = is_gmm)
    }
    saveRDS(moc_dyn, here("output", glue("results_ctrlglv_{dv}.rds")))
    saveRDS(naive_dyn_results, here("output", glue("results_ctrlglv_{dv}_naive.rds")))
}

# --- Presentation
FOCAL8D <- c("plm::lag(disc_trim, 1)", "plm::lag(supp_trim, 1)",
             "plm::lag(disc_dem_trim, 1)", "plm::lag(disc_aut_trim, 1)",
             "plm::lag(supp_dem_trim, 1)", "plm::lag(supp_aut_trim, 1)")
for (dv in names(ls_dyn)) {
  moc_dyn <- readRDS(here("output", glue("results_ctrlglv_{dv}.rds")))
  naive_dyn_results <- readRDS(here("output", glue("results_ctrlglv_{dv}_naive.rds")))
  for (nm in names(moc_dyn)) {
    m <- MOCsumm(as_tibble(moc_dyn[[nm]]), digits = 6) %>%
      filter(term %in% FOCAL8D) %>%
      mutate(
        estimate  = as.numeric(estimate),
        std.error = as.numeric(std.error),
        moc_p     = round(2 * (1 - pnorm(abs(estimate / std.error))), 3),
        estimate  = signif(estimate, 3),
        std.error = signif(std.error, 3)
      )
    n <- naive_dyn_results[[nm]] %>%
      filter(term %in% FOCAL8D) %>%
      transmute(term,
                naive_p   = round(2 * (1 - pnorm(abs(naive_est / naive_se))), 3),
                naive_est = signif(naive_est, 3))
    m <- left_join(m, n, by = "term")
    cat("\n--", dv, "[", nm, "] ( N:", naive_dyn_results[[nm]]$nobs[1], ")\n")
    print(as.data.frame(m), row.names = FALSE)
  }
}

# --- DIAGNOSTICS: system GMM (naive) ---------------------------------------
cat("\n------ system-GMM diagnostics, controls spec (naive) ------\n")
for (dv in names(ls_dyn)) for (nm in c("gmm", "gmm-regime")) {
    aData <- naive_p
    g <- tryCatch(eval(parse(text = ls_dyn[[dv]][[nm]])), error = function(e) NULL)
    if (is.null(g)) { cat(dv, nm, ": pgmm failed\n"); next }
    s <- summary(g, robust = TRUE)
    n_inst <- dim(g$W[[1]])[2]
    n_grp <- pdim(g)$nT$n
    cat(sprintf(
        "%-25s | #inst=%d  #countries=%d (%s) | Sargan p=%.3f | AR(2) p=%.3f\n",
        paste(dv, nm), n_inst, n_grp,
        ifelse(n_inst <= n_grp, "Roodman OK", "OVER-INSTRUMENTED"),
        s$sargan$p.value, plm::mtest(g, order = 2)$p.value
    ))
}

##############################################################################
# 9. Combine Results
###############################################################################

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
  "disc_dem_trim_row", "disc_aut_trim_row", "supp_dem_trim_row", "supp_aut_trim_row",
  "plm::lag(disc_trim, 1)", "plm::lag(supp_trim, 1)",
  "plm::lag(disc_dem_trim, 1)", "plm::lag(disc_aut_trim, 1)",
  "plm::lag(supp_dem_trim, 1)", "plm::lag(supp_aut_trim, 1)"
)

# --- sec. 8 dynamic panel: pooled / pooled-regime / gmm --------------------
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
          block = "8 dynamic panel",
          model = glue("{dv_lab} [{est}]"), n_events = NA_integer_
        )
    })
  }
)

# --- secs. 6 + 7 hazard cells ----------------------------------------------
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
    "6 ERT hazard"
  ),
  haz9_one("results_canonicalglv_hazard.rds", "results_canonicalglv_hazard_naive.rds",
    "7 various DV"   # all cells are full-controls now; no keep filter needed
  )
)

main_results <- bind_rows(dyn9, haz9) %>%
  # display names (ASCII only -- write.csv under a C locale mangles unicode):
  # regime-restricted samples say "subsample"; ERT ladder DVs are annotated:
  # any onset / liberalization onset (up) / erosion onset (down) /
  # RoW change (4 category) / regime transition (dem vs auto)
  mutate(
    model = gsub("\\((democracies|autocracies|non-democracies)\\)", "(\\1 subsample)", model),
    model = gsub("^onset \\(", "any onset (", model),
    model = gsub("^liberalization onset ", "liberalization onset (up) ", model),
    model = gsub("^erosion onset ", "erosion onset (down) ", model),
    model = gsub("^RoW change ", "RoW change (4 category) ", model),
    model = gsub("^regime transition ", "regime transition (dem vs auto) ", model),
    model = gsub("^(DD|GWF|BMR) (crossing|transition) ", "\\1 regime transition (dem vs auto) ", model),
    # one grammar everywhere: DV (design) [estimator]
    model = gsub("\\(split IVs\\)", "(regime split)", model),
    model = gsub("\\[pooled\\]$", "(all regimes) [pooled OLS]", model),
    model = gsub("\\[pooled-regime\\]$", "(regime split) [pooled OLS]", model),
    model = gsub("\\[gmm\\]$", "(all regimes) [system GMM]", model),
    model = gsub("\\[gmm-regime\\]$", "(regime split) [system GMM, cautionary]", model)
  ) %>%
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


