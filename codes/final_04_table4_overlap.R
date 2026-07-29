# ============================================================================
# final_04_table4_overlap.R -- Table 4: which democratization/breakdown events each
# coding captures, within the survey-covered sample. Match rule: same country,
# event years within +/-2 (codings date the same transition differently).
# Requires the master's data-build head (sections 0-6); estimates nothing.
# Output: output/table4_event_overlap.csv
# ============================================================================
msrc <- readLines("codes/final_01_master.R")
cut1 <- grep("data <- pmap", msrc, fixed = TRUE)[1]
eval(parse(text = msrc[1:(cut1 - 1)]), envir = .GlobalEnv)   # master head (rm(list=ls()) inside)

d1  <- build_one(disc_list[[1]], supp_list[[1]], ctrl_list[[1]])
smp <- d1 %>% filter(!is.na(disc_trim))   # in-sample = survey-covered country-years

ev <- function(col) smp %>% filter(.data[[col]] == 1) %>% dplyr::select(iso3c, year)
lst <- list(
  Democratization = list(ERT = ev("ert_democratization"), DD = ev("dd_democratization"),
                         BMR = ev("bmr_democratization"), GWF = ev("gwf_democratization")),
  Breakdown       = list(ERT = ev("ert_dem_break"), DD = ev("dd_demo_break"),
                         BMR = ev("bmr_demo_break"), GWF = ev("gwf_demo_break")))

# Coverage ends by coding: a row event later than a column coding's last year
# (+2yr match margin) can never be captured by that column, so it is dropped
# from that cell's denominator. Only GWF (ends 2010) binds in our sample.
cov_end <- c(ERT = Inf, DD = Inf, BMR = Inf, GWF = 2010)

near <- function(a, b) sapply(seq_len(nrow(a)), function(i)
  any(b$iso3c == a$iso3c[i] & abs(b$year - a$year[i]) <= 2))

t4 <- purrr::imap_dfr(lst, function(side, side_lab)
  purrr::imap_dfr(side, function(A, la) {
    row <- tibble::tibble(Event = side_lab, Coding = la, Events = nrow(A))
    for (lb in names(side)) {
      row[[paste("Captured by", lb)]] <-
        if (lb == la) "--" else {
          A_el <- A %>% filter(year <= cov_end[[lb]] + 2)  # comparable window only
          m <- sum(near(A_el, side[[lb]]))
          sprintf("%d/%d (%.0f%%)", m, nrow(A_el), 100 * m / nrow(A_el))
        }
    }
    row
  }))

write.csv(t4, here("output", "table4_event_overlap.csv"), row.names = FALSE)
cat("written: output/table4_event_overlap.csv\n"); print(as.data.frame(t4))
