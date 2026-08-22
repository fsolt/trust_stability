##############################################################################
# Political Discontent and Regime (In)stability
# Authors: Choi, Ma, Tai, Hu, Solt
# Input:  output/naive_dat.rds; ERT::get_eps()
# Output: output/table4_event_overlap.csv
##############################################################################

library(dplyr)
library(here)
library(countrycode)
library(purrr)
library(tibble)

smp <- readRDS(here("output", "naive_dat.rds")) %>%
    filter(!is.na(disc_trim))

cov <- smp %>%
    distinct(iso3c, year)

eps_raw <- ERT::get_eps() %>%
    mutate(
        iso3c = countrycode(
            country_id, "vdem", "iso3c",
            warn = FALSE
        )
    ) %>%
    filter(!is.na(iso3c))

# ERT events are completed transformation episodes.
# Keep the full episode window rather than assigning the event to a single year.
ep_ev <- function(id, start, end, out) {

    eps_raw %>%
        filter(.data[[out]] == 1) %>%
        group_by(iso3c, .data[[id]]) %>%
        summarise(
            s = min(.data[[start]], na.rm = TRUE),
            e = max(.data[[end]],   na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::select(iso3c, s, e) %>%
        # Keep episodes whose completion year falls within the
        # substantive analysis coverage.
        semi_join(
            cov %>% rename(e = year),
            by = c("iso3c", "e")
        )
}

# Dichotomous codings are point comparisons
ev <- function(col) {
    smp %>%
        filter(.data[[col]] == 1) %>%
        transmute(
            iso3c,
            s = year,
            e = year
        ) %>%
        distinct()
}

lst <- list(
    Democratization = list(
        ERT = ep_ev(
            "dem_ep_id",
            "dem_ep_start_year",
            "dem_ep_end_year",
            "dem_ep_outcome_agg"
        ),
        DD  = ev("dd_demo_trans"),
        BMR = ev("bmr_demo_trans"),
        GWF = ev("gwf_demo_trans")
    ),
    Breakdown = list(
        ERT = ep_ev(
            "aut_ep_id",
            "aut_ep_start_year",
            "aut_ep_end_year",
            "aut_ep_outcome_agg"
        ),
        DD  = ev("dd_auto_trans"),
        BMR = ev("bmr_auto_trans"),
        GWF = ev("gwf_auto_trans")
    )
)

cov_end <- c(
    ERT = 2025,
    DD  = 2025,
    BMR = 2020,
    GWF = 2010
)

near <- function(a, b, marg) {

    if (nrow(a) == 0L)
        return(logical(0))

    vapply(seq_len(nrow(a)), function(i) {

        any(
            b$iso3c == a$iso3c[i] &
            b$s - a$e[i] <= marg &
            a$s[i] - b$e <= marg
        )

    }, logical(1))
}

t4 <- imap_dfr(lst, function(side, side_lab) {

    imap_dfr(side, function(A, la) {

        row <- tibble(
            Event  = side_lab,
            Coding = la,
            Events = nrow(A)
        )

        for (lb in names(side)) {

            cell <- paste("Captured by", lb)

            if (lb == la) {
                row[[cell]] <- "--"
                next
            }

            # ERT uses an episode window; binary codings use point events.
            marg <- if (la == "ERT" || lb == "ERT") 0 else 2
            A_el <- if (la == "ERT") {
                A %>% filter(e <= cov_end[[lb]])
            } else {
                A %>% filter(s <= cov_end[[lb]])
            }

            if (nrow(A_el) == 0L) {
                row[[cell]] <- "NA"
                next
            }

            m <- sum(near(A_el, side[[lb]], marg))

            row[[cell]] <- sprintf(
                "%d/%d (%.0f%%)",
                m,
                nrow(A_el),
                100 * m / nrow(A_el)
            )
        }

        row
    })
})

write.csv(
    t4,
    here("output", "table4_event_overlap.csv"),
    row.names = FALSE
)

cat("written: output/table4_event_overlap.csv\n")
print(as.data.frame(t4))
