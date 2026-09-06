# ==============================================================================
# F08_ll_careers.R
# Cross-tab tables: LL match performance and LL slot distribution
# for the first-LL restricted sample.
#
# Table 1 (per sample): How many LL recipients won 0, 1, 2, 3+ main draw
#   matches at the LL event.
# Table 2 (per sample): LL distribution -- number of LL slots per event,
#   number of events.
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#
# Outputs:
#   Tables_FirstLL/table_ll_careers_gs.tex
#   Tables_FirstLL/table_ll_careers_nongs.tex
#   Tables_FirstLL/table_ll_dist_gs.tex
#   Tables_FirstLL/table_ll_dist_nongs.tex
#
# Dependencies: dplyr, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("F08: LL CAREERS AND DISTRIBUTION (First-LL)")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))

message("  GS: ", nrow(gs), " rows | NonGS: ", nrow(ngs), " rows")


# ==============================================================================
# TABLE 1: LL MATCH PERFORMANCE AT THE LL EVENT
# ==============================================================================

generate_careers_table <- function(df, sample_label) {
  # Filter to LL recipients only
  ll <- df[df$got_ll == 1, ]

  # Determine match wins column
  # Try md_matches_won, then matches_won_dose, then md_matches_played
  wins_col <- NULL
  for (cc in c("md_matches_won", "matches_won_dose", "md_matches_played")) {
    if (cc %in% names(ll) && !all(is.na(ll[[cc]]))) {
      wins_col <- cc
      break
    }
  }

  if (is.null(wins_col)) {
    message("  WARNING: No match wins column found for ", sample_label)
    return(NULL)
  }

  ll$wins_cat <- ifelse(ll[[wins_col]] >= 3, "3+",
                        as.character(ll[[wins_col]]))
  ll$wins_cat <- factor(ll$wins_cat, levels = c("0", "1", "2", "3+"))

  # Split by tour
  tours <- sort(unique(ll$tour))

  # Build cross-tab
  tab_rows <- list()
  for (cat in levels(ll$wins_cat)) {
    row_vals <- c()
    for (tr in tours) {
      sub <- ll[ll$tour == tr, ]
      n_cat <- sum(sub$wins_cat == cat, na.rm = TRUE)
      pct   <- 100 * n_cat / nrow(sub)
      row_vals <- c(row_vals, n_cat, sprintf("%.1f", pct))
    }
    tab_rows[[cat]] <- row_vals
  }

  # Total row
  total_vals <- c()
  for (tr in tours) {
    sub <- ll[ll$tour == tr, ]
    total_vals <- c(total_vals, nrow(sub), "100.0")
  }

  ncol_per_tour <- 2  # N and %
  ncol_total <- 1 + length(tours) * ncol_per_tour

  lines <- c(
    paste0("\\begin{tabular}{l", paste(rep(" rr", length(tours)), collapse = ""), "}"),
    "\\toprule"
  )

  # Header
  header_spans <- paste(vapply(tours, function(tr) {
    start_col <- 2 + (which(tours == tr) - 1) * ncol_per_tour
    end_col   <- start_col + ncol_per_tour - 1
    paste0("\\multicolumn{", ncol_per_tour, "}{c}{", tr, "}")
  }, character(1)), collapse = " & ")
  lines <- c(lines, paste0("MD Wins & ", header_spans, " \\\\"))

  # Cmidrules
  for (i in seq_along(tours)) {
    start_col <- 2 + (i - 1) * ncol_per_tour
    end_col   <- start_col + ncol_per_tour - 1
    lines <- c(lines, paste0("\\cmidrule(lr){", start_col, "-", end_col, "}"))
  }

  subheader <- paste(rep(c("$N$", "\\%"), length(tours)), collapse = " & ")
  lines <- c(lines, paste0(" & ", subheader, " \\\\"))
  lines <- c(lines, "\\midrule")

  # Data rows
  for (cat in levels(ll$wins_cat)) {
    vals <- paste(tab_rows[[cat]], collapse = " & ")
    lines <- c(lines, paste0(cat, " & ", vals, " \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Total & ", paste(total_vals, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS careers
tab_careers_gs <- generate_careers_table(gs, "GS")
if (!is.null(tab_careers_gs)) {
  writeLines(tab_careers_gs, file.path(FIRSTLL_TABLES, "table_ll_careers_gs.tex"))
  message("  Saved: table_ll_careers_gs.tex")
}

# NonGS careers
tab_careers_ngs <- generate_careers_table(ngs, "nonGS")
if (!is.null(tab_careers_ngs)) {
  writeLines(tab_careers_ngs, file.path(FIRSTLL_TABLES, "table_ll_careers_nongs.tex"))
  message("  Saved: table_ll_careers_nongs.tex")
}


# ==============================================================================
# TABLE 2: LL SLOT DISTRIBUTION PER EVENT
# ==============================================================================

generate_dist_table <- function(df, sample_label) {
  # Count LL slots per event (tourney_id)
  event_ll <- df %>%
    group_by(tourney_id, tour) %>%
    summarise(
      n_ll   = sum(got_ll == 1),
      n_total = n(),
      .groups = "drop"
    )

  tours <- sort(unique(event_ll$tour))

  # Distribution of n_ll per event
  max_ll <- max(event_ll$n_ll)
  slot_cats <- 0:min(max_ll, 6)
  if (max_ll > 6) slot_cats <- c(slot_cats, 99)  # 7+ category

  tab_rows <- list()
  for (s in slot_cats) {
    row_vals <- c()
    for (tr in tours) {
      sub <- event_ll[event_ll$tour == tr, ]
      if (s == 99) {
        n_events <- sum(sub$n_ll >= 7)
      } else {
        n_events <- sum(sub$n_ll == s)
      }
      pct <- 100 * n_events / nrow(sub)
      row_vals <- c(row_vals, n_events, sprintf("%.1f", pct))
    }
    label <- if (s == 99) "7+" else as.character(s)
    tab_rows[[label]] <- row_vals
  }

  # Totals
  total_vals <- c()
  for (tr in tours) {
    sub <- event_ll[event_ll$tour == tr, ]
    total_vals <- c(total_vals, nrow(sub), "100.0")
  }

  ncol_per_tour <- 2
  lines <- c(
    paste0("\\begin{tabular}{l", paste(rep(" rr", length(tours)), collapse = ""), "}"),
    "\\toprule"
  )

  header_spans <- paste(vapply(tours, function(tr) {
    paste0("\\multicolumn{", ncol_per_tour, "}{c}{", tr, "}")
  }, character(1)), collapse = " & ")
  lines <- c(lines, paste0("LL Slots & ", header_spans, " \\\\"))

  for (i in seq_along(tours)) {
    start_col <- 2 + (i - 1) * ncol_per_tour
    end_col   <- start_col + ncol_per_tour - 1
    lines <- c(lines, paste0("\\cmidrule(lr){", start_col, "-", end_col, "}"))
  }

  subheader <- paste(rep(c("Events", "\\%"), length(tours)), collapse = " & ")
  lines <- c(lines, paste0(" & ", subheader, " \\\\"))
  lines <- c(lines, "\\midrule")

  for (label in names(tab_rows)) {
    vals <- paste(tab_rows[[label]], collapse = " & ")
    lines <- c(lines, paste0(label, " & ", vals, " \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Total & ", paste(total_vals, collapse = " & "), " \\\\"))

  # Mean LL slots
  mean_row <- c()
  for (tr in tours) {
    sub <- event_ll[event_ll$tour == tr, ]
    mean_row <- c(mean_row,
                  paste0("\\multicolumn{2}{c}{", sprintf("%.2f", mean(sub$n_ll)), "}"))
  }
  lines <- c(lines, paste0("Mean & ", paste(mean_row, collapse = " & "), " \\\\"))

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

# GS distribution
tab_dist_gs <- generate_dist_table(gs, "GS")
writeLines(tab_dist_gs, file.path(FIRSTLL_TABLES, "table_ll_dist_gs.tex"))
message("  Saved: table_ll_dist_gs.tex")

# NonGS distribution
tab_dist_ngs <- generate_dist_table(ngs, "nonGS")
writeLines(tab_dist_ngs, file.path(FIRSTLL_TABLES, "table_ll_dist_nongs.tex"))
message("  Saved: table_ll_dist_nongs.tex")


# ==============================================================================
# SUMMARY
# ==============================================================================
message("\n", strrep("=", 70))
message("F08 COMPLETE")
message(strrep("=", 70))
message("  GS LL recipients: ", sum(gs$got_ll))
message("  NonGS LL recipients: ", sum(ngs$got_ll))
message("\nDONE.")
