# ==============================================================================
# 38_regenerate_tables.R
# Regenerate tournament-level LaTeX tables from delta model results.
#
# PURPOSE: The manuscript (mechanisms.tex, appendix.tex) references
#   table_tournament_* filenames, but the delta model script (37) produced
#   table_delta_* filenames. This script reads the delta tables and the
#   existing tournament tables, then writes all manuscript-expected files.
#
# STRATEGY: The delta_model_results.rds (475MB) contains glm objects with
#   stripped $data that cause segfaults on load. Instead, we parse the
#   already-generated table_delta_*.tex files for the current estimates
#   and produce the tournament-named tables the manuscript expects.
#
# INPUTS:
#   Tables/table_delta_pooled_*.tex    (pooled delta estimates)
#   Tables/table_delta_dose_mw_*.tex   (dose: matches won)
#   Tables/table_delta_dose_pp_*.tex   (dose: performance probability)
#   Tables/table_delta_horizon_*.tex   (horizon heterogeneity)
#   Tables/table_tournament_nongs_*.tex (existing CF tables -- preserved)
#
# OUTPUTS (mechanisms.tex -- main body):
#   Tables/table_tournament_firstll_gs_atp.tex
#   Tables/table_tournament_firstll_gs_wta.tex
#   Tables/table_tournament_dose_gs_atp.tex
#   Tables/table_tournament_dose_gs_wta.tex
#   Tables/table_tournament_firstll_horizon_gs_atp.tex
#   Tables/table_tournament_firstll_horizon_gs_wta.tex
#
# OUTPUTS (appendix.tex):
#   Tables/table_tournament_gs_atp.tex
#   Tables/table_tournament_gs_wta.tex
#   Tables/table_tournament_nongs_atp.tex   (existing CF table preserved)
#   Tables/table_tournament_nongs_wta.tex   (existing CF table preserved)
#   Tables/table_tournament_robustness_gs_atp.tex
#   Tables/table_tournament_robustness_gs_wta.tex
#   Tables/table_tournament_firstll_robust_gs_atp.tex
#   Tables/table_tournament_firstll_robust_gs_wta.tex
#   Tables/table_tournament_firstll_nongs_atp.tex
#   Tables/table_tournament_firstll_nongs_wta.tex
#   Tables/table_tournament_match_effects.tex
#
# DEPENDENCIES: none (pure string processing)
# ==============================================================================

set.seed(20260328)

library(here)

source(here("scripts", "R", "utils.R"))

TABLES_DIR <- here("Tables")
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: Parse existing delta tables for coefficient data
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: Parse existing delta tables")
message(strrep("=", 70))

# Helper: read a tex file and return as character vector
read_tex <- function(fname) {
  path <- file.path(TABLES_DIR, fname)
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(NULL)
  }
  readLines(path, warn = FALSE)
}

# Helper: extract a numeric value after a pattern from a tex line
extract_num <- function(line) {
  # Remove LaTeX formatting, stars, parens
  clean <- gsub("\\$\\^\\{[*]+\\}\\$", "", line)
  clean <- gsub("[\\\\${}]", "", clean)
  clean <- gsub("&", "", clean)
  nums <- regmatches(clean, gregexpr("-?[0-9]+\\.[0-9]+", clean))[[1]]
  as.numeric(nums)
}

# Helper: extract stars from a line
extract_stars <- function(line) {
  m <- regmatches(line, regexpr("\\$\\^\\{[*]+\\}\\$", line))
  if (length(m) == 0) return("")
  m
}

# Known sample sizes (from task description + existing tables)
SAMPLES <- list(
  gs_atp = list(
    label = "GS-ATP", n_matches = "8,543", n_players = "130",
    has_cf = FALSE
  ),
  gs_wta = list(
    label = "GS-WTA", n_matches = "5,391", n_players = "98",
    has_cf = FALSE
  ),
  nongs_atp = list(
    label = "NonGS-ATP", n_matches = "113,189", n_players = "838",
    has_cf = TRUE
  ),
  nongs_wta = list(
    label = "NonGS-WTA", n_matches = "80,168", n_players = "747",
    has_cf = TRUE
  )
)

# Parse pooled tables
parse_pooled <- function(tag) {
  lines <- read_tex(paste0("table_delta_pooled_", tag, ".tex"))
  if (is.null(lines)) return(NULL)
  result <- list()
  for (i in seq_along(lines)) {
    if (grepl("hat\\{\\\\delta\\}", lines[i])) {
      nums <- extract_num(lines[i])
      result$delta <- nums[1]
      result$delta_stars <- extract_stars(lines[i])
      # SE on next line
      se_nums <- extract_num(lines[i + 1])
      result$delta_se <- se_nums[1]
    }
    if (grepl("hat\\{\\\\rho\\}", lines[i])) {
      nums <- extract_num(lines[i])
      result$rho <- nums[1]
      result$rho_stars <- extract_stars(lines[i])
      se_nums <- extract_num(lines[i + 1])
      result$rho_se <- se_nums[1]
    }
  }
  result
}

# Parse dose (matches won) tables
parse_dose_mw <- function(tag) {
  lines <- read_tex(paste0("table_delta_dose_mw_", tag, ".tex"))
  if (is.null(lines)) return(NULL)
  result <- list()
  for (i in seq_along(lines)) {
    if (grepl("hat\\{\\\\delta\\}.*LL entry", lines[i])) {
      nums <- extract_num(lines[i])
      result$delta <- nums[1]
      result$delta_stars <- extract_stars(lines[i])
      se_nums <- extract_num(lines[i + 1])
      result$delta_se <- se_nums[1]
    }
    if (grepl("times.*Matches won", lines[i])) {
      nums <- extract_num(lines[i])
      result$dose <- nums[1]
      result$dose_stars <- extract_stars(lines[i])
      se_nums <- extract_num(lines[i + 1])
      result$dose_se <- se_nums[1]
    }
    if (grepl("hat\\{\\\\rho\\}", lines[i])) {
      nums <- extract_num(lines[i])
      result$rho <- nums[1]
      result$rho_stars <- extract_stars(lines[i])
      se_nums <- extract_num(lines[i + 1])
      result$rho_se <- se_nums[1]
    }
  }
  result
}

# Parse horizon tables
parse_horizon <- function(tag) {
  lines <- read_tex(paste0("table_delta_horizon_", tag, ".tex"))
  if (is.null(lines)) return(NULL)
  result <- list()
  # Find the delta_h line
  for (i in seq_along(lines)) {
    if (grepl("hat\\{\\\\delta\\}_h", lines[i])) {
      # Extract coefficients
      coef_line <- lines[i]
      se_line <- lines[i + 1]
      # Split by & to get values for each horizon
      coef_parts <- strsplit(coef_line, "&")[[1]][-1]  # drop label
      se_parts <- strsplit(se_line, "&")[[1]][-1]  # drop empty label

      horizons <- c("4w", "8w", "12w", "26w", "52w")
      for (j in seq_along(horizons)) {
        if (j <= length(coef_parts)) {
          nums <- extract_num(coef_parts[j])
          stars <- extract_stars(coef_parts[j])
          se_nums <- extract_num(se_parts[j])
          result[[horizons[j]]] <- list(
            coef = if (length(nums) > 0) nums[1] else NA,
            stars = if (length(stars) > 0) stars else "",
            se = if (length(se_nums) > 0) se_nums[1] else NA
          )
        }
      }
    }
  }
  result
}

# Parse all
pooled <- lapply(names(SAMPLES), parse_pooled)
names(pooled) <- names(SAMPLES)

dose_mw <- lapply(names(SAMPLES), parse_dose_mw)
names(dose_mw) <- names(SAMPLES)

horizon <- lapply(names(SAMPLES), parse_horizon)
names(horizon) <- names(SAMPLES)

# Verify parsing
for (tag in names(SAMPLES)) {
  message("  ", tag, ": pooled delta=", pooled[[tag]]$delta,
          ", dose delta=", dose_mw[[tag]]$delta,
          ", horizon 4w=", horizon[[tag]][["4w"]]$coef)
}


# ==============================================================================
# STEP 2: Generate pooled tables (table_tournament_firstll_* and table_tournament_gs_*)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: Generate pooled tables")
message(strrep("=", 70))

generate_pooled_table <- function(tag) {
  p <- pooled[[tag]]
  s <- SAMPLES[[tag]]

  lines <- c(
    "\\begin{tabular}{lc}",
    "\\toprule",
    paste0(" & ", s$label, " \\\\"),
    "\\midrule"
  )

  # Delta coefficient
  lines <- c(lines, paste0(
    "$\\hat{\\delta}$ (LL entry) & ",
    fmt(p$delta, 4), p$delta_stars, " \\\\"
  ))
  lines <- c(lines, paste0(
    " & (", fmt(p$delta_se, 4), ") \\\\"
  ))

  # Rho (CF) for nonGS
  if (s$has_cf && !is.null(p$rho)) {
    lines <- c(lines, paste0(
      "$\\hat{\\rho}$ (CF) & ",
      fmt(p$rho, 4), p$rho_stars, " \\\\"
    ))
    lines <- c(lines, paste0(
      " & (", fmt(p$rho_se, 4), ") \\\\"
    ))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", s$n_matches, " \\\\"))
  lines <- c(lines, paste0("Players & ", s$n_players, " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# Write GS pooled tables (main body: firstll; appendix: gs)
for (tag in c("gs_atp", "gs_wta")) {
  tex <- generate_pooled_table(tag)
  # Main body: table_tournament_firstll_*
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_firstll_", tag, ".tex")))
  message("  Wrote table_tournament_firstll_", tag, ".tex")
  # Appendix: table_tournament_gs_* (same content for GS samples)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_", tag, ".tex")))
  message("  Wrote table_tournament_", tag, ".tex")
}

# Write NonGS pooled tables (firstll version)
for (tag in c("nongs_atp", "nongs_wta")) {
  tex <- generate_pooled_table(tag)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_firstll_", tag, ".tex")))
  message("  Wrote table_tournament_firstll_", tag, ".tex")
}

# NOTE: table_tournament_nongs_atp.tex and table_tournament_nongs_wta.tex
# already exist with the full CF specification (including all covariates).
# We preserve those existing files as they contain richer information.
message("  Preserved existing table_tournament_nongs_atp.tex (full CF spec)")
message("  Preserved existing table_tournament_nongs_wta.tex (full CF spec)")


# ==============================================================================
# STEP 3: Generate dose tables (table_tournament_dose_*)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: Generate dose tables")
message(strrep("=", 70))

generate_dose_table <- function(tag) {
  d <- dose_mw[[tag]]
  s <- SAMPLES[[tag]]

  lines <- c(
    "\\begin{tabular}{lc}",
    "\\toprule",
    paste0(" & ", s$label, " \\\\"),
    "\\midrule"
  )

  # Base delta
  lines <- c(lines, paste0(
    "$\\hat{\\delta}$ (LL entry) & ",
    fmt(d$delta, 4), d$delta_stars, " \\\\"
  ))
  lines <- c(lines, paste0(
    " & (", fmt(d$delta_se, 4), ") \\\\"
  ))

  # Dose interaction
  if (!is.null(d$dose)) {
    lines <- c(lines, paste0(
      "$\\hat{\\delta}$ $\\times$ Matches won & ",
      fmt(d$dose, 4), d$dose_stars, " \\\\"
    ))
    lines <- c(lines, paste0(
      " & (", fmt(d$dose_se, 4), ") \\\\"
    ))
  }

  # Rho (CF) for nonGS
  if (s$has_cf && !is.null(d$rho)) {
    lines <- c(lines, paste0(
      "$\\hat{\\rho}$ (CF) & ",
      fmt(d$rho, 4), d$rho_stars, " \\\\"
    ))
    lines <- c(lines, paste0(
      " & (", fmt(d$rho_se, 4), ") \\\\"
    ))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", s$n_matches, " \\\\"))
  lines <- c(lines, paste0("Players & ", s$n_players, " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

for (tag in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  tex <- generate_dose_table(tag)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_dose_", tag, ".tex")))
  message("  Wrote table_tournament_dose_", tag, ".tex")
}


# ==============================================================================
# STEP 4: Generate horizon tables (table_tournament_firstll_horizon_*)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: Generate horizon tables")
message(strrep("=", 70))

generate_horizon_table <- function(tag) {
  h <- horizon[[tag]]
  s <- SAMPLES[[tag]]
  horizons <- c("4w", "8w", "12w", "26w", "52w")

  lines <- c(
    "\\begin{tabular}{l*{5}{c}}",
    "\\toprule",
    paste0(" & ", paste(horizons, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  # Coefficient row
  coef_vals <- sapply(horizons, function(hz) {
    if (!is.null(h[[hz]]) && !is.na(h[[hz]]$coef)) {
      paste0(fmt(h[[hz]]$coef, 4), h[[hz]]$stars)
    } else {
      ""
    }
  })
  lines <- c(lines, paste0(
    "$\\hat{\\delta}_h$ & ", paste(coef_vals, collapse = " & "), " \\\\"
  ))

  # SE row
  se_vals <- sapply(horizons, function(hz) {
    if (!is.null(h[[hz]]) && !is.na(h[[hz]]$se)) {
      paste0("(", fmt(h[[hz]]$se, 4), ")")
    } else {
      ""
    }
  })
  lines <- c(lines, paste0(
    " & ", paste(se_vals, collapse = " & "), " \\\\"
  ))

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & \\multicolumn{4}{l}{", s$n_matches, "} \\\\"))
  lines <- c(lines, paste0("Players & \\multicolumn{4}{l}{", s$n_players, "} \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

for (tag in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  tex <- generate_horizon_table(tag)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_firstll_horizon_", tag, ".tex")))
  message("  Wrote table_tournament_firstll_horizon_", tag, ".tex")
}


# ==============================================================================
# STEP 5: Generate robustness tables
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: Generate robustness tables")
message(strrep("=", 70))

# The robustness tables show the pooled estimate in a multi-specification format.
# For GS samples, we show the delta model result as the primary specification.
generate_robustness_table <- function(tag) {
  p <- pooled[[tag]]
  d_mw <- dose_mw[[tag]]
  s <- SAMPLES[[tag]]

  # Compute OR from delta
  or_pooled <- exp(p$delta)
  or_dose <- if (!is.null(d_mw$delta)) exp(d_mw$delta) else NA

  lines <- c(
    "\\begin{tabular}{lrrrrrr}",
    "\\toprule",
    "Specification & $N_{\\text{matches}}$ & $N_{\\text{players}}$ & $\\hat{\\delta}$ & SE & OR \\\\"
  )

  # Rho column for CF models
  if (s$has_cf) {
    lines[3] <- "Specification & $N_{\\text{matches}}$ & $N_{\\text{players}}$ & $\\hat{\\delta}$ & SE & OR & $\\hat{\\rho}$ \\\\"
  }

  lines <- c(lines, "\\midrule")

  # Row: Pooled
  row_pooled <- sprintf("Pooled & %s & %s & %s%s & (%s) & %.3f",
                        s$n_matches, s$n_players,
                        fmt(p$delta, 4), p$delta_stars,
                        fmt(p$delta_se, 4), or_pooled)
  if (s$has_cf && !is.null(p$rho)) {
    row_pooled <- paste0(row_pooled, " & ", fmt(p$rho, 4), p$rho_stars)
  }
  lines <- c(lines, paste0(row_pooled, " \\\\"))

  # Row: Dose (matches won)
  row_dose <- sprintf("Dose (matches won) & %s & %s & %s%s & (%s) & %.3f",
                      s$n_matches, s$n_players,
                      fmt(d_mw$delta, 4), d_mw$delta_stars,
                      fmt(d_mw$delta_se, 4), or_dose)
  if (s$has_cf && !is.null(d_mw$rho)) {
    row_dose <- paste0(row_dose, " & ", fmt(d_mw$rho, 4), d_mw$rho_stars)
  }
  lines <- c(lines, paste0(row_dose, " \\\\"))

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

for (tag in c("gs_atp", "gs_wta")) {
  tex <- generate_robustness_table(tag)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_robustness_", tag, ".tex")))
  message("  Wrote table_tournament_robustness_", tag, ".tex")
  # Also write firstll_robust version (same content)
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_firstll_robust_", tag, ".tex")))
  message("  Wrote table_tournament_firstll_robust_", tag, ".tex")
}


# ==============================================================================
# STEP 6: Generate match effects summary table
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: Generate match effects summary table")
message(strrep("=", 70))

# Summary table: one row per sample
lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Sample & $\\hat{\\delta}$ & SE & $p$-value & $N$ matches & $N$ players \\\\",
  "\\midrule"
)

for (tag in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  p <- pooled[[tag]]
  s <- SAMPLES[[tag]]

  # Compute p-value from delta/se
  z <- abs(p$delta / p$delta_se)
  pval <- 2 * pnorm(-z)
  pval_str <- if (pval < 0.001) "$<$0.001" else fmt(pval, 3)

  row <- sprintf("%s & %s%s & (%s) & %s & %s & %s \\\\",
                 s$label,
                 fmt(p$delta, 4), p$delta_stars,
                 fmt(p$delta_se, 4),
                 pval_str,
                 s$n_matches, s$n_players)
  lines <- c(lines, row)
}

lines <- c(lines, "\\bottomrule", "\\end{tabular}")

writeLines(paste(lines, collapse = "\n"),
           file.path(TABLES_DIR, "table_tournament_match_effects.tex"))
message("  Wrote table_tournament_match_effects.tex")


# ==============================================================================
# STEP 7: Verify all expected files exist
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 7: Verify outputs")
message(strrep("=", 70))

expected_files <- c(
  # Main body (mechanisms.tex)
  "table_tournament_firstll_gs_atp.tex",
  "table_tournament_firstll_gs_wta.tex",
  "table_tournament_dose_gs_atp.tex",
  "table_tournament_dose_gs_wta.tex",
  "table_tournament_firstll_horizon_gs_atp.tex",
  "table_tournament_firstll_horizon_gs_wta.tex",
  # Appendix
  "table_tournament_gs_atp.tex",
  "table_tournament_gs_wta.tex",
  "table_tournament_nongs_atp.tex",
  "table_tournament_nongs_wta.tex",
  "table_tournament_robustness_gs_atp.tex",
  "table_tournament_robustness_gs_wta.tex",
  "table_tournament_firstll_robust_gs_atp.tex",
  "table_tournament_firstll_robust_gs_wta.tex",
  "table_tournament_firstll_nongs_atp.tex",
  "table_tournament_firstll_nongs_wta.tex",
  "table_tournament_match_effects.tex"
)

all_ok <- TRUE
for (f in expected_files) {
  path <- file.path(TABLES_DIR, f)
  if (file.exists(path)) {
    sz <- file.info(path)$size
    message("  OK  ", f, " (", sz, " bytes)")
  } else {
    message("  MISSING  ", f)
    all_ok <- FALSE
  }
}

if (all_ok) {
  message("\n  All ", length(expected_files), " expected table files verified.")
} else {
  message("\n  WARNING: Some files missing!")
}

message("\n", strrep("=", 70))
message("DONE: Script 38 complete")
message(strrep("=", 70))
