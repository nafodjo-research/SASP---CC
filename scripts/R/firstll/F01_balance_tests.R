# ==============================================================================
# F01_balance_tests.R
# Balance tests for the first-LL restricted sample.
#
# Adapts 39_balance_tests.R:
#   - Reads from Data/cleaned/firstll/
#   - Drops prior-LL variables from balance (all zero by construction)
#   - Runs GS and non-GS balance tests
#   - Outputs to Tables_FirstLL/
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#
# Outputs:
#   Tables_FirstLL/table_balance.tex       (GS balance)
#   Tables_FirstLL/table_balance_nongs.tex  (non-GS balance)
#
# Dependencies: dplyr, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()

# Load first-LL samples
gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))

gs_atp <- gs[gs$tour == "ATP", ]
gs_wta <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

message("  GS-ATP: ", nrow(gs_atp), "  GS-WTA: ", nrow(gs_wta))
message("  NGS-ATP: ", nrow(ngs_atp), "  NGS-WTA: ", nrow(ngs_wta))


# ==============================================================================
# BALANCE TEST FUNCTION (no prior-LL variables)
# ==============================================================================

balance_vars <- c(
  "pre_rank_pts_s", "pre_rank_pts_sq_s",
  "pre_elo_s", "pre_elo_sq_s",
  "pre_surf_elo_s", "pre_surf_elo_sq_s",
  "player_age"
)

bal_var_labels <- c(
  "pre_rank_pts_s"     = "Ranking points / 1000",
  "pre_rank_pts_sq_s"  = "(Ranking points / 1000)$^2$",
  "pre_elo_s"          = "Elo / 100",
  "pre_elo_sq_s"       = "(Elo / 100)$^2$",
  "pre_surf_elo_s"     = "Surface Elo / 100",
  "pre_surf_elo_sq_s"  = "(Surface Elo / 100)$^2$",
  "player_age"         = "Age"
)

run_balance <- function(data, sample_label) {
  rows <- list()
  for (v in balance_vars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    d <- data$got_ll
    ok <- !is.na(x)
    if (sum(ok & d == 1) < 3 || sum(ok & d == 0) < 3) next

    vlabel <- unname(bal_var_labels[v])
    tt <- tryCatch(t.test(x[ok & d == 1], x[ok & d == 0]), error = function(e) NULL)
    rows[[v]] <- data.frame(
      variable = v, label = vlabel,
      ll_mean = mean(x[ok & d == 1]), ct_mean = mean(x[ok & d == 0]),
      pvalue = if (!is.null(tt)) tt$p.value else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  bal <- do.call(rbind, rows)

  # Joint F-test
  avail_vars <- balance_vars[balance_vars %in% names(data)]
  fml_str <- paste0("got_ll ~ ", paste(avail_vars, collapse = " + "))
  f_test <- tryCatch({
    fit <- lm(as.formula(fml_str), data = data)
    fs <- summary(fit)$fstatistic
    f_pv <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    list(fstat = fs[1], pv = f_pv)
  }, error = function(e) list(fstat = NA, pv = NA))

  list(bal = bal, f_test = f_test, n = nrow(data), n_treated = sum(data$got_ll == 1))
}


# ==============================================================================
# RUN BALANCE TESTS
# ==============================================================================
message("\n", strrep("=", 70))
message("BALANCE TESTS")
message(strrep("=", 70))

# GS
bal_gs_atp <- run_balance(gs_atp, "GS-ATP")
bal_gs_wta <- run_balance(gs_wta, "GS-WTA")

message("  GS-ATP: N=", bal_gs_atp$n, " (", bal_gs_atp$n_treated, " treated)")
message("  GS-ATP joint F-test p = ", fmt(bal_gs_atp$f_test$pv, 3))
message("  GS-WTA: N=", bal_gs_wta$n, " (", bal_gs_wta$n_treated, " treated)")
message("  GS-WTA joint F-test p = ", fmt(bal_gs_wta$f_test$pv, 3))

# Non-GS
bal_ngs_atp <- run_balance(ngs_atp, "NGS-ATP")
bal_ngs_wta <- run_balance(ngs_wta, "NGS-WTA")

message("  NGS-ATP: N=", bal_ngs_atp$n, " (", bal_ngs_atp$n_treated, " treated)")
message("  NGS-ATP joint F-test p = ", fmt(bal_ngs_atp$f_test$pv, 3))
message("  NGS-WTA: N=", bal_ngs_wta$n, " (", bal_ngs_wta$n_treated, " treated)")
message("  NGS-WTA joint F-test p = ", fmt(bal_ngs_wta$f_test$pv, 3))


# ==============================================================================
# GENERATE LATEX TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("GENERATE TABLES")
message(strrep("=", 70))

generate_balance_table <- function(bal_a, bal_w) {
  vars_order <- balance_vars

  lines <- c(
    "\\begin{tabular}{l rrr rrr}",
    "\\toprule",
    " & \\multicolumn{3}{c}{ATP} & \\multicolumn{3}{c}{WTA} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Variable & LL & Control & $p$ & LL & Control & $p$ \\\\",
    "\\midrule"
  )

  for (v in vars_order) {
    ra <- bal_a$bal[bal_a$bal$variable == v, ]
    rw <- bal_w$bal[bal_w$bal$variable == v, ]
    if (nrow(ra) == 0 && nrow(rw) == 0) next

    lab <- if (nrow(ra) > 0) ra$label[1] else rw$label[1]

    atp_str <- if (nrow(ra) > 0) {
      paste0(fmt(ra$ll_mean), " & ", fmt(ra$ct_mean), " & ", fmt(ra$pvalue, 3))
    } else " & & "

    wta_str <- if (nrow(rw) > 0) {
      paste0(fmt(rw$ll_mean), " & ", fmt(rw$ct_mean), " & ", fmt(rw$pvalue, 3))
    } else " & & "

    lines <- c(lines, paste0(lab, " & ", atp_str, " & ", wta_str, " \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0(
    "Joint $F$-test $p$-value & \\multicolumn{3}{c}{",
    fmt(bal_a$f_test$pv, 3), "} & \\multicolumn{3}{c}{",
    fmt(bal_w$f_test$pv, 3), "} \\\\"
  ))
  lines <- c(lines, paste0(
    "$N$ & \\multicolumn{3}{c}{",
    bal_a$n, "} & \\multicolumn{3}{c}{",
    bal_w$n, "} \\\\"
  ))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  paste(lines, collapse = "\n")
}

# GS balance table
table_gs <- generate_balance_table(bal_gs_atp, bal_gs_wta)
writeLines(table_gs, file.path(FIRSTLL_TABLES, "table_balance.tex"))
message("  Saved: Tables_FirstLL/table_balance.tex")

# Non-GS balance table
table_ngs <- generate_balance_table(bal_ngs_atp, bal_ngs_wta)
writeLines(table_ngs, file.path(FIRSTLL_TABLES, "table_balance_nongs.tex"))
message("  Saved: Tables_FirstLL/table_balance_nongs.tex")

# Print results
for (lbl in c("GS-ATP", "GS-WTA", "NGS-ATP", "NGS-WTA")) {
  bal <- switch(lbl,
    "GS-ATP" = bal_gs_atp, "GS-WTA" = bal_gs_wta,
    "NGS-ATP" = bal_ngs_atp, "NGS-WTA" = bal_ngs_wta
  )
  message("\n  === ", lbl, " (N=", bal$n, ") ===")
  for (i in seq_len(nrow(bal$bal))) {
    r <- bal$bal[i, ]
    star <- ifelse(r$pvalue < 0.05, " **", ifelse(r$pvalue < 0.10, " *", ""))
    message(sprintf("    %-30s  LL=%.2f  Ctrl=%.2f  p=%.3f%s",
                    r$label, r$ll_mean, r$ct_mean, r$pvalue, star))
  }
  message("  Joint F-test p = ", fmt(bal$f_test$pv, 3))
}

message("\nDONE.")
