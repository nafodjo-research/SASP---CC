# Fix nonGS tournament performance tables
# These were preserved by script 38 but need updating to match new delta model
library(here)
source(here("scripts", "R", "utils.R"))
summary_log <- character()

TABLES_DIR <- here("Tables")
CLEANED_DIR <- here("Data", "cleaned")

res <- readRDS(file.path(CLEANED_DIR, "delta_model_results.rds"))

sample_info <- list(
  nongs_atp = list(n = 113189, np = 838, label = "Non-GS ATP"),
  nongs_wta = list(n = 80168, np = 747, label = "Non-GS WTA")
)

for (tag in c("nongs_atp", "nongs_wta")) {
  r <- res[[tag]]
  boot <- res[[paste0("boot_", tag)]]
  info <- sample_info[[tag]]
  if (is.null(r)) next

  mod <- r$pooled
  ct <- coef(summary(mod))
  delta <- ct["got_ll", "Estimate"]
  delta_se <- boot$delta_se
  delta_p <- 2 * pnorm(-abs(delta / delta_se))

  rho <- if ("v_hat" %in% rownames(ct)) ct["v_hat", "Estimate"] else NA
  rho_se <- if ("v_hat" %in% rownames(ct)) ct["v_hat", "Std. Error"] else NA
  rho_p <- if ("v_hat" %in% rownames(ct)) ct["v_hat", "Pr(>|z|)"] else NA

  lines <- c("\\begin{tabular}{lc}", "\\toprule",
             paste0(" & ", info$label, " \\\\"), "\\midrule")

  lines <- c(lines, paste0("$\\hat{\\delta}$ (LL entry) & ",
                           fmt(delta, 4), add_stars(delta_p), " \\\\"))
  lines <- c(lines, paste0(" & (", fmt(delta_se, 4), ") \\\\"))

  if (!is.na(rho)) {
    lines <- c(lines, paste0("$\\hat{\\rho}$ (CF) & ",
                             fmt(rho, 4), add_stars(rho_p), " \\\\"))
    lines <- c(lines, paste0(" & (", fmt(rho_se, 4), ") \\\\"))
  }

  lines <- c(lines, "\\midrule")
  lines <- c(lines, paste0("Matches & ", format(info$n, big.mark = ","), " \\\\"))
  lines <- c(lines, paste0("Players & ", format(info$np, big.mark = ","), " \\\\"))
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  tex <- paste(lines, collapse = "\n")
  writeLines(tex, file.path(TABLES_DIR, paste0("table_tournament_nongs_", sub("nongs_", "", tag), ".tex")))
  cat("Saved:", paste0("table_tournament_nongs_", sub("nongs_", "", tag), ".tex"), "\n")
}
