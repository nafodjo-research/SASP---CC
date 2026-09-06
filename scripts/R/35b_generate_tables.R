# 35b_generate_tables.R
# Generate LaTeX tables from the re-estimated non-GS data (skeleton_nongs_est_v5.rds)
library(dplyr)
library(here)
library(fixest)
source(here("scripts", "R", "utils.R"))
summary_log <- character()

TABLES_DIR <- here("Tables")

nongs_est <- readRDS(here("Data", "cleaned", "skeleton_nongs_est_v5.rds"))
outcomes_base <- c("points_change", "n_main_draws", "n_matches_250plus", "elo_change")
HORIZONS <- c(4, 8, 12, 26, 52)
HORIZON_LABS <- paste0(HORIZONS, "w")
ZPRE_FULL <- paste0("pre_rank_pts + pre_rank_pts_sq + pre_elo + pre_elo_sq",
                     " + n_prior_gs_ll_won + n_prior_gs_ll_notwon",
                     " + n_prior_nongs_ll_won + n_prior_nongs_ll_notwon",
                     " + player_age")

stack_horizons_v2 <- function(data, outcomes_base, horizons = c(4, 8, 12, 26, 52)) {
  stacked <- list()
  for (h in horizons) {
    h_label <- paste0(h, "w")
    row_data <- data[, c("player_id", "tourney_id", "tour", "slam_year", "got_ll",
                         "pre_rank_pts", "pre_rank_pts_sq", "player_age",
                         "pre_elo", "pre_elo_sq",
                         "n_prior_gs_ll_won", "n_prior_gs_ll_notwon",
                         "n_prior_nongs_ll_won", "n_prior_nongs_ll_notwon",
                         "peer_component", "v_hat"), drop = FALSE]
    row_data$horizon <- h_label
    row_data$horizon_num <- h
    for (ob in outcomes_base) {
      col_name <- paste0(ob, "_", h, "w")
      if (col_name %in% names(data)) row_data[[ob]] <- data[[col_name]]
      else row_data[[ob]] <- NA_real_
    }
    stacked[[h_label]] <- row_data
  }
  result <- do.call(rbind, stacked)
  result$horizon <- factor(result$horizon, levels = paste0(horizons, "w"))
  result
}

nongs_atp <- nongs_est[nongs_est$tour == "ATP" & !is.na(nongs_est$peer_component), ]
nongs_wta <- nongs_est[nongs_est$tour == "WTA" & !is.na(nongs_est$peer_component), ]
cat("ATP N:", nrow(nongs_atp), "WTA N:", nrow(nongs_wta), "\n")

stacked_atp <- stack_horizons_v2(nongs_atp, outcomes_base)
stacked_wta <- stack_horizons_v2(nongs_wta, outcomes_base)

run_cf <- function(stacked_data, tour_label) {
  results <- list()
  rho_results <- list()
  for (ob in outcomes_base) {
    if (!ob %in% names(stacked_data)) next
    sdata <- stacked_data[!is.na(stacked_data[[ob]]) &
                          !is.na(stacked_data$pre_rank_pts) &
                          !is.na(stacked_data$player_age) &
                          !is.na(stacked_data$pre_elo) &
                          !is.na(stacked_data$v_hat), ]
    if (nrow(sdata) < 50) next
    fml <- as.formula(paste0(ob, " ~ got_ll:horizon + v_hat:horizon + ", ZPRE_FULL,
                             " | tourney_id + horizon"))
    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id), error = function(e) NULL)
    if (is.null(mod)) next
    ct <- as.data.frame(coeftable(mod))
    ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      rn <- paste0("v_hat:horizon", h)
      if (tn %in% ct$var) {
        row_t <- ct[ct$var == tn, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          coef = row_t$Estimate, se = row_t[["Std. Error"]],
          pval = row_t[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
      if (rn %in% ct$var) {
        row_r <- ct[ct$var == rn, ]
        rho_results[[paste0(ob, "_", h)]] <- data.frame(
          tour = tour_label, outcome = ob, horizon = h,
          rho = row_r$Estimate, rho_se = row_r[["Std. Error"]],
          rho_pval = row_r[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
    }
  }
  list(results = do.call(rbind, results), rho_results = do.call(rbind, rho_results))
}

atp_cf <- run_cf(stacked_atp, "ATP")
wta_cf <- run_cf(stacked_wta, "WTA")

make_table <- function(cf_res, rho_res, tour_label) {
  outcomes_order <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus")
  out_labs <- c(
    points_change     = "Ranking Pts $\\Delta$",
    elo_change        = "Elo $\\Delta$",
    n_main_draws      = "Main Draws",
    n_matches_250plus = "Matches 250+"
  )
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")
  for (ob in outcomes_order) {
    ob_res <- cf_res[cf_res$outcome == ob, ]
    if (nrow(ob_res) == 0) next
    cv <- sv <- character()
    for (h in HORIZON_LABS) {
      r <- ob_res[ob_res$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        st <- add_stars(r$pval[1])
        cv <- c(cv, paste0(fmt(r$coef[1], 1), st))
        sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")"))
      }
    }
    L <- c(L, paste0(out_labs[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
  L <- c(L, "\\midrule")
  rv <- character()
  for (h in HORIZON_LABS) {
    rr <- rho_res[rho_res$outcome == "points_change" & rho_res$horizon == h, ]
    if (is.null(rr) || nrow(rr) == 0) rv <- c(rv, "")
    else {
      st <- add_stars(rr$rho_pval[1])
      rv <- c(rv, paste0(fmt(rr$rho[1], 2), st))
    }
  }
  L <- c(L, paste0("$\\hat{\\rho}$ (Pts) & ", paste(rv, collapse = " & "), " \\\\"))
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

atp_tab <- make_table(atp_cf$results, atp_cf$rho_results, "ATP")
wta_tab <- make_table(wta_cf$results, wta_cf$rho_results, "WTA")
writeLines(atp_tab, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_atp.tex"))
writeLines(wta_tab, file.path(TABLES_DIR, "table_dynamic_stacked_nongs_wta.tex"))
cat("Tables saved.\n")

# Print results for inspection
cat("\n=== ATP Non-GS CF (v3 model) ===\n")
print(atp_cf$results[, c("outcome", "horizon", "coef", "se", "pval")])
cat("\n=== WTA Non-GS CF (v3 model) ===\n")
print(wta_cf$results[, c("outcome", "horizon", "coef", "se", "pval")])
