# ==============================================================================
# F03_dynamics_hetero.R
# Core estimation: stacked dynamic, heterogeneity, and dose models
# for the first-LL restricted sample.
#
# Incorporates fixes from 41 and 42:
#   - ZPRE as main effects only (no :horizon interaction) — 42 FIX 1
#   - Win percentage outcome — 41 FIX 2
#   - Dose uses -logit(π) — 41 FIX 3
#   - Heterogeneity tables in side-by-side format — 41 FIX 4
#
# Key difference from main pipeline:
#   - ZPRE drops prior-LL variables (all zero by construction)
#   - No heterogeneity by had_prior_ll (no variation)
#
# Inputs:
#   Data/cleaned/firstll/firstll_gs_est.rds
#   Data/cleaned/firstll/firstll_nongs_est.rds
#   Data/cleaned/firstll/firstll_performance_dose.rds
#
# Outputs:
#   Tables_FirstLL/table_dynamic_stacked_{atp,wta}.tex
#   Tables_FirstLL/table_dynamic_stacked_nongs_{atp,wta}.tex
#   Tables_FirstLL/table_dynamic_full_{atp,wta}.tex
#   Tables_FirstLL/table_dynamic_full_nongs_{atp,wta}.tex
#   Tables_FirstLL/table_hetero_stacked_{atp,wta}.tex
#   Tables_FirstLL/table_hetero_stacked_nongs_{atp,wta}.tex
#   Tables_FirstLL/table_dose_stacked.tex
#   Tables_FirstLL/table_dose_stacked_nongs.tex
#   Tables_FirstLL/table_perf_prob_stacked.tex
#   Tables_FirstLL/table_perf_prob_stacked_nongs.tex
#   Data/cleaned/firstll/firstll_gs_est_v2.rds
#   Data/cleaned/firstll/firstll_nongs_est_v2.rds
#   Output_FirstLL/F03_dynamics_summary.md
#
# Dependencies: dplyr, fixest, here
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(fixest)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

gs  <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_gs_est.rds"))
ngs <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_nongs_est.rds"))
dose_data <- readRDS(file.path(FIRSTLL_CLEANED, "firstll_performance_dose.rds"))

gs  <- ensure_scaled(gs)
ngs <- ensure_scaled(ngs)

# Dose columns already merged in F00 — verify they exist
stopifnot("dose" %in% names(gs), "matches_won_dose" %in% names(gs))
stopifnot("dose" %in% names(ngs), "matches_won_dose" %in% names(ngs))

# Save updated skeletons
saveRDS(gs,  file.path(FIRSTLL_CLEANED, "firstll_gs_est_v2.rds"))
saveRDS(ngs, file.path(FIRSTLL_CLEANED, "firstll_nongs_est_v2.rds"))

# Split by tour
gs_atp  <- gs[gs$tour == "ATP", ]
gs_wta  <- gs[gs$tour == "WTA", ]
ngs_atp <- ngs[ngs$tour == "ATP", ]
ngs_wta <- ngs[ngs$tour == "WTA", ]

slog("GS-ATP: ", nrow(gs_atp), "  GS-WTA: ", nrow(gs_wta))
slog("NGS-ATP: ", nrow(ngs_atp), "  NGS-WTA: ", nrow(ngs_wta))

# Stack horizons
st_gs_atp  <- stack_horizons_full(gs_atp)
st_gs_wta  <- stack_horizons_full(gs_wta)
st_ngs_atp <- stack_horizons_full(ngs_atp)
st_ngs_wta <- stack_horizons_full(ngs_wta)


# ==============================================================================
# STEP 2: STACKED DYNAMIC MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: STACKED DYNAMIC MODELS")
message(strrep("=", 70))

run_dynamic <- function(stacked, fe_str, cf_term = NULL, label) {
  message("  ", label)
  results <- list()
  rho_results <- list()
  n_obs <- list()

  for (ob in outcomes_base) {
    sdata <- stacked[!is.na(stacked[[ob]]) &
                     !is.na(stacked$pre_rank_pts_s) &
                     !is.na(stacked$player_age), ]
    if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
    if (nrow(sdata) < 50) {
      message("    Skipping ", ob, " (N=", nrow(sdata), ")")
      next
    }

    rhs <- "got_ll:horizon"
    if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
    rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
    fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))

    mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                    error = function(e) { message("    Error: ", e$message); NULL })
    if (is.null(mod)) next

    n_obs[[ob]] <- nobs(mod)
    ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
    for (h in HORIZON_LABS) {
      tn <- paste0("got_ll:horizon", h)
      if (tn %in% ct$var) {
        rt <- ct[ct$var == tn, ]
        results[[paste0(ob, "_", h)]] <- data.frame(
          outcome = ob, horizon = h,
          coef = rt$Estimate, se = rt[["Std. Error"]],
          pval = rt[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
      rn <- paste0("v_hat:horizon", h)
      if (rn %in% ct$var) {
        rr <- ct[ct$var == rn, ]
        rho_results[[paste0(ob, "_", h)]] <- data.frame(
          outcome = ob, horizon = h,
          rho = rr$Estimate, rho_se = rr[["Std. Error"]],
          rho_pval = rr[["Pr(>|t|)"]], stringsAsFactors = FALSE)
      }
    }
  }
  list(results = do.call(rbind, results),
       rho = do.call(rbind, rho_results),
       n_obs = n_obs)
}

dyn_gs_atp  <- run_dynamic(st_gs_atp, "slam_year + horizon", NULL, "GS-ATP")
dyn_gs_wta  <- run_dynamic(st_gs_wta, "slam_year + horizon", NULL, "GS-WTA")
dyn_ngs_atp <- run_dynamic(st_ngs_atp, "tourney_id + horizon", "v_hat", "NGS-ATP")
dyn_ngs_wta <- run_dynamic(st_ngs_wta, "tourney_id + horizon", "v_hat", "NGS-WTA")


# ==============================================================================
# STEP 3: GENERATE DYNAMIC TABLES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: GENERATE TABLES")
message(strrep("=", 70))

make_dynamic_table <- function(res, rho_res = NULL) {
  oo <- c("points_change", "elo_change", "n_main_draws", "n_matches_250plus", "win_pct")
  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")
  for (ob in oo) {
    obr <- res[res$outcome == ob, ]
    if (nrow(obr) == 0) next
    cv <- sv <- character()
    for (h in HORIZON_LABS) {
      r <- obr[obr$horizon == h, ]
      if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
      else {
        d <- if (ob == "win_pct") 2 else 1
        cv <- c(cv, paste0(fmt(r$coef[1], d), add_stars(r$pval[1])))
        sv <- c(sv, paste0("(", fmt(r$se[1], d), ")"))
      }
    }
    L <- c(L, paste0(outcome_labels[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
    L <- c(L, paste0(" & ", paste(sv, collapse = " & "), " \\\\[0.3em]"))
  }
  if (!is.null(rho_res) && nrow(rho_res) > 0) {
    L <- c(L, "\\midrule")
    rv <- character()
    for (h in HORIZON_LABS) {
      rr <- rho_res[rho_res$outcome == "points_change" & rho_res$horizon == h, ]
      if (nrow(rr) == 0) rv <- c(rv, "")
      else rv <- c(rv, paste0(fmt(rr$rho[1], 2), add_stars(rr$rho_pval[1])))
    }
    L <- c(L, paste0("$\\hat{\\rho}_h$ (Pts) & ", paste(rv, collapse = " & "), " \\\\"))
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_dynamic_table(dyn_gs_atp$results),
           file.path(FIRSTLL_TABLES, "table_dynamic_stacked_atp.tex"))
writeLines(make_dynamic_table(dyn_gs_wta$results),
           file.path(FIRSTLL_TABLES, "table_dynamic_stacked_wta.tex"))
writeLines(make_dynamic_table(dyn_ngs_atp$results, dyn_ngs_atp$rho),
           file.path(FIRSTLL_TABLES, "table_dynamic_stacked_nongs_atp.tex"))
writeLines(make_dynamic_table(dyn_ngs_wta$results, dyn_ngs_wta$rho),
           file.path(FIRSTLL_TABLES, "table_dynamic_stacked_nongs_wta.tex"))
message("  Dynamic tables saved.")


# ==============================================================================
# STEP 4: HETEROGENEITY (rank, age — no had_prior_ll)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: HETEROGENEITY")
message(strrep("=", 70))

run_hetero <- function(stacked, fe_str, cf_term = NULL, label) {
  message("  ", label)
  # Split variables: rank and age (no had_prior_ll — no variation)
  # Rank: above vs below median
  if (!"rank_cat" %in% names(stacked)) {
    med_pts <- median(stacked$pre_rank_pts, na.rm = TRUE)
    stacked$rank_cat <- ifelse(stacked$pre_rank_pts >= med_pts, "high", "low")
  }
  if (!"age_cat" %in% names(stacked)) {
    stacked$age_cat <- ifelse(stacked$player_age < 24, "young", "old")
  }

  hetero_results <- list()

  for (dim in c("rank_cat", "age_cat")) {
    dim_label <- switch(dim, rank_cat = "Rank", age_cat = "Age")
    for (ob in c("points_change", "n_main_draws", "elo_change")) {
      sdata <- stacked[!is.na(stacked[[ob]]) &
                       !is.na(stacked$pre_rank_pts_s) &
                       !is.na(stacked$player_age) &
                       !is.na(stacked[[dim]]), ]
      if (!is.null(cf_term)) sdata <- sdata[!is.na(sdata$v_hat), ]
      if (nrow(sdata) < 50) next

      # Interaction: got_ll:horizon + got_ll:horizon:dim_cat
      sdata$dim_hi <- as.integer(sdata[[dim]] == levels(factor(sdata[[dim]]))[2])

      rhs <- paste0("got_ll:horizon + got_ll:horizon:dim_hi")
      if (!is.null(cf_term)) rhs <- paste0(rhs, " + v_hat:horizon")
      rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
      fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", fe_str))

      mod <- tryCatch(feols(fml, data = sdata, cluster = ~player_id),
                      error = function(e) NULL)
      if (is.null(mod)) next

      ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
      for (h in HORIZON_LABS) {
        # Base effect
        tn <- paste0("got_ll:horizon", h)
        if (tn %in% ct$var) {
          rt <- ct[ct$var == tn, ]
          hetero_results[[paste0(dim, "_", ob, "_base_", h)]] <- data.frame(
            dimension = dim_label, outcome = ob, horizon = h, type = "base",
            coef = rt$Estimate, se = rt[["Std. Error"]],
            pval = rt[["Pr(>|t|)"]], stringsAsFactors = FALSE)
        }
        # Interaction
        tn2 <- paste0("got_ll:horizon", h, ":dim_hi")
        if (tn2 %in% ct$var) {
          rt2 <- ct[ct$var == tn2, ]
          hetero_results[[paste0(dim, "_", ob, "_interact_", h)]] <- data.frame(
            dimension = dim_label, outcome = ob, horizon = h, type = "interaction",
            coef = rt2$Estimate, se = rt2[["Std. Error"]],
            pval = rt2[["Pr(>|t|)"]], stringsAsFactors = FALSE)
        }
      }
    }
  }
  do.call(rbind, hetero_results)
}

het_gs_atp  <- run_hetero(st_gs_atp, "slam_year + horizon", NULL, "GS-ATP hetero")
het_gs_wta  <- run_hetero(st_gs_wta, "slam_year + horizon", NULL, "GS-WTA hetero")
het_ngs_atp <- run_hetero(st_ngs_atp, "tourney_id + horizon", "v_hat", "NGS-ATP hetero")
het_ngs_wta <- run_hetero(st_ngs_wta, "tourney_id + horizon", "v_hat", "NGS-WTA hetero")

# Generate hetero tables (side-by-side: base + interaction)
make_hetero_table <- function(het_res) {
  if (is.null(het_res) || nrow(het_res) == 0) return("% No heterogeneity results")
  dims <- unique(het_res$dimension)
  oos <- unique(het_res$outcome)

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  for (d in dims) {
    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{", d, "}} \\\\"))
    for (ob in oos) {
      sub <- het_res[het_res$dimension == d & het_res$outcome == ob, ]
      base <- sub[sub$type == "base", ]
      inter <- sub[sub$type == "interaction", ]
      if (nrow(base) == 0) next

      # Base row
      cv <- sv <- character()
      for (h in HORIZON_LABS) {
        r <- base[base$horizon == h, ]
        if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
        else {
          cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$pval[1])))
          sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")"))
        }
      }
      L <- c(L, paste0("  ", outcome_labels[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv, collapse = " & "), " \\\\"))

      # Interaction row
      if (nrow(inter) > 0) {
        cv2 <- sv2 <- character()
        for (h in HORIZON_LABS) {
          r2 <- inter[inter$horizon == h, ]
          if (nrow(r2) == 0) { cv2 <- c(cv2, ""); sv2 <- c(sv2, "") }
          else {
            cv2 <- c(cv2, paste0(fmt(r2$coef[1], 1), add_stars(r2$pval[1])))
            sv2 <- c(sv2, paste0("(", fmt(r2$se[1], 1), ")"))
          }
        }
        L <- c(L, paste0("  $\\times$ High & ", paste(cv2, collapse = " & "), " \\\\"))
        L <- c(L, paste0("  & ", paste(sv2, collapse = " & "), " \\\\[0.3em]"))
      }
    }
    L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_hetero_table(het_gs_atp),
           file.path(FIRSTLL_TABLES, "table_hetero_stacked_atp.tex"))
writeLines(make_hetero_table(het_gs_wta),
           file.path(FIRSTLL_TABLES, "table_hetero_stacked_wta.tex"))
writeLines(make_hetero_table(het_ngs_atp),
           file.path(FIRSTLL_TABLES, "table_hetero_stacked_nongs_atp.tex"))
writeLines(make_hetero_table(het_ngs_wta),
           file.path(FIRSTLL_TABLES, "table_hetero_stacked_nongs_wta.tex"))
message("  Heterogeneity tables saved.")


# ==============================================================================
# STEP 5: DOSE MODELS (matches_won and -logit(π))
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: DOSE MODELS")
message(strrep("=", 70))

run_dose <- function(stacked_list, fe_str_gs, fe_str_ngs, dose_var, dose_label) {
  results <- list()
  samples <- list(
    "GS-ATP"  = list(data = stacked_list$gs_atp,  fe = fe_str_gs,  cf = NULL),
    "GS-WTA"  = list(data = stacked_list$gs_wta,  fe = fe_str_gs,  cf = NULL),
    "NGS-ATP" = list(data = stacked_list$ngs_atp, fe = fe_str_ngs, cf = "v_hat"),
    "NGS-WTA" = list(data = stacked_list$ngs_wta, fe = fe_str_ngs, cf = "v_hat")
  )

  for (sname in names(samples)) {
    s <- samples[[sname]]
    sdata <- s$data
    if (!dose_var %in% names(sdata)) next
    # Only treated players have meaningful dose
    sdata$dose_var <- sdata[[dose_var]]
    sdata$dose_var[sdata$got_ll == 0] <- 0

    for (ob in c("points_change", "n_main_draws", "elo_change")) {
      sd <- sdata[!is.na(sdata[[ob]]) & !is.na(sdata$pre_rank_pts_s) &
                  !is.na(sdata$player_age) & !is.na(sdata$dose_var), ]
      if (!is.null(s$cf)) sd <- sd[!is.na(sd$v_hat), ]
      if (nrow(sd) < 50) next

      rhs <- "got_ll:horizon + got_ll:horizon:dose_var"
      if (!is.null(s$cf)) rhs <- paste0(rhs, " + v_hat:horizon")
      rhs <- paste0(rhs, " + ", ZPRE_FIRSTLL)
      fml <- as.formula(paste0(ob, " ~ ", rhs, " | ", s$fe))

      mod <- tryCatch(feols(fml, data = sd, cluster = ~player_id),
                      error = function(e) NULL)
      if (is.null(mod)) next

      ct <- as.data.frame(coeftable(mod)); ct$var <- rownames(ct)
      for (h in HORIZON_LABS) {
        tn <- paste0("got_ll:horizon", h)
        if (tn %in% ct$var) {
          rt <- ct[ct$var == tn, ]
          results[[paste0(sname, "_", ob, "_base_", h)]] <- data.frame(
            sample = sname, outcome = ob, horizon = h, type = "base",
            coef = rt$Estimate, se = rt[["Std. Error"]],
            pval = rt[["Pr(>|t|)"]], stringsAsFactors = FALSE)
        }
        dn <- paste0("got_ll:horizon", h, ":dose_var")
        if (dn %in% ct$var) {
          rd <- ct[ct$var == dn, ]
          results[[paste0(sname, "_", ob, "_dose_", h)]] <- data.frame(
            sample = sname, outcome = ob, horizon = h, type = "dose",
            coef = rd$Estimate, se = rd[["Std. Error"]],
            pval = rd[["Pr(>|t|)"]], stringsAsFactors = FALSE)
        }
      }
    }
  }
  do.call(rbind, results)
}

stacked_list <- list(gs_atp = st_gs_atp, gs_wta = st_gs_wta,
                     ngs_atp = st_ngs_atp, ngs_wta = st_ngs_wta)

dose_mw <- run_dose(stacked_list, "slam_year + horizon", "tourney_id + horizon",
                    "matches_won_dose", "Matches Won")
dose_pp <- run_dose(stacked_list, "slam_year + horizon", "tourney_id + horizon",
                    "dose", "-logit(π)")

# Generate dose tables
make_dose_table <- function(dose_res, gs_only = TRUE) {
  if (is.null(dose_res) || nrow(dose_res) == 0) return("% No dose results")
  if (gs_only) {
    dose_res <- dose_res[grepl("^GS-", dose_res$sample), ]
  } else {
    dose_res <- dose_res[grepl("^NGS-", dose_res$sample), ]
  }
  if (nrow(dose_res) == 0) return("% No results for this sample")

  samples <- unique(dose_res$sample)
  oos <- unique(dose_res$outcome)

  L <- c("\\begin{tabular}{l*{5}{c}}", "\\toprule",
         paste0(" & ", paste(HORIZON_LABS, collapse = " & "), " \\\\"), "\\midrule")

  for (s in samples) {
    L <- c(L, paste0("\\multicolumn{6}{l}{\\textit{", s, "}} \\\\"))
    for (ob in oos) {
      sub <- dose_res[dose_res$sample == s & dose_res$outcome == ob, ]
      base <- sub[sub$type == "base", ]
      dose <- sub[sub$type == "dose", ]
      if (nrow(base) == 0) next

      cv <- sv <- character()
      for (h in HORIZON_LABS) {
        r <- base[base$horizon == h, ]
        if (nrow(r) == 0) { cv <- c(cv, ""); sv <- c(sv, "") }
        else { cv <- c(cv, paste0(fmt(r$coef[1], 1), add_stars(r$pval[1])))
               sv <- c(sv, paste0("(", fmt(r$se[1], 1), ")")) }
      }
      L <- c(L, paste0("  ", outcome_labels[ob], " & ", paste(cv, collapse = " & "), " \\\\"))
      L <- c(L, paste0("  & ", paste(sv, collapse = " & "), " \\\\"))

      if (nrow(dose) > 0) {
        cv2 <- sv2 <- character()
        for (h in HORIZON_LABS) {
          r2 <- dose[dose$horizon == h, ]
          if (nrow(r2) == 0) { cv2 <- c(cv2, ""); sv2 <- c(sv2, "") }
          else { cv2 <- c(cv2, paste0(fmt(r2$coef[1], 1), add_stars(r2$pval[1])))
                 sv2 <- c(sv2, paste0("(", fmt(r2$se[1], 1), ")")) }
        }
        L <- c(L, paste0("  $\\times$ Dose & ", paste(cv2, collapse = " & "), " \\\\"))
        L <- c(L, paste0("  & ", paste(sv2, collapse = " & "), " \\\\[0.3em]"))
      }
    }
    L <- c(L, "\\midrule")
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}")
  paste(L, collapse = "\n")
}

writeLines(make_dose_table(dose_mw, TRUE),
           file.path(FIRSTLL_TABLES, "table_dose_stacked.tex"))
writeLines(make_dose_table(dose_mw, FALSE),
           file.path(FIRSTLL_TABLES, "table_dose_stacked_nongs.tex"))
writeLines(make_dose_table(dose_pp, TRUE),
           file.path(FIRSTLL_TABLES, "table_perf_prob_stacked.tex"))
writeLines(make_dose_table(dose_pp, FALSE),
           file.path(FIRSTLL_TABLES, "table_perf_prob_stacked_nongs.tex"))
message("  Dose tables saved.")


# ==============================================================================
# STEP 6: LOG SUMMARY
# ==============================================================================

# Log key results
for (lbl in c("GS-ATP", "GS-WTA", "NGS-ATP", "NGS-WTA")) {
  res <- switch(lbl,
    "GS-ATP" = dyn_gs_atp, "GS-WTA" = dyn_gs_wta,
    "NGS-ATP" = dyn_ngs_atp, "NGS-WTA" = dyn_ngs_wta
  )
  slog("\n--- ", lbl, " ---")
  if (!is.null(res$results) && nrow(res$results) > 0) {
    for (ob in c("points_change", "elo_change", "n_main_draws")) {
      sub <- res$results[res$results$outcome == ob, ]
      if (nrow(sub) == 0) next
      for (i in seq_len(nrow(sub))) {
        r <- sub[i, ]
        star <- ifelse(r$pval < 0.01, "***",
                  ifelse(r$pval < 0.05, "**",
                    ifelse(r$pval < 0.1, "*", "")))
        slog(sprintf("  %s %s: %.2f (%.2f) p=%.3f %s",
                     ob, r$horizon, r$coef, r$se, r$pval, star))
      }
    }
  }
}

writeLines(summary_log, file.path(FIRSTLL_OUTPUT, "F03_dynamics_summary.md"))
message("\nSummary saved to Output_FirstLL/F03_dynamics_summary.md")
message("DONE.")
