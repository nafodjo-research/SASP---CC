# ==============================================================================
# 36_performance_dose.R
# Estimate match-winning logit on main-draw matches at LL-granting events,
# then compute performance probability dose for each LL awardee.
#
# dose_i = -log(pi_ie) where pi_ie = prod P(observed match outcome)
# Higher dose = more surprising performance = stronger confidence boost
# Controls get dose = 0.
#
# Four models: GS-ATP, GS-WTA, nonGS-ATP, nonGS-WTA
# Same covariate spec as 35d (via win_model_helpers.R)
#
# Outputs:
#   Data/cleaned/win_model_md_gs_atp.rds
#   Data/cleaned/win_model_md_gs_wta.rds
#   Data/cleaned/win_model_md_nongs_atp.rds
#   Data/cleaned/win_model_md_nongs_wta.rds
#   Data/cleaned/performance_dose.rds
#   Tables/table_win_model_main_draw.tex
# ==============================================================================

set.seed(20260327)

library(dplyr)
library(data.table)
library(here)

source(here("scripts", "R", "utils.R"))
source(here("scripts", "R", "win_model_helpers.R"))
summary_log <- character()

RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLES_DIR  <- here("Tables")
OUTPUT_DIR  <- here("Output")
for (d in c(CLEANED_DIR, TABLES_DIR, OUTPUT_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 1: LOAD DATA")
message(strrep("=", 70))

atp_md <- readRDS(file.path(RAW_DIR, "atp_main_matches.rds"))
wta_md <- readRDS(file.path(RAW_DIR, "wta_main_matches.rds"))

atp_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "elo_history.rds")))
wta_elo <- as.data.table(readRDS(file.path(CLEANED_DIR, "wta_elo_history.rds")))
setnames(atp_elo, c("player_id", "match_date", "elo"))
setnames(wta_elo, c("player_id", "match_date", "elo"))
setkey(atp_elo, player_id, match_date)
setkey(wta_elo, player_id, match_date)

elo_cache <- readRDS(file.path(CLEANED_DIR, "tournament_elo_cache.rds"))

# Event tables
tr <- readRDS(file.path(CLEANED_DIR, "tournament_rebuild_results.rds"))
nongs_est <- readRDS(file.path(CLEANED_DIR, "skeleton_nongs_est_v6.rds"))

# Collect tourney_ids for each sample
gs_atp_tids <- unique(tr$gs_atp$events$tourney_id)
gs_wta_tids <- unique(tr$gs_wta$events$tourney_id)
nongs_atp_tids <- unique(nongs_est$tourney_id[nongs_est$tour == "ATP"])
nongs_wta_tids <- unique(nongs_est$tourney_id[nongs_est$tour == "WTA"])

message("  GS-ATP events: ", length(gs_atp_tids))
message("  GS-WTA events: ", length(gs_wta_tids))
message("  NonGS-ATP events: ", length(nongs_atp_tids))
message("  NonGS-WTA events: ", length(nongs_wta_tids))


# ==============================================================================
# STEP 2: FILTER MAIN-DRAW MATCHES AT LL-GRANTING EVENTS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 2: FILTER MAIN-DRAW MATCHES")
message(strrep("=", 70))

# Only keep actual main draw rounds (not qualifying rounds that might be in main file)
md_rounds <- c("R128", "R64", "R32", "R16", "QF", "SF", "F", "RR", "BR", "ER")

gs_atp_matches <- atp_md[atp_md$tourney_id %in% gs_atp_tids &
                          atp_md$round %in% md_rounds, ]
gs_atp_matches$tour <- "ATP"

gs_wta_matches <- wta_md[wta_md$tourney_id %in% gs_wta_tids &
                          wta_md$round %in% md_rounds, ]
gs_wta_matches$tour <- "WTA"

nongs_atp_matches <- atp_md[atp_md$tourney_id %in% nongs_atp_tids &
                             atp_md$round %in% md_rounds, ]
nongs_atp_matches$tour <- "ATP"

nongs_wta_matches <- wta_md[wta_md$tourney_id %in% nongs_wta_tids &
                             wta_md$round %in% md_rounds, ]
nongs_wta_matches$tour <- "WTA"

message("  GS-ATP main draw matches: ", nrow(gs_atp_matches))
message("  GS-WTA main draw matches: ", nrow(gs_wta_matches))
message("  NonGS-ATP main draw matches: ", nrow(nongs_atp_matches))
message("  NonGS-WTA main draw matches: ", nrow(nongs_wta_matches))

rm(atp_md, wta_md)
gc()


# ==============================================================================
# STEP 3: MERGE ELO + H2H FOR EACH SAMPLE
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 3: MERGE ELO AND H2H")
message(strrep("=", 70))

# Build H2H indices (one per tour, from all match sources)
message("  Building ATP H2H index...")
atp_h2h_idx <- build_h2h_index_multi(c(
  file.path(RAW_DIR, "atp_main_matches.rds"),
  file.path(RAW_DIR, "atp_qual_chall_matches.rds")
))
message("  Building WTA H2H index...")
wta_h2h_idx <- build_h2h_index_multi(c(
  file.path(RAW_DIR, "wta_main_matches.rds"),
  file.path(RAW_DIR, "wta_qual_itf_matches.rds")
))

process_sample <- function(df, elo_dt, h2h_idx, tour_prefix, label) {
  message("\n  --- ", label, " ---")

  # Add round_num for weighting (main draw rounds)
  round_map <- c("R128"=1, "R64"=2, "R32"=3, "R16"=4, "QF"=5, "SF"=6, "F"=7, "RR"=1, "BR"=1, "ER"=1)
  df$round_num <- round_map[df$round]
  df$round_num[is.na(df$round_num)] <- 3L  # default

  # Weights
  df$total_pts <- df$w_svpt + df$l_svpt
  has_pts <- !is.na(df$total_pts)
  df$weight <- ifelse(has_pts, df$total_pts, df$round_num)
  df$weight <- df$weight / mean(df$weight, na.rm = TRUE)
  df$weight[is.na(df$weight)] <- 1.0
  message("    Matches with service pts: ", sum(has_pts), " / ", nrow(df))

  # Elo
  df <- merge_elo(df, elo_dt, tour_prefix)

  # H2H
  message("    Computing H2H...")
  h2h <- compute_h2h(df$winner_id, df$loser_id, df$match_date, h2h_idx)
  df$h2h_win_prop <- h2h$h2h_prop
  df$h2h_count <- h2h$h2h_count

  df
}

gs_atp_matches <- process_sample(gs_atp_matches, atp_elo, atp_h2h_idx, "ATP", "GS-ATP")
gs_wta_matches <- process_sample(gs_wta_matches, wta_elo, wta_h2h_idx, "WTA", "GS-WTA")
nongs_atp_matches <- process_sample(nongs_atp_matches, atp_elo, atp_h2h_idx, "ATP", "NonGS-ATP")
nongs_wta_matches <- process_sample(nongs_wta_matches, wta_elo, wta_h2h_idx, "WTA", "NonGS-WTA")

rm(atp_h2h_idx, wta_h2h_idx)
gc()


# ==============================================================================
# STEP 4: ESTIMATE 4 WEIGHTED LOGIT MODELS
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 4: ESTIMATE LOGIT MODELS")
message(strrep("=", 70))

fit_model <- function(df, label) {
  est <- build_est_data(df)
  est <- est[complete.cases(est[, c("elo_diff", "surface_elo_diff")]), ]
  message("  ", label, ": N = ", nrow(est))
  mod <- glm(LOGIT_FORMULA, data = est, family = binomial(link = "logit"), weights = weight)
  message("    AIC: ", round(AIC(mod)))
  mod
}

model_gs_atp <- fit_model(gs_atp_matches, "GS-ATP")
model_gs_wta <- fit_model(gs_wta_matches, "GS-WTA")
model_nongs_atp <- fit_model(nongs_atp_matches, "NonGS-ATP")
model_nongs_wta <- fit_model(nongs_wta_matches, "NonGS-WTA")

saveRDS(model_gs_atp, file.path(CLEANED_DIR, "win_model_md_gs_atp.rds"))
saveRDS(model_gs_wta, file.path(CLEANED_DIR, "win_model_md_gs_wta.rds"))
saveRDS(model_nongs_atp, file.path(CLEANED_DIR, "win_model_md_nongs_atp.rds"))
saveRDS(model_nongs_wta, file.path(CLEANED_DIR, "win_model_md_nongs_wta.rds"))
message("  Models saved.")

slog("## Step 4: Models estimated")
slog("- GS-ATP: N=", nobs(model_gs_atp), " AIC=", round(AIC(model_gs_atp)))
slog("- GS-WTA: N=", nobs(model_gs_wta), " AIC=", round(AIC(model_gs_wta)))
slog("- NonGS-ATP: N=", nobs(model_nongs_atp), " AIC=", round(AIC(model_nongs_atp)))
slog("- NonGS-WTA: N=", nobs(model_nongs_wta), " AIC=", round(AIC(model_nongs_wta)))


# ==============================================================================
# STEP 5: SUMMARY TABLE (GS: ATP|WTA, NonGS: ATP|WTA)
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 5: SUMMARY TABLES")
message(strrep("=", 70))

gs_table <- generate_logit_table(model_gs_atp, model_gs_wta)
writeLines(gs_table, file.path(TABLES_DIR, "table_win_model_main_draw_gs.tex"))

nongs_table <- generate_logit_table(model_nongs_atp, model_nongs_wta)
writeLines(nongs_table, file.path(TABLES_DIR, "table_win_model_main_draw_nongs.tex"))
message("  Tables saved.")


# ==============================================================================
# STEP 6: COMPUTE PERFORMANCE PROBABILITY DOSE FOR LL AWARDEES
# ==============================================================================
message("\n", strrep("=", 70))
message("STEP 6: COMPUTE PERFORMANCE DOSE")
message(strrep("=", 70))

# For each LL awardee, identify their main-draw matches at the focal event,
# predict P(win) for each, compute pi_ie = prod P(observed outcome), dose = -log(pi_ie)

compute_dose <- function(event_df, match_df, model, tour_label, event_type) {
  # event_df: event table with got_ll, player_id, tourney_id
  # match_df: main draw matches with Elo/H2H already merged

  ll_events <- event_df[event_df$got_ll == 1, ]
  message("  ", tour_label, " ", event_type, ": ", nrow(ll_events), " LL awardees")

  dose_results <- list()

  for (i in seq_len(nrow(ll_events))) {
    pid <- ll_events$player_id[i]
    tid <- ll_events$tourney_id[i]

    # Find this player's matches at this tournament in the main draw
    # Player could be winner or loser
    player_matches <- match_df[match_df$tourney_id == tid &
                                (match_df$winner_id == pid | match_df$loser_id == pid), ]

    if (nrow(player_matches) == 0) {
      # LL player did not appear in main draw data (possibly walkover or data gap)
      dose_results[[i]] <- data.frame(
        player_id = pid, tourney_id = tid, tour = tour_label,
        event_type = event_type, n_md_matches = 0L,
        pi_ie = NA_real_, dose = 0, matches_won = 0L,
        stringsAsFactors = FALSE
      )
      next
    }

    # For each match, compute P(focal player wins) from the FOCAL PLAYER's perspective
    # Then determine if the observed outcome matches
    match_probs <- numeric(nrow(player_matches))
    match_won <- integer(nrow(player_matches))

    for (j in seq_len(nrow(player_matches))) {
      m <- player_matches[j, ]
      focal_is_winner <- (m$winner_id == pid)

      if (focal_is_winner) {
        # Focal player won this match → predict from winner perspective directly
        p_focal_wins <- predict_match_probs(m, model)
        match_probs[j] <- p_focal_wins
        match_won[j] <- 1L
      } else {
        # Focal player lost → they are the loser; P(focal wins) = 1 - P(winner wins)
        p_winner_wins <- predict_match_probs(m, model)
        match_probs[j] <- 1 - p_winner_wins
        match_won[j] <- 0L
      }
    }

    # pi_ie = product of P(observed outcome)
    # If focal won: contribution = P(focal wins)
    # If focal lost: contribution = 1 - P(focal wins) = P(focal loses)
    contributions <- ifelse(match_won == 1L, match_probs, 1 - match_probs)
    pi_ie <- prod(contributions)
    dose_val <- -log(max(pi_ie, 1e-300))  # avoid log(0)

    dose_results[[i]] <- data.frame(
      player_id = pid, tourney_id = tid, tour = tour_label,
      event_type = event_type, n_md_matches = nrow(player_matches),
      pi_ie = pi_ie, dose = dose_val, matches_won = sum(match_won),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(dose_results)
}

# GS samples: event tables from tournament_rebuild_results
dose_gs_atp <- compute_dose(tr$gs_atp$events, gs_atp_matches, model_gs_atp, "ATP", "GS")
dose_gs_wta <- compute_dose(tr$gs_wta$events, gs_wta_matches, model_gs_wta, "WTA", "GS")

# NonGS samples: event tables from skeleton
nongs_atp_events <- nongs_est[nongs_est$tour == "ATP", c("player_id", "tourney_id", "got_ll")]
nongs_wta_events <- nongs_est[nongs_est$tour == "WTA", c("player_id", "tourney_id", "got_ll")]

dose_nongs_atp <- compute_dose(nongs_atp_events, nongs_atp_matches, model_nongs_atp, "ATP", "nonGS")
dose_nongs_wta <- compute_dose(nongs_wta_events, nongs_wta_matches, model_nongs_wta, "WTA", "nonGS")

# Combine all dose data
all_dose <- bind_rows(dose_gs_atp, dose_gs_wta, dose_nongs_atp, dose_nongs_wta)

# Add dose = 0 for controls (non-LL players)
# Controls are in event tables but with got_ll == 0
control_gs_atp <- tr$gs_atp$events[tr$gs_atp$events$got_ll == 0,
                                    c("player_id", "tourney_id")]
control_gs_atp$tour <- "ATP"; control_gs_atp$event_type <- "GS"
control_gs_atp$n_md_matches <- 0L; control_gs_atp$pi_ie <- 1
control_gs_atp$dose <- 0; control_gs_atp$matches_won <- 0L

control_gs_wta <- tr$gs_wta$events[tr$gs_wta$events$got_ll == 0,
                                    c("player_id", "tourney_id")]
control_gs_wta$tour <- "WTA"; control_gs_wta$event_type <- "GS"
control_gs_wta$n_md_matches <- 0L; control_gs_wta$pi_ie <- 1
control_gs_wta$dose <- 0; control_gs_wta$matches_won <- 0L

control_nongs_atp <- nongs_est[nongs_est$tour == "ATP" & nongs_est$got_ll == 0,
                                c("player_id", "tourney_id")]
control_nongs_atp$tour <- "ATP"; control_nongs_atp$event_type <- "nonGS"
control_nongs_atp$n_md_matches <- 0L; control_nongs_atp$pi_ie <- 1
control_nongs_atp$dose <- 0; control_nongs_atp$matches_won <- 0L

control_nongs_wta <- nongs_est[nongs_est$tour == "WTA" & nongs_est$got_ll == 0,
                                c("player_id", "tourney_id")]
control_nongs_wta$tour <- "WTA"; control_nongs_wta$event_type <- "nonGS"
control_nongs_wta$n_md_matches <- 0L; control_nongs_wta$pi_ie <- 1
control_nongs_wta$dose <- 0; control_nongs_wta$matches_won <- 0L

all_dose <- bind_rows(all_dose,
                      control_gs_atp, control_gs_wta,
                      control_nongs_atp, control_nongs_wta)

message("\n  === Dose Summary ===")
for (et in c("GS", "nonGS")) {
  for (t in c("ATP", "WTA")) {
    sub <- all_dose[all_dose$event_type == et & all_dose$tour == t, ]
    treated <- sub[sub$dose > 0, ]
    message(sprintf("  %s-%s: N=%d total, %d treated, mean dose=%.2f, median=%.2f",
                    et, t, nrow(sub), nrow(treated),
                    mean(treated$dose, na.rm = TRUE),
                    median(treated$dose, na.rm = TRUE)))
  }
}

saveRDS(all_dose, file.path(CLEANED_DIR, "performance_dose.rds"))
message("  Saved: Data/cleaned/performance_dose.rds")

slog("## Step 6: Performance dose computed")
slog("- Total dose observations: ", nrow(all_dose))
slog("- Treated with dose > 0: ", sum(all_dose$dose > 0, na.rm = TRUE))
slog("")

# Save summary log
writeLines(summary_log, file.path(OUTPUT_DIR, "36_performance_dose_log.md"))

message("\n", strrep("=", 70))
message("DONE: Script 36 complete")
message(strrep("=", 70))
