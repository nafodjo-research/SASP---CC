# ==============================================================================
# 12_tournament_performance.R
# Sequential Bernoulli Tournament Model: CF-IV estimation of causal effect
# of Lucky Loser entry on match-level winning probability
#
# Methodology:
#   - First stage: Probit for LL entry, instrumented by P(LL opportunity)
#   - Second stage: Match-level logit with generalized residual (control function)
#   - Stacked event-cohort design with pre-treatment frozen covariates
#   - Counterfactual tournament simulation for derived treatment effects
#
# Inputs:
#   Data/raw/atp_main_matches.rds, atp_qual_chall_matches.rds
#   Data/raw/wta_main_matches.rds, wta_qual_itf_matches.rds
#   Data/cleaned/elo_history.rds (optional, for speed)
#
# Outputs:
#   Data/cleaned/tournament_model_results.rds
#   Data/cleaned/tournament_model_event_table.rds
#   Data/cleaned/tournament_model_match_data.rds
#   Tables/table_tournament_first_stage.tex
#   Tables/table_tournament_match_effects.tex
#   Tables/table_tournament_effects.tex
#   Tables/table_tournament_heterogeneity.tex
#   Figures/fig_dynamic_effects.pdf
#   Figures/fig_tournament_effects.pdf
#   Output/tournament_model_summary.md
#
# Dependencies: dplyr, tidyr, readr, ggplot2, here, patchwork, knitr, kableExtra
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)
library(patchwork)
library(stringr)
library(data.table)

set.seed(20260321)

# -- Project paths -----------------------------------------------------------
RAW_DIR     <- here("Data", "raw")
CLEANED_DIR <- here("Data", "cleaned")
TABLE_DIR   <- here("Tables")
FIG_DIR     <- here("Figures")
OUTPUT_DIR  <- here("Output")

for (d in c(CLEANED_DIR, TABLE_DIR, FIG_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -- Analysis parameters -----------------------------------------------------
T_HORIZON        <- 10L       # Max subsequent tournaments to track
CALENDAR_CAP     <- 365L      # Max days after qualifying loss
TRUNCATION_MODE  <- "at_next_event"
YEARS            <- 2000:2024
N_BOOT           <- 50L       # Bootstrap replications (increase for production)

# -- Custom theme (consistent with 07_figures.R) ------------------------------
theme_paper <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.3, color = "grey30"),
      axis.ticks.length = unit(2, "pt"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(18, "pt"),
      plot.margin      = margin(8, 12, 8, 8, unit = "pt"),
      strip.text       = element_text(face = "bold", size = base_size - 1)
    )
}

col_treat   <- "#E69F00"
col_control <- "#56B4E9"


###############################################################################
# SECTION 1: DATA LOADING
###############################################################################

message("================================================================")
message("  Lucky Loser CF-IV: Sequential Bernoulli Tournament Model")
message("================================================================")

message("\n[1/12] Loading match data...")

atp_main <- read_rds(file.path(RAW_DIR, "atp_main_matches.rds"))
atp_qual <- read_rds(file.path(RAW_DIR, "atp_qual_chall_matches.rds"))
wta_main <- read_rds(file.path(RAW_DIR, "wta_main_matches.rds"))
wta_qual <- read_rds(file.path(RAW_DIR, "wta_qual_itf_matches.rds"))

# Harmonize column names and combine ATP + WTA
harmonize_matches <- function(main_df, qual_df, tour_label) {
  # Ensure consistent column types
  ensure_cols <- function(df) {
    # Make sure critical columns exist
    if (!"winner_entry" %in% names(df)) df$winner_entry <- NA_character_
    if (!"loser_entry" %in% names(df)) df$loser_entry <- NA_character_
    if (!"best_of" %in% names(df)) df$best_of <- NA_integer_
    if (!"winner_age" %in% names(df)) df$winner_age <- NA_real_
    if (!"loser_age" %in% names(df)) df$loser_age <- NA_real_
    if (!"match_num" %in% names(df)) df$match_num <- seq_len(nrow(df))
    if (!"draw_size" %in% names(df)) df$draw_size <- NA_integer_

    # Ensure winner_id and loser_id exist
    if (!"winner_id" %in% names(df)) df$winner_id <- NA_integer_
    if (!"loser_id" %in% names(df)) df$loser_id <- NA_integer_

    df
  }

  main_df <- ensure_cols(main_df) |> mutate(match_source = "main")
  qual_df <- ensure_cols(qual_df) |> mutate(match_source = "qual")

  # Get common columns
  common_cols <- intersect(names(main_df), names(qual_df))

  combined <- bind_rows(
    main_df |> select(all_of(common_cols)),
    qual_df |> select(all_of(common_cols))
  ) |>
    mutate(
      tour = tour_label,
      # Parse tourney_date
      tourney_date = if (inherits(tourney_date, "Date")) {
        tourney_date
      } else {
        as.Date(as.character(tourney_date), format = "%Y%m%d")
      },
      year = as.integer(format(tourney_date, "%Y"))
    ) |>
    filter(year >= min(YEARS), year <= max(YEARS))

  combined
}

atp_all <- harmonize_matches(atp_main, atp_qual, "ATP")
wta_all <- harmonize_matches(wta_main, wta_qual, "WTA")

# Combine all matches
matches <- bind_rows(atp_all, wta_all) |>
  arrange(tourney_date, tourney_id, match_num)

# Create a unified player identifier
# Use winner_id/loser_id where available, fall back to name
matches <- matches |>
  mutate(
    winner_pid = if_else(!is.na(winner_id) & winner_id != 0,
                         paste0(tour, "_", winner_id),
                         paste0(tour, "_name_", winner_name)),
    loser_pid  = if_else(!is.na(loser_id) & loser_id != 0,
                         paste0(tour, "_", loser_id),
                         paste0(tour, "_name_", loser_name))
  )

message("  ATP matches: ", nrow(atp_all))
message("  WTA matches: ", nrow(wta_all))
message("  Combined: ", nrow(matches), " matches across ",
        length(unique(matches$tourney_id)), " tournaments")

# Clean up large objects
rm(atp_main, atp_qual, wta_main, wta_qual, atp_all, wta_all)
gc()


###############################################################################
# SECTION 2: ELO RATING ENGINE
###############################################################################

message("\n[2/12] Computing Elo ratings...")

# We compute our own Elo here using the v3 engine approach (name-based hashed envs)
# but adapted to use our unified player IDs. This gives us overall + surface Elo.

compute_elo <- function(match_data) {
  # -----------------------------------------------------------------------
  # OPTIMIZED: Instead of rbind-ing data.frames inside environment entries
  # on every match (O(n^2) memory allocation), we store ratings as growing
  # vectors using a list-of-lists approach, then convert at the end.
  # -----------------------------------------------------------------------
  elo_overall  <- new.env(hash = TRUE)
  elo_hard     <- new.env(hash = TRUE)
  elo_clay     <- new.env(hash = TRUE)
  elo_grass    <- new.env(hash = TRUE)
  match_counts <- new.env(hash = TRUE)

  # Store current rating only (scalar) for speed; build history separately
  cur_overall  <- new.env(hash = TRUE)
  cur_hard     <- new.env(hash = TRUE)
  cur_clay     <- new.env(hash = TRUE)
  cur_grass    <- new.env(hash = TRUE)

  INIT <- 1500

  # History storage: lists of (rating, date) pairs, batched
  hist_overall <- new.env(hash = TRUE)
  hist_hard    <- new.env(hash = TRUE)
  hist_clay    <- new.env(hash = TRUE)
  hist_grass   <- new.env(hash = TRUE)

  get_cur <- function(env, p) {
    r <- env[[p]]
    if (is.null(r)) INIT else r
  }

  get_n <- function(p) {
    n <- match_counts[[p]]
    if (is.null(n)) 0L else n
  }

  append_hist <- function(hist_env, p, rating, d) {
    existing <- hist_env[[p]]
    if (is.null(existing)) {
      hist_env[[p]] <- list(rating = rating, date = d)
    } else {
      hist_env[[p]] <- list(
        rating = c(existing$rating, rating),
        date   = c(existing$date, d)
      )
    }
  }

  update_pair <- function(cur_env, hist_env, pA, pB, winner, lvl, d) {
    rA <- get_cur(cur_env, pA); rB <- get_cur(cur_env, pB)

    eA <- 1 / (1 + 10^((rB - rA) / 400))
    sA <- as.numeric(winner == pA)

    kA <- 250 / (get_n(pA) + 5)^0.4
    kB <- 250 / (get_n(pB) + 5)^0.4
    km <- if (!is.na(lvl) && lvl == "G") 1.1 else 1.0

    rA_new <- rA + km * kA * (sA - eA)
    rB_new <- rB + km * kB * ((1 - sA) - (1 - eA))

    cur_env[[pA]] <- rA_new
    cur_env[[pB]] <- rB_new

    append_hist(hist_env, pA, rA_new, d)
    append_hist(hist_env, pB, rB_new, d)
  }

  surf_env_pair <- function(s) {
    if (is.na(s)) return(NULL)
    s <- tolower(trimws(s))
    switch(s,
      hard  = list(cur = cur_hard, hist = hist_hard),
      clay  = list(cur = cur_clay, hist = hist_clay),
      grass = list(cur = cur_grass, hist = hist_grass),
      NULL
    )
  }

  n_total <- nrow(match_data)
  report_every <- 100000L

  for (i in seq_len(n_total)) {
    if (i %% report_every == 0) message("    Elo: ", i, " / ", n_total)

    r <- match_data[i, ]
    w <- r$winner_pid; l <- r$loser_pid
    if (is.na(w) || is.na(l) || w == "" || l == "") next

    match_counts[[w]] <- get_n(w) + 1L
    match_counts[[l]] <- get_n(l) + 1L

    update_pair(cur_overall, hist_overall, w, l, w, r$tourney_level, r$tourney_date)
    se <- surf_env_pair(r$surface)
    if (!is.null(se)) update_pair(se$cur, se$hist, w, l, w, r$tourney_level, r$tourney_date)
  }

  # Convert history lists to data.frames for lookup
  convert_hist <- function(hist_env) {
    out <- new.env(hash = TRUE)
    for (p in ls(hist_env)) {
      h <- hist_env[[p]]
      out[[p]] <- data.frame(rating = h$rating, date = h$date,
                              stringsAsFactors = FALSE)
    }
    out
  }

  list(overall = convert_hist(hist_overall),
       hard    = convert_hist(hist_hard),
       clay    = convert_hist(hist_clay),
       grass   = convert_hist(hist_grass),
       counts  = match_counts)
}

get_elo_at_date <- function(env, player, date) {
  df <- env[[player]]
  if (is.null(df)) return(1500)
  v <- df[df$date <= date, ]
  if (nrow(v) == 0) return(1500)
  tail(v$rating, 1)
}

get_blended_surface_elo <- function(elo_list, player, surface, date) {
  ov <- get_elo_at_date(elo_list$overall, player, date)
  if (is.na(surface)) return(ov)
  se <- switch(tolower(trimws(surface)),
               hard  = elo_list$hard,
               clay  = elo_list$clay,
               grass = elo_list$grass,
               NULL)
  if (is.null(se)) return(ov)
  0.5 * ov + 0.5 * get_elo_at_date(se, player, date)
}

elo_list <- compute_elo(matches)
message("  Rated ", length(ls(elo_list$overall)), " players (overall Elo)")


###############################################################################
# SECTION 3: H2H PRECOMPUTATION (data.table optimized)
###############################################################################

message("\n[3/12] Precomputing H2H lookup table (data.table)...")

precompute_h2h <- function(match_data) {
  dt <- as.data.table(match_data)[!is.na(winner_pid) & !is.na(loser_pid),
    .(winner = winner_pid, loser = loser_pid, date = tourney_date)]

  dt[, `:=`(
    pA = fifelse(winner < loser, winner, loser),
    pB = fifelse(winner < loser, loser, winner),
    pA_won = as.integer(winner == fifelse(winner < loser, winner, loser))
  )]

  setorder(dt, pA, pB, date)
  dt[, `:=`(cum_wins_A = cumsum(pA_won), cum_total = seq_len(.N)), by = .(pA, pB)]

  setkey(dt, pA, pB, date)
  dt
}

h2h_table <- precompute_h2h(matches)
message("  H2H records: ", nrow(h2h_table))

# Vectorized H2H lookup using data.table rolling join
lookup_h2h <- function(h2h_dt, focal, opponent, before_date) {
  pA <- min(focal, opponent)
  pB <- max(focal, opponent)

  # Rolling join: find last row where date < before_date
  result <- h2h_dt[.(pA, pB, before_date), roll = -Inf, nomatch = NA,
                    on = .(pA, pB, date)]

  # Actually we need the row BEFORE before_date, so filter properly
  sub <- h2h_dt[.(pA, pB), on = .(pA, pB), nomatch = NULL]
  sub <- sub[date < before_date]
  if (nrow(sub) == 0L) return(0.5)

  last_row <- sub[.N]
  wins_focal <- if (focal == pA) last_row$cum_wins_A
                else last_row$cum_total - last_row$cum_wins_A
  total <- last_row$cum_total
  (wins_focal + 1) / (total + 2)
}


###############################################################################
# SECTION 4: EVENT TABLE CONSTRUCTION (with Bernoulli convolution IV)
###############################################################################

message("\n[4/12] Building event table (all qualifying losses ", min(YEARS), "-",
        max(YEARS), ")...")

# Load Bernoulli convolution selection probabilities from 11_selection_model.R
sel_prob_file <- file.path(CLEANED_DIR, "selection_probabilities.rds")
has_bernoulli_iv <- file.exists(sel_prob_file)

if (has_bernoulli_iv) {
  sel_probs <- read_rds(sel_prob_file)
  message("  Loaded Bernoulli convolution IV: ", nrow(sel_probs), " obs")
} else {
  message("  WARNING: selection_probabilities.rds not found.")
  message("  Run 11_selection_model.R first. Falling back to crude IV.")
}

build_event_table <- function(match_data) {

  # Identify all qualifying losers
  qual_losers <- match_data |>
    filter(grepl("^Q", round)) |>
    transmute(
      player     = loser_pid,
      player_name = loser_name,
      tourney_id, tourney_date, tourney_level, surface, draw_size, tour
    ) |>
    distinct(player, tourney_id, .keep_all = TRUE)

  # Identify which got LL entry (in main draw)
  ll_winners <- match_data |>
    filter(!is.na(winner_entry), winner_entry == "LL") |>
    transmute(player = winner_pid, tourney_id) |>
    distinct()

  ll_losers <- match_data |>
    filter(!is.na(loser_entry), loser_entry == "LL") |>
    transmute(player = loser_pid, tourney_id) |>
    distinct()

  ll_players <- bind_rows(ll_winners, ll_losers) |> distinct()

  # Mark treatment
  qual_losers <- qual_losers |>
    mutate(ll_entry = as.integer(
      paste(player, tourney_id) %in% paste(ll_players$player, ll_players$tourney_id)
    ))

  # Merge Bernoulli convolution IV if available
  if (has_bernoulli_iv) {
    qual_losers <- qual_losers |>
      left_join(sel_probs |> mutate(player_id = as.character(player_id)) |>
                  select(player_id, tourney_id, selection_prob),
                by = c("player" = "player_id", "tourney_id" = "tourney_id"))
    n_matched <- sum(!is.na(qual_losers$selection_prob))
    message("  Bernoulli IV matched: ", n_matched, " / ", nrow(qual_losers))
  }

  # Crude instrument as fallback
  tourney_stats <- match_data |>
    group_by(tourney_id) |>
    summarise(
      draw_size_calc = first(draw_size),
      n_ll  = sum(winner_entry == "LL" | loser_entry == "LL", na.rm = TRUE),
      n_wo  = sum(!is.na(score) & score == "W/O", na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(p_ll_opportunity = (n_ll + n_wo) / pmax(draw_size_calc, 1))

  qual_losers <- qual_losers |>
    left_join(tourney_stats |> select(tourney_id, p_ll_opportunity),
              by = "tourney_id")
  qual_losers$p_ll_opportunity[is.na(qual_losers$p_ll_opportunity)] <- 0

  # Unified instrument column
  if (has_bernoulli_iv) {
    qual_losers <- qual_losers |>
      mutate(instrument = ifelse(!is.na(selection_prob), selection_prob, p_ll_opportunity))
  } else {
    qual_losers$instrument <- qual_losers$p_ll_opportunity
  }

  qual_losers <- qual_losers |> mutate(event_id = row_number())

  message("  Event table: ", nrow(qual_losers), " qualifying loss events")
  message("    Treated (LL entry): ", sum(qual_losers$ll_entry))
  message("    Control (no LL): ", sum(1 - qual_losers$ll_entry))

  qual_losers
}

event_table <- build_event_table(matches)


###############################################################################
# SECTION 5: MATCH-LEVEL DATASET CONSTRUCTION (data.table optimized)
###############################################################################

message("\n[5/12] Building match-level analysis data (data.table)...")

build_match_data <- function(match_data, ev_table, elo_lst, h2h_dt,
                              T_horizon = 10L,
                              truncation_mode = "at_next_event",
                              calendar_cap_days = 365L) {

  # =========================================================================
  # STRATEGY: Replace the triple-nested R loop with data.table joins.
  #
  # Step A: Build player_tourneys and focal_matches as data.tables with keys
  # Step B: For each event, identify eligible future tournaments via keyed
  #         lookup (not dplyr::filter on the full table)
  # Step C: For each event × tournament, retrieve matches via keyed lookup
  # Step D: Compute Elo/H2H covariates using split-based O(1) lookups
  #
  # The outer loop over events is retained (unavoidable: each event has a
  # unique qualifying date for frozen covariates), but the inner lookups
  # are O(1) via data.table keys and pre-split environments.
  # =========================================================================

  t_start <- Sys.time()

  # --- Step A: Player-tournament index as data.table ----------------------
  # Convert to data.table if needed (match_data may be a tibble)
  if (!data.table::is.data.table(match_data)) {
    match_data <- data.table::as.data.table(match_data)
  }
  pt_dt <- rbindlist(list(
    match_data[, .(player = winner_pid, tourney_id, tourney_date)],
    match_data[, .(player = loser_pid, tourney_id, tourney_date)]
  ), use.names = TRUE)
  pt_dt <- unique(pt_dt)
  setorder(pt_dt, player, tourney_date)
  pt_dt[, prev_tourney_date := shift(tourney_date, type = "lag"), by = player]
  pt_dt[, tourney_seq := seq_len(.N), by = player]

  # Pre-split by player for O(1) access
  pt_split <- split(pt_dt, by = "player", keep.by = FALSE)
  # Restore the player column in each element (keep.by=FALSE drops it)
  for (nm in names(pt_split)) pt_split[[nm]][, player := nm]

  # --- Focal matches: pivot to player-centric view -----------------------
  fm_dt <- rbindlist(list(
    match_data[, .(
      focal = winner_pid, opponent = loser_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, draw_size,
      focal_age = winner_age, opp_age = loser_age,
      win = 1L
    )],
    match_data[, .(
      focal = loser_pid, opponent = winner_pid,
      tourney_id, tourney_date, match_num, surface, tourney_level,
      best_of, draw_size,
      focal_age = loser_age, opp_age = winner_age,
      win = 0L
    )]
  ), use.names = TRUE)
  setorder(fm_dt, focal, tourney_date, match_num)

  # Pre-split by focal player
  fm_split <- split(fm_dt, by = "focal", keep.by = FALSE)
  for (nm in names(fm_split)) fm_split[[nm]][, focal := nm]

  # --- H2H: pre-split by pair key for O(1) lookup -----------------------
  h2h_dt[, pair_key := paste(pA, pB)]
  h2h_split <- split(h2h_dt, by = "pair_key", keep.by = FALSE)

  lookup_h2h_fast <- function(focal, opponent, before_date) {
    pA_val <- min(focal, opponent)
    pB_val <- max(focal, opponent)
    key <- paste(pA_val, pB_val)
    sub <- h2h_split[[key]]
    if (is.null(sub)) return(0.5)
    valid <- sub[date < before_date]
    if (nrow(valid) == 0L) return(0.5)
    last_row <- valid[.N]
    wins_focal <- if (focal == pA_val) last_row$cum_wins_A
                  else last_row$cum_total - last_row$cum_wins_A
    total <- last_row$cum_total
    (wins_focal + 1) / (total + 2)
  }

  # --- Elo: fast lookup using environment + base R -----------------------
  get_elo_fast <- function(pid, date) {
    df <- elo_lst$overall[[pid]]
    if (is.null(df)) return(1500)
    valid_idx <- which(df$date <= date)
    if (length(valid_idx) == 0L) return(1500)
    df$rating[valid_idx[length(valid_idx)]]
  }

  get_blended_fast <- function(pid, surface, date) {
    ov <- get_elo_fast(pid, date)
    if (is.na(surface)) return(ov)
    s_env <- switch(tolower(trimws(surface)),
                     hard  = elo_lst$hard,
                     clay  = elo_lst$clay,
                     grass = elo_lst$grass,
                     NULL)
    if (is.null(s_env)) return(ov)
    df <- s_env[[pid]]
    if (is.null(df)) return(ov)
    valid_idx <- which(df$date <= date)
    if (length(valid_idx) == 0L) return(ov)
    0.5 * ov + 0.5 * df$rating[valid_idx[length(valid_idx)]]
  }

  # --- Truncation setup ---------------------------------------------------
  ev_dt <- as.data.table(ev_table)
  if (truncation_mode == "at_next_event") {
    setorder(ev_dt, player, tourney_date)
    ev_dt[, next_event_date := shift(tourney_date, type = "lead",
                                      fill = as.Date("2099-12-31")),
           by = player]
    message("  Truncation mode: at_next_event")
  } else {
    ev_dt[, next_event_date := as.Date("2099-12-31")]
    message("  Truncation mode: full_window")
  }

  message("  T_horizon: ", T_horizon, " tournaments, calendar cap: ",
          calendar_cap_days, " days")

  # --- Main loop (event-level, with O(1) inner lookups) -------------------
  n_events <- nrow(ev_dt)
  result_list <- vector("list", n_events)
  truncated_count <- 0L

  for (k in seq_len(n_events)) {
    ev <- ev_dt[k]

    if (k %% 1000 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      rate <- k / elapsed
      eta <- (n_events - k) / rate
      message(sprintf("  Event %d / %d (%.1f min elapsed, ~%.1f min remaining)",
                      k, n_events, elapsed, eta))
    }

    # Pre-treatment Elo (FROZEN)
    pre_elo_focal <- get_elo_fast(ev$player, ev$tourney_date)
    pre_surf_elo_focal <- get_blended_fast(ev$player, ev$surface, ev$tourney_date)

    cal_cap_date <- ev$tourney_date + calendar_cap_days

    # Future tournaments from pre-split (O(1) hash lookup)
    pt_player <- pt_split[[ev$player]]
    if (is.null(pt_player)) next

    future_t <- pt_player[tourney_date > ev$tourney_date &
                            tourney_date < ev$next_event_date &
                            tourney_date <= cal_cap_date]

    if (nrow(future_t) == 0L) next
    if (nrow(future_t) > T_horizon) future_t <- future_t[1:T_horizon]

    n_ft <- nrow(future_t)
    if (n_ft < T_horizon && ev$next_event_date < as.Date("2099-01-01")) {
      truncated_count <- truncated_count + 1L
    }

    # Get all matches for this player from pre-split (O(1))
    player_fm <- fm_split[[ev$player]]
    if (is.null(player_fm)) next

    # For each future tournament, extract matches
    cohort_rows <- vector("list", n_ft)

    for (tt in seq_len(n_ft)) {
      ft <- future_t[tt]
      pm <- player_fm[tourney_id == ft$tourney_id]
      if (nrow(pm) == 0L) next

      setorder(pm, match_num)
      n_pm <- nrow(pm)

      weeks_since_focal <- if (!is.na(ft$prev_tourney_date)) {
        as.numeric(difftime(ft$tourney_date, ft$prev_tourney_date, units = "weeks"))
      } else {
        NA_real_
      }

      # Vectorize over matches within tournament
      elo_opp_vec    <- vapply(seq_len(n_pm), function(m) get_elo_fast(pm$opponent[m], pm$tourney_date[m]), numeric(1))
      surf_opp_vec   <- vapply(seq_len(n_pm), function(m) get_blended_fast(pm$opponent[m], pm$surface[m], pm$tourney_date[m]), numeric(1))
      h2h_vec        <- vapply(seq_len(n_pm), function(m) lookup_h2h_fast(ev$player, pm$opponent[m], ev$tourney_date), numeric(1))

      age_diff_vec <- ifelse(!is.na(pm$focal_age) & !is.na(pm$opp_age),
                              pm$focal_age - pm$opp_age, NA_real_)
      bo5_vec <- ifelse(!is.na(pm$best_of), as.integer(pm$best_of == 5), 0L)

      cohort_rows[[tt]] <- data.table(
        event_id         = ev$event_id,
        player           = ev$player,
        player_name      = ev$player_name,
        opponent         = pm$opponent,
        tourney_id       = ft$tourney_id,
        match_date       = pm$tourney_date,
        t_after_event    = tt,
        days_after_event = as.numeric(difftime(pm$tourney_date, ev$tourney_date, units = "days")),
        round_num        = seq_len(n_pm),
        win              = pm$win,
        ll_entry         = ev$ll_entry,
        instrument       = ev$instrument,
        pre_elo_focal       = pre_elo_focal,
        pre_surf_elo_focal  = pre_surf_elo_focal,
        elo_opp             = elo_opp_vec,
        surf_elo_opp        = surf_opp_vec,
        elo_diff            = pre_elo_focal - elo_opp_vec,
        surf_elo_diff       = pre_surf_elo_focal - surf_opp_vec,
        tourney_level    = pm$tourney_level[1],
        surface          = pm$surface[1],
        weeks_since_focal = weeks_since_focal,
        weeks_since_opp  = NA_real_,
        h2h_smoothed     = h2h_vec,
        age_diff         = age_diff_vec,
        bo5              = bo5_vec,
        qual_tourney     = ev$tourney_id,
        qual_date        = ev$tourney_date,
        qual_level       = ev$tourney_level,
        qual_surface     = ev$surface,
        tour             = ev$tour,
        cohort_truncated = as.integer(n_ft < T_horizon &
                                        ev$next_event_date < as.Date("2099-01-01")),
        max_horizon_available = n_ft
      )
    }

    cohort_rows <- cohort_rows[!sapply(cohort_rows, is.null)]
    if (length(cohort_rows) > 0L) {
      result_list[[k]] <- rbindlist(cohort_rows, use.names = TRUE)
    }
  }

  elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  message(sprintf("  Loop completed in %.1f minutes", elapsed_total))

  result_list <- result_list[!sapply(result_list, is.null)]
  full_df <- rbindlist(result_list, use.names = TRUE)

  # Convert back to data.frame for compatibility with glm()
  full_df <- as.data.frame(full_df)

  message("  Built ", nrow(full_df), " match-level observations across ",
          length(unique(full_df$event_id)), " event cohorts")
  message("  Cohorts truncated before T_horizon: ", truncated_count)

  full_df
}

match_df <- build_match_data(matches, event_table, elo_list, h2h_table,
                              T_HORIZON, TRUNCATION_MODE, CALENDAR_CAP)

# Save intermediate data
saveRDS(event_table, file.path(CLEANED_DIR, "tournament_model_event_table.rds"))
saveRDS(match_df, file.path(CLEANED_DIR, "tournament_model_match_data.rds"))
message("  Saved event table and match data to Data/cleaned/")


###############################################################################
# SECTION 6: CF-IV ESTIMATION — FIRST STAGE
###############################################################################

message("\n[6/12] First stage: Probit for LL entry...")

first_stage <- function(ev_table, verbose = TRUE) {
  model <- glm(ll_entry ~ instrument,
               family = binomial(link = "probit"),
               data = ev_table)

  # Generalized residuals
  lp <- predict(model, type = "link")
  phi <- dnorm(lp)
  Phi <- pnorm(lp)

  vhat <- ev_table$ll_entry * (phi / Phi) -
    (1 - ev_table$ll_entry) * (phi / (1 - Phi))

  ev_table$vhat <- vhat

 if (verbose) {
    s <- summary(model)
    message("\n=== FIRST STAGE: Probit for LL Entry ===")
    message("  N events: ", nrow(ev_table))
    message("  Instrument coef: ",
            round(coef(model)["instrument"], 4))
    message("  Instrument p-value: ",
            round(s$coefficients["instrument", "Pr(>|z|)"], 6))
    message("  Pseudo-R2: ",
            round(1 - model$deviance / model$null.deviance, 4))
  }

  list(model = model, event_table = ev_table)
}

s1 <- first_stage(event_table)


###############################################################################
# SECTION 7: CF-IV ESTIMATION — SECOND STAGE
###############################################################################

message("\n[7/12] Second stage: Match-level logit with control function...")

second_stage_pooled <- function(match_data, ev_table, verbose = TRUE) {
  df <- match_data |>
    left_join(ev_table |> select(event_id, vhat), by = "event_id")

  # Drop rows with missing key covariates
  df <- df |>
    filter(!is.na(elo_diff), !is.na(vhat),
           !is.na(win), !is.na(ll_entry))

  # Replace NA in non-critical covariates with 0

  df$h2h_smoothed[is.na(df$h2h_smoothed)] <- 0.5
  df$age_diff[is.na(df$age_diff)] <- 0
  df$weeks_since_focal[is.na(df$weeks_since_focal)] <- median(df$weeks_since_focal, na.rm = TRUE)
  df$weeks_since_opp[is.na(df$weeks_since_opp)] <- median(df$weeks_since_focal, na.rm = TRUE)
  df$surf_elo_diff[is.na(df$surf_elo_diff)] <- df$elo_diff[is.na(df$surf_elo_diff)]

  # Ensure factor levels for tourney_level and surface
  df$tourney_level <- factor(df$tourney_level)
  df$surface <- factor(df$surface)

  model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                 age_diff + bo5 +
                 tourney_level + surface +
                 weeks_since_focal +
                 ll_entry + vhat,
               family = binomial(link = "logit"),
               data = df)

  if (verbose) {
    s <- summary(model)
    message("\n=== SECOND STAGE (POOLED): Match-Level Logit + CF ===")
    message("  N matches: ", nrow(df))
    message("  delta (ll_entry): ", round(coef(model)["ll_entry"], 4))
    message("  Odds ratio: ", round(exp(coef(model)["ll_entry"]), 4))
    if ("ll_entry" %in% rownames(s$coefficients)) {
      message("  p-value: ", round(s$coefficients["ll_entry", "Pr(>|z|)"], 4))
    }
    if ("vhat" %in% rownames(s$coefficients)) {
      message("  rho (vhat): ", round(coef(model)["vhat"], 4))
      message("  vhat p-value: ", round(s$coefficients["vhat", "Pr(>|z|)"], 4))
    }
  }

  list(model = model, data = df)
}

s2_pooled <- second_stage_pooled(match_df, s1$event_table)


# Dynamic model with horizon interactions
second_stage_dynamic <- function(match_data, ev_table, verbose = TRUE) {
  df <- match_data |>
    left_join(ev_table |> select(event_id, vhat), by = "event_id") |>
    mutate(horizon = factor(t_after_event))

  df <- df |>
    filter(!is.na(elo_diff), !is.na(vhat), !is.na(win), !is.na(ll_entry))

  df$h2h_smoothed[is.na(df$h2h_smoothed)] <- 0.5
  df$age_diff[is.na(df$age_diff)] <- 0
  df$weeks_since_focal[is.na(df$weeks_since_focal)] <- median(df$weeks_since_focal, na.rm = TRUE)
  df$surf_elo_diff[is.na(df$surf_elo_diff)] <- df$elo_diff[is.na(df$surf_elo_diff)]

  df$tourney_level <- factor(df$tourney_level)
  df$surface <- factor(df$surface)

  model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                 age_diff + bo5 +
                 tourney_level + surface +
                 weeks_since_focal +
                 ll_entry:horizon + vhat:horizon,
               family = binomial(link = "logit"),
               data = df)

  if (verbose) {
    coef_tbl <- summary(model)$coefficients
    ll_rows <- grep("ll_entry:horizon", rownames(coef_tbl))

    message("\n=== SECOND STAGE (DYNAMIC): Horizon-Interacted Logit + CF ===")
    message("  N matches: ", nrow(df))
    if (length(ll_rows) > 0) {
      message("  LL effect by horizon:")
      for (r in ll_rows) {
        message("    ", rownames(coef_tbl)[r], ": ",
                round(coef_tbl[r, 1], 4), " (SE=", round(coef_tbl[r, 2], 4),
                ", p=", round(coef_tbl[r, 4], 4), ")")
      }
    }
  }

  list(model = model, data = df)
}

s2_dynamic <- second_stage_dynamic(match_df, s1$event_table)


###############################################################################
# SECTION 8: NAIVE LOGIT (NO CONTROL FUNCTION) — ROBUSTNESS
###############################################################################

message("\n[8/12] Robustness checks...")

# Naive logit (no control function)
naive_logit <- function(match_data, ev_table) {
  df <- match_data |>
    filter(!is.na(elo_diff), !is.na(win), !is.na(ll_entry))
  df$h2h_smoothed[is.na(df$h2h_smoothed)] <- 0.5
  df$age_diff[is.na(df$age_diff)] <- 0
  df$weeks_since_focal[is.na(df$weeks_since_focal)] <- median(df$weeks_since_focal, na.rm = TRUE)
  df$surf_elo_diff[is.na(df$surf_elo_diff)] <- df$elo_diff[is.na(df$surf_elo_diff)]
  df$tourney_level <- factor(df$tourney_level)
  df$surface <- factor(df$surface)

  model <- glm(win ~ elo_diff + surf_elo_diff + h2h_smoothed +
                 age_diff + bo5 +
                 tourney_level + surface +
                 weeks_since_focal +
                 ll_entry,
               family = binomial(link = "logit"),
               data = df)

  message("  Naive logit delta: ", round(coef(model)["ll_entry"], 4),
          " (p=", round(summary(model)$coefficients["ll_entry", "Pr(>|z|)"], 4), ")")

  list(model = model, data = df)
}

s2_naive <- naive_logit(match_df, s1$event_table)

# First-LL-only robustness
restrict_first_ll <- function(ev_table) {
  first_ll <- ev_table |>
    filter(ll_entry == 1) |>
    group_by(player) |>
    arrange(tourney_date) |>
    slice(1) |>
    ungroup()

  first_ctrl <- ev_table |>
    filter(ll_entry == 0) |>
    group_by(player) |>
    arrange(tourney_date) |>
    slice(1) |>
    ungroup()

  restricted <- bind_rows(first_ll, first_ctrl)
  message("  First-LL sample: ", nrow(first_ll), " treated, ",
          nrow(first_ctrl), " control")
  restricted
}

event_first <- restrict_first_ll(s1$event_table)
s1_first <- first_stage(event_first, verbose = FALSE)
match_first <- match_df |> filter(event_id %in% event_first$event_id)

s2_first <- NULL
if (nrow(match_first) > 50) {
  s2_first <- second_stage_pooled(match_first, s1_first$event_table, verbose = FALSE)
  message("  First-LL delta: ", round(coef(s2_first$model)["ll_entry"], 4))
}

# Full window (no truncation) robustness
message("  Building full-window (no truncation) match data...")
match_df_full <- build_match_data(matches, event_table, elo_list, h2h_table,
                                   T_HORIZON, "full_window", CALENDAR_CAP)

s2_full_window <- NULL
if (nrow(match_df_full) > 50) {
  s2_full_window <- second_stage_pooled(match_df_full, s1$event_table, verbose = FALSE)
  message("  Full-window delta: ",
          round(coef(s2_full_window$model)["ll_entry"], 4))
}


###############################################################################
# SECTION 9: COUNTERFACTUAL TOURNAMENT SIMULATION
###############################################################################

message("\n[9/12] Computing counterfactual tournament effects...")

compute_counterfactual_probs <- function(model, data) {
  beta <- coef(model)
  delta <- beta["ll_entry"]

  linpred_observed <- predict(model, newdata = data, type = "link")
  linpred_control <- linpred_observed - delta * data$ll_entry
  linpred_treated <- linpred_control + delta

  data$p0 <- plogis(linpred_control)
  data$p1 <- plogis(linpred_treated)
  data$marginal_effect <- data$p1 - data$p0

  data
}

compute_tournament_effects <- function(model, data) {
  cf_data <- compute_counterfactual_probs(model, data)
  cf_dt <- as.data.table(cf_data)
  setorder(cf_dt, event_id, tourney_id, round_num)

  # --- Tournament-level effects via data.table grouped operations ---------
  # E[W|X=x] = sum_{r=1}^{R} prod_{s=1}^{r} p_s(x)  (cumulative product sums)
  tourney_df <- cf_dt[, {
    R <- .N
    cp0 <- cumprod(p0)
    cp1 <- cumprod(p1)
    ew_c <- sum(cp0)
    ew_t <- sum(cp1)
    tw_c <- cp0[R]
    tw_t <- cp1[R]

    list(
      player        = player[1],
      ll_entry      = ll_entry[1],
      t_after_event = t_after_event[1],
      n_rounds      = R,
      avg_match_me  = mean(p1 - p0),
      ew_control    = ew_c,
      ew_treated    = ew_t,
      delta_ew      = ew_t - ew_c,
      tw_control    = tw_c,
      tw_treated    = tw_t,
      delta_tw      = tw_t - tw_c,
      ratio_tw      = tw_t / max(tw_c, 1e-12),
      log_ratio_tw  = sum(log(p1)) - sum(log(p0))
    )
  }, by = .(event_id, tourney_id)]

  # --- Round-reaching effects via data.table ------------------------------
  round_df <- cf_dt[, {
    R <- .N
    reach_c <- cumprod(p0)
    reach_t <- cumprod(p1)
    list(
      round         = seq_len(R),
      reach_control = reach_c,
      reach_treated = reach_t,
      delta_reach   = reach_t - reach_c
    )
  }, by = .(event_id, tourney_id)]

  # Convert to data.frame for compatibility
  tourney_df <- as.data.frame(tourney_df)
  round_df   <- as.data.frame(round_df)

  message("  Match-level ATE (avg marginal effect): ",
          round(mean(tourney_df$avg_match_me), 4))
  message("  Tournament-level ATE on expected wins: ",
          round(mean(tourney_df$delta_ew), 4))
  message("  Tournament-level ATE on P(win tournament): ",
          round(mean(tourney_df$delta_tw), 6))

  list(tourney_effects = tourney_df,
       round_effects   = round_df,
       match_cf        = cf_data)
}

te_results <- compute_tournament_effects(s2_pooled$model, s2_pooled$data)


###############################################################################
# SECTION 10: HETEROGENEITY ANALYSIS
###############################################################################

message("\n[10/12] Heterogeneity analysis...")

analyse_heterogeneity <- function(tourney_effects, match_data) {
  tourney_meta <- match_data |>
    distinct(event_id, tourney_id, qual_level, qual_surface,
             pre_elo_focal, bo5, tour) |>
    group_by(event_id, tourney_id) |>
    slice(1) |>
    ungroup()

  te <- tourney_effects |>
    left_join(tourney_meta, by = c("event_id", "tourney_id"))

  # By tournament level
  by_level <- te |>
    group_by(qual_level) |>
    summarise(
      n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
      mean_match = mean(avg_match_me), .groups = "drop"
    )
  message("  By tournament level:")
  for (j in seq_len(nrow(by_level))) {
    message("    ", by_level$qual_level[j], ": n=", by_level$n[j],
            " dEW=", round(by_level$mean_dEW[j], 4))
  }

  # By surface
  by_surface <- te |>
    group_by(qual_surface) |>
    summarise(
      n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
      mean_match = mean(avg_match_me), .groups = "drop"
    )

  # By player strength quartile
  te$elo_quartile <- cut(te$pre_elo_focal,
                          breaks = quantile(te$pre_elo_focal,
                                            probs = c(0, 0.25, 0.5, 0.75, 1),
                                            na.rm = TRUE),
                          labels = c("Q1 (weakest)", "Q2", "Q3", "Q4 (strongest)"),
                          include.lowest = TRUE)

  by_elo <- te |>
    filter(!is.na(elo_quartile)) |>
    group_by(elo_quartile) |>
    summarise(
      n = n(), mean_elo = mean(pre_elo_focal, na.rm = TRUE),
      mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
      mean_match = mean(avg_match_me), .groups = "drop"
    )

  # By horizon
  by_horizon <- te |>
    group_by(t_after_event) |>
    summarise(
      n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
      mean_match = mean(avg_match_me), .groups = "drop"
    )

  # By tour (ATP vs WTA)
  by_tour <- te |>
    group_by(tour) |>
    summarise(
      n = n(), mean_dEW = mean(delta_ew), mean_dTW = mean(delta_tw),
      mean_match = mean(avg_match_me), .groups = "drop"
    )

  list(by_level = by_level, by_surface = by_surface,
       by_elo = by_elo, by_horizon = by_horizon,
       by_tour = by_tour, full = te)
}

het <- analyse_heterogeneity(te_results$tourney_effects, match_df)


###############################################################################
# SECTION 11: BOOTSTRAP INFERENCE
###############################################################################

message("\n[11/12] Bootstrap inference (", N_BOOT, " replications)...")

bootstrap_both_stages <- function(ev_table, match_data, n_boot = 50L,
                                   model_type = "pooled") {
  players <- unique(ev_table$player)
  n_players <- length(players)

  all_coefs <- list()
  n_success <- 0L

  message("  Bootstrapping ", n_boot, " replications (player-level blocks)...")

  for (b in seq_len(n_boot)) {
    if (b %% 10 == 0) message("    Replication ", b, " / ", n_boot)

    boot_players <- sample(players, n_players, replace = TRUE)

    boot_events <- do.call(rbind, lapply(seq_along(boot_players), function(j) {
      sub <- ev_table[ev_table$player == boot_players[j], ]
      if (nrow(sub) == 0) return(NULL)
      sub$boot_id <- j
      sub$event_id <- sub$event_id + j * 1e6
      sub
    }))

    if (is.null(boot_events) || nrow(boot_events) < 20) next

    boot_matches <- do.call(rbind, lapply(seq_along(boot_players), function(j) {
      sub <- match_data[match_data$player == boot_players[j], ]
      if (nrow(sub) == 0) return(NULL)
      sub$event_id <- sub$event_id + j * 1e6
      sub
    }))

    if (is.null(boot_matches) || nrow(boot_matches) < 50) next

    tryCatch({
      s1_b <- first_stage(boot_events, verbose = FALSE)

      if (model_type == "pooled") {
        s2_b <- second_stage_pooled(boot_matches, s1_b$event_table, verbose = FALSE)
      } else {
        s2_b <- second_stage_dynamic(boot_matches, s1_b$event_table, verbose = FALSE)
      }

      n_success <- n_success + 1L
      all_coefs[[n_success]] <- coef(s2_b$model)
    }, error = function(e) {})
  }

  message("  Successful replications: ", n_success, " of ", n_boot)

  if (n_success < 10) {
    message("  WARNING: Too few successful replications for reliable inference.")
    return(NULL)
  }

  all_names <- unique(unlist(lapply(all_coefs, names)))
  boot_matrix <- matrix(NA, nrow = n_success, ncol = length(all_names),
                         dimnames = list(NULL, all_names))
  for (i in seq_len(n_success)) {
    boot_matrix[i, names(all_coefs[[i]])] <- all_coefs[[i]]
  }

  boot_se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
  boot_mean <- apply(boot_matrix, 2, mean, na.rm = TRUE)

  message("  Bootstrap SE for ll_entry: ",
          if ("ll_entry" %in% all_names) round(boot_se["ll_entry"], 4) else "N/A")

  boot_matrix
}

boot_pooled  <- bootstrap_both_stages(s1$event_table, match_df, N_BOOT, "pooled")
boot_dynamic <- bootstrap_both_stages(s1$event_table, match_df, N_BOOT, "dynamic")


###############################################################################
# SECTION 12: OUTPUTS — TABLES, FIGURES, SUMMARY
###############################################################################

message("\n[12/12] Generating outputs...")

# ---- Helper: extract coefficient info for tables ----------------------------

extract_coef_row <- function(model_obj, boot_mat = NULL, label = "") {
  if (is.null(model_obj)) return(NULL)
  m <- model_obj$model
  d <- model_obj$data
  s <- summary(m)$coefficients

  delta <- s["ll_entry", "Estimate"]
  delta_se_analytic <- s["ll_entry", "Std. Error"]
  delta_p_analytic <- s["ll_entry", "Pr(>|z|)"]

  # Use bootstrap SE if available
  if (!is.null(boot_mat) && "ll_entry" %in% colnames(boot_mat)) {
    delta_se <- sd(boot_mat[, "ll_entry"], na.rm = TRUE)
    delta_z <- delta / delta_se
    delta_p <- 2 * pnorm(-abs(delta_z))
  } else {
    delta_se <- delta_se_analytic
    delta_p <- delta_p_analytic
  }

  rho <- if ("vhat" %in% rownames(s)) s["vhat", "Estimate"] else NA
  rho_p <- if ("vhat" %in% rownames(s)) s["vhat", "Pr(>|z|)"] else NA

  data.frame(
    Specification = label,
    N_matches = nrow(d),
    N_events = length(unique(d$event_id)),
    delta = delta,
    delta_SE = delta_se,
    OR = exp(delta),
    p_value = delta_p,
    rho = rho,
    rho_p = rho_p,
    stringsAsFactors = FALSE
  )
}

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

# ---- Table: First Stage (Probit) -------------------------------------------

write_first_stage_table <- function(model, filepath) {
  s <- summary(model)$coefficients
  intercept <- s["(Intercept)", ]
  instrument_row <- s["instrument", ]

  n_obs <- model$df.null + 1
  pseudo_r2 <- 1 - model$deviance / model$null.deviance

  iv_label <- if (has_bernoulli_iv) "Selection Probability" else "P(LL opportunity)"

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Coefficient & Std. Error \\\\",
    "\\midrule",
    sprintf("%s & %.4f%s & (%.4f) \\\\",
            iv_label, instrument_row[1], stars_fn(instrument_row[4]), instrument_row[2]),
    sprintf("Intercept & %.4f%s & (%.4f) \\\\",
            intercept[1], stars_fn(intercept[4]), intercept[2]),
    "\\midrule",
    sprintf("Observations & \\multicolumn{2}{c}{%s} \\\\",
            format(n_obs, big.mark = ",")),
    sprintf("Pseudo $R^2$ & \\multicolumn{2}{c}{%.4f} \\\\", pseudo_r2),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_first_stage_table(s1$model, file.path(TABLE_DIR, "table_tournament_first_stage.tex"))


# ---- Table: Match Effects ---------------------------------------------------

write_match_effects_table <- function(pooled, first_ll, full_window, naive,
                                       boot_mat, filepath) {
  rows <- list(
    extract_coef_row(pooled, boot_mat, "CF-IV (truncated)"),
    extract_coef_row(first_ll, NULL, "CF-IV (first LL only)"),
    extract_coef_row(full_window, NULL, "CF-IV (full window)"),
    extract_coef_row(naive, NULL, "Naive logit (no CF)")
  )
  rows <- rows[!sapply(rows, is.null)]
  tbl <- do.call(rbind, rows)

  lines <- c(
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Specification & $N_{\\text{matches}}$ & $N_{\\text{events}}$ & $\\hat{\\delta}$ & SE & OR & $\\hat{\\rho}$ & Endog.~$p$ \\\\",
    "\\midrule"
  )

  for (j in seq_len(nrow(tbl))) {
    r <- tbl[j, ]
    rho_str <- if (is.na(r$rho)) "---" else sprintf("%.4f", r$rho)
    rho_p_str <- if (is.na(r$rho_p)) "---" else sprintf("%.4f", r$rho_p)
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f%s & (%.4f) & %.3f & %s & %s \\\\",
      r$Specification,
      format(r$N_matches, big.mark = ","),
      format(r$N_events, big.mark = ","),
      r$delta, stars_fn(r$p_value), r$delta_SE, r$OR,
      rho_str, rho_p_str
    ))
  }

  lines <- c(lines,
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_match_effects_table(s2_pooled, s2_first, s2_full_window, s2_naive,
                           boot_pooled,
                           file.path(TABLE_DIR, "table_tournament_match_effects.tex"))


# ---- Table: Tournament Effects ----------------------------------------------

write_tournament_effects_table <- function(te_results, filepath) {
  te <- te_results$tourney_effects

  avg_me <- mean(te$avg_match_me)
  avg_dew <- mean(te$delta_ew)
  avg_dtw <- mean(te$delta_tw)
  med_me <- median(te$avg_match_me)
  med_dew <- median(te$delta_ew)
  med_dtw <- median(te$delta_tw)

  lines <- c(
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & Match $\\Delta P(\\text{win})$ & $\\Delta E[W]$ & $\\Delta P(\\text{win tourney})$ \\\\",
    "\\midrule",
    sprintf("Mean & %.4f & %.4f & %.6f \\\\", avg_me, avg_dew, avg_dtw),
    sprintf("Median & %.4f & %.4f & %.6f \\\\", med_me, med_dew, med_dtw),
    sprintf("SD & %.4f & %.4f & %.6f \\\\",
            sd(te$avg_match_me), sd(te$delta_ew), sd(te$delta_tw)),
    sprintf("N (player-tournaments) & \\multicolumn{3}{c}{%s} \\\\",
            format(nrow(te), big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_tournament_effects_table(te_results,
                                file.path(TABLE_DIR, "table_tournament_effects.tex"))


# ---- Table: Heterogeneity --------------------------------------------------

write_heterogeneity_table <- function(het, filepath) {
  lines <- c(
    "\\begin{tabular}{llcccc}",
    "\\toprule",
    "Dimension & Category & $N$ & $\\Delta P(\\text{match})$ & $\\Delta E[W]$ & $\\Delta P(\\text{tourney})$ \\\\",
    "\\midrule"
  )

  # By level
  for (j in seq_len(nrow(het$by_level))) {
    r <- het$by_level[j, ]
    prefix <- if (j == 1) "Tournament level" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$qual_level, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }

  lines <- c(lines, "\\midrule")

  # By surface
  for (j in seq_len(nrow(het$by_surface))) {
    r <- het$by_surface[j, ]
    prefix <- if (j == 1) "Surface" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$qual_surface, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }

  lines <- c(lines, "\\midrule")

  # By Elo quartile
  for (j in seq_len(nrow(het$by_elo))) {
    r <- het$by_elo[j, ]
    prefix <- if (j == 1) "Player strength" else ""
    lines <- c(lines, sprintf(
      "%s & %s & %s & %.4f & %.4f & %.6f \\\\",
      prefix, r$elo_quartile, format(r$n, big.mark = ","),
      r$mean_match, r$mean_dEW, r$mean_dTW
    ))
  }

  lines <- c(lines,
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_heterogeneity_table(het, file.path(TABLE_DIR, "table_tournament_heterogeneity.tex"))


# ---- Figure: Dynamic Effects ------------------------------------------------

make_dynamic_effects_plot <- function(dynamic_model, boot_matrix = NULL) {
  coef_tbl <- summary(dynamic_model$model)$coefficients
  ll_rows <- grep("ll_entry:horizon", rownames(coef_tbl))

  if (length(ll_rows) == 0) {
    message("  WARNING: No horizon interaction coefficients found.")
    return(NULL)
  }

  horizons <- as.integer(gsub(".*horizon", "", rownames(coef_tbl)[ll_rows]))
  deltas <- coef_tbl[ll_rows, "Estimate"]

  # Use bootstrap SEs if available
  if (!is.null(boot_matrix)) {
    ll_cols <- grep("ll_entry:horizon", colnames(boot_matrix))
    if (length(ll_cols) > 0 && length(ll_cols) == length(ll_rows)) {
      se <- apply(boot_matrix[, ll_cols, drop = FALSE], 2, sd, na.rm = TRUE)
      ci_label <- "95% CI (bootstrap)"
    } else {
      se <- coef_tbl[ll_rows, "Std. Error"]
      ci_label <- "95% CI (analytic)"
    }
  } else {
    se <- coef_tbl[ll_rows, "Std. Error"]
    ci_label <- "95% CI (analytic)"
  }

  # Sample sizes per horizon
  match_data <- dynamic_model$data
  n_by_h <- match_data |>
    group_by(t_after_event) |>
    summarise(n = n(), .groups = "drop")

  plot_df <- data.frame(
    horizon = horizons,
    delta = deltas,
    se = se,
    lower = deltas - 1.96 * se,
    upper = deltas + 1.96 * se
  ) |>
    left_join(n_by_h, by = c("horizon" = "t_after_event"))

  p <- ggplot(plot_df, aes(x = horizon, y = delta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
               linewidth = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, color = col_treat, linewidth = 0.6) +
    geom_point(color = col_treat, size = 3) +
    geom_line(color = col_treat, linewidth = 0.8) +
    geom_text(aes(label = paste0("n=", format(n, big.mark = ",")),
                  y = lower - 0.02 * diff(range(c(lower, upper)))),
              size = 2.5, family = "serif", color = "grey40") +
    scale_x_continuous(breaks = horizons) +
    labs(x = "Tournaments After LL Entry (t)",
         y = expression(hat(delta)(t) ~ " (log-odds)")) +
    theme_paper()

  p
}

p_dynamic <- make_dynamic_effects_plot(s2_dynamic, boot_dynamic)
if (!is.null(p_dynamic)) {
  ggsave(file.path(FIG_DIR, "fig_dynamic_effects.pdf"), p_dynamic,
         width = 8, height = 5, device = cairo_pdf)
  message("  Saved: Figures/fig_dynamic_effects.pdf")
}


# ---- Figure: Tournament Effects (4-panel) -----------------------------------

make_tournament_effects_plot <- function(te_results, het) {
  te <- te_results$tourney_effects
  rd <- te_results$round_effects

  # (a) Match-level marginal effects distribution
  p_a <- ggplot(te, aes(x = avg_match_me)) +
    geom_histogram(bins = 40, fill = col_treat, color = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(te$avg_match_me), linetype = "dashed",
               color = "red3", linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(x = expression(Delta * P(win)), y = "Frequency",
         tag = "(a)") +
    theme_paper(base_size = 11)

  # (b) Tournament-level delta E[W]
  p_b <- ggplot(te, aes(x = delta_ew)) +
    geom_histogram(bins = 40, fill = col_control, color = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(te$delta_ew), linetype = "dashed",
               color = "red3", linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(x = "Additional Expected Wins", y = "Frequency",
         tag = "(b)") +
    theme_paper(base_size = 11)

  # (c) Round-reaching probability shift
  round_avg <- rd |>
    group_by(round) |>
    summarise(
      mean_delta = mean(delta_reach),
      se_delta   = sd(delta_reach) / sqrt(n()),
      .groups = "drop"
    )

  p_c <- ggplot(round_avg, aes(x = round, y = mean_delta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = mean_delta - 1.96 * se_delta,
                      ymax = mean_delta + 1.96 * se_delta),
                  width = 0.15, color = "darkgreen", linewidth = 0.6) +
    geom_point(color = "darkgreen", size = 2.5) +
    geom_line(color = "darkgreen", linewidth = 0.7) +
    scale_x_continuous(breaks = round_avg$round) +
    labs(x = "Round", y = expression(Delta * P(reach)),
         tag = "(c)") +
    theme_paper(base_size = 11)

  # (d) Delta E[W] by horizon
  hz <- het$by_horizon

  p_d <- ggplot(hz, aes(x = t_after_event, y = mean_dEW)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "purple4", size = 2.5) +
    geom_line(color = "purple4", linewidth = 0.7) +
    scale_x_continuous(breaks = hz$t_after_event) +
    labs(x = "Tournaments After LL Entry (t)",
         y = expression(Delta * E * "[W]"),
         tag = "(d)") +
    theme_paper(base_size = 11)

  combined <- (p_a | p_b) / (p_c | p_d)
  combined
}

p_tournament <- make_tournament_effects_plot(te_results, het)
ggsave(file.path(FIG_DIR, "fig_tournament_effects.pdf"), p_tournament,
       width = 10, height = 8, device = cairo_pdf)
message("  Saved: Figures/fig_tournament_effects.pdf")


# ---- Save full results object -----------------------------------------------

results_obj <- list(
  first_stage            = s1,
  pooled                 = s2_pooled,
  dynamic                = s2_dynamic,
  naive                  = s2_naive,
  robustness_first_ll    = s2_first,
  robustness_full_window = s2_full_window,
  boot_pooled            = boot_pooled,
  boot_dynamic           = boot_dynamic,
  tournament_effects     = te_results,
  heterogeneity          = het,
  event_table            = event_table,
  match_data             = match_df,
  params = list(
    T_horizon = T_HORIZON,
    calendar_cap = CALENDAR_CAP,
    truncation_mode = TRUNCATION_MODE,
    years = YEARS,
    n_boot = N_BOOT
  )
)

saveRDS(results_obj, file.path(CLEANED_DIR, "tournament_model_results.rds"))
message("  Saved: Data/cleaned/tournament_model_results.rds")


# ---- Summary markdown -------------------------------------------------------

write_summary <- function(results, filepath) {
  pooled_model <- results$pooled$model
  pooled_s <- summary(pooled_model)$coefficients

  delta <- pooled_s["ll_entry", "Estimate"]
  delta_se <- pooled_s["ll_entry", "Std. Error"]
  delta_p <- pooled_s["ll_entry", "Pr(>|z|)"]

  # Bootstrap SE if available
  boot_se <- if (!is.null(results$boot_pooled) && "ll_entry" %in% colnames(results$boot_pooled)) {
    sd(results$boot_pooled[, "ll_entry"], na.rm = TRUE)
  } else {
    delta_se
  }

  te <- results$tournament_effects$tourney_effects

  rho <- if ("vhat" %in% rownames(pooled_s)) pooled_s["vhat", "Estimate"] else NA
  rho_p <- if ("vhat" %in% rownames(pooled_s)) pooled_s["vhat", "Pr(>|z|)"] else NA

  lines <- c(
    "# Tournament Performance Model: Results Summary",
    "",
    paste0("Generated: ", Sys.time()),
    "",
    "## Parameters",
    paste0("- Years: ", min(results$params$years), "-", max(results$params$years)),
    paste0("- T_horizon: ", results$params$T_horizon, " tournaments"),
    paste0("- Calendar cap: ", results$params$calendar_cap, " days"),
    paste0("- Truncation: ", results$params$truncation_mode),
    paste0("- Bootstrap replications: ", results$params$n_boot),
    "",
    "## Sample",
    paste0("- Events (qualifying losses): ", nrow(results$event_table)),
    paste0("  - Treated (LL entry): ", sum(results$event_table$ll_entry)),
    paste0("  - Control: ", sum(1 - results$event_table$ll_entry)),
    paste0("- Match-level observations: ", nrow(results$match_data)),
    "",
    "## Main Results (Pooled CF-IV)",
    paste0("- delta (ll_entry, log-odds): ", round(delta, 4)),
    paste0("- Analytic SE: ", round(delta_se, 4)),
    paste0("- Bootstrap SE: ", round(boot_se, 4)),
    paste0("- Odds ratio: ", round(exp(delta), 4)),
    paste0("- p-value (analytic): ", round(delta_p, 4)),
    "",
    "## Endogeneity Test",
    paste0("- rho (vhat): ", if (!is.na(rho)) round(rho, 4) else "N/A"),
    paste0("- rho p-value: ", if (!is.na(rho_p)) round(rho_p, 4) else "N/A"),
    paste0("- Interpretation: ",
           if (!is.na(rho_p) && rho_p < 0.05) "Evidence of endogeneity; CF correction needed"
           else "No strong evidence of endogeneity; naive and CF results may be similar"),
    "",
    "## Tournament-Level Treatment Effects",
    paste0("- Match-level ATE (avg marginal effect on P(win)): ",
           round(mean(te$avg_match_me), 4)),
    paste0("- Tournament-level ATE on expected wins (E[dW]): ",
           round(mean(te$delta_ew), 4)),
    paste0("- Tournament-level ATE on P(win tournament): ",
           round(mean(te$delta_tw), 6)),
    "",
    "## Interpretation",
    paste0("A delta of ", round(delta, 3), " in log-odds means:"),
    paste0("- At P(win)=0.50: shift to ~", round(plogis(qlogis(0.50) + delta), 3)),
    paste0("- At P(win)=0.30: shift to ~", round(plogis(qlogis(0.30) + delta), 3)),
    "- These effects compound across tournament rounds",
    "",
    "## Robustness",
    paste0("- First-LL only: delta = ",
           if (!is.null(results$robustness_first_ll))
             round(coef(results$robustness_first_ll$model)["ll_entry"], 4)
           else "N/A"),
    paste0("- Full window: delta = ",
           if (!is.null(results$robustness_full_window))
             round(coef(results$robustness_full_window$model)["ll_entry"], 4)
           else "N/A"),
    paste0("- Naive logit: delta = ",
           round(coef(results$naive$model)["ll_entry"], 4)),
    "",
    "## Output Files",
    "- Data/cleaned/tournament_model_results.rds",
    "- Data/cleaned/tournament_model_event_table.rds",
    "- Data/cleaned/tournament_model_match_data.rds",
    "- Tables/table_tournament_first_stage.tex",
    "- Tables/table_tournament_match_effects.tex",
    "- Tables/table_tournament_effects.tex",
    "- Tables/table_tournament_heterogeneity.tex",
    "- Figures/fig_dynamic_effects.pdf",
    "- Figures/fig_tournament_effects.pdf"
  )

  writeLines(lines, filepath)
  message("  Saved: ", filepath)
}

write_summary(results_obj, file.path(OUTPUT_DIR, "tournament_model_summary.md"))


# ---- Done -------------------------------------------------------------------

message("\n================================================================")
message("  Tournament performance model complete.")
message("  Key result: delta = ",
        round(coef(s2_pooled$model)["ll_entry"], 4),
        " (OR = ", round(exp(coef(s2_pooled$model)["ll_entry"]), 4), ")")
message("  See Output/tournament_model_summary.md for full details.")
message("================================================================")
