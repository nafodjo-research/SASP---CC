# Pseudo-Code: Main Estimation for Tennis Lucky Losers

**Date:** 2026-03-20
**Corresponds to:** `strategy_memo.md` Section 5

---

## 0. Data Construction Pipeline

```
# ============================================================
# STEP 0: Load and merge raw data
# ============================================================

FOR year IN 2007:2024 DO:
    main[year]  <- read_csv("atp_matches_{year}.csv")
    qual[year]  <- read_csv("atp_matches_qual_chall_{year}.csv")
END

main <- bind_rows(main)  # all main draw matches
qual <- bind_rows(qual)  # all qualifying + challenger matches

# Filter qualifying to tour-level only
qual <- qual[tourney_level IN ("G", "M", "A")]

# Load rankings panel
rankings <- bind_rows(
    read_csv("atp_rankings_00s.csv"),
    read_csv("atp_rankings_10s.csv"),
    read_csv("atp_rankings_20s.csv"),
    read_csv("atp_rankings_current.csv")
)

players <- read_csv("atp_players.csv")
```

## 1. Construct the Analysis Sample

```
# ============================================================
# STEP 1: Identify final-round qualifying losers
# ============================================================

FOR each tournament t IN qual DO:
    final_round[t] <- max(round) among {Q1, Q2, Q3} at tournament t
    final_losers[t] <- all loser_id where round = final_round[t] at tournament t

    # Get each loser's ranking at tournament time
    FOR each player i IN final_losers[t] DO:
        ranking[i,t] <- loser_rank from the qualifying match record
        IF ranking[i,t] IS MISSING:
            ranking[i,t] <- merge with rankings file using
                            closest ranking_date <= (tourney_date - 7)
        END
    END

    # Rank losers within tournament (1 = highest-ranked = best)
    rank_among_losers[i,t] <- row_number(ranking[i,t]) within tournament t
    total_losers[t] <- count(final_losers[t])
END

# ============================================================
# STEP 2: Identify LL entries in main draw
# ============================================================

FOR each tournament t IN main DO:
    ll_players[t] <- distinct player_id where
                     (winner_entry = "LL" AND winner_id = player_id) OR
                     (loser_entry = "LL" AND loser_id = player_id)
    ll_slots[t] <- count(ll_players[t])
END

# ============================================================
# STEP 3: Merge and define treatment
# ============================================================

analysis_sample <- final_losers  # one row per player-tournament

FOR each observation (i, t) IN analysis_sample DO:
    is_ll[i,t] <- 1 if player i IN ll_players[t], else 0
    cutoff[t] <- ll_slots[t]
    R_tilde[i,t] <- rank_among_losers[i,t] - cutoff[t]
    # R_tilde <= 0 => treated (should be LL)
    # R_tilde > 0 => control (not LL)
END

# Drop tournaments with zero LL slots (no treatment variation)
analysis_sample <- analysis_sample[ll_slots[t] >= 1]

# ============================================================
# STEP 4: First-stage verification (sharp RDD check)
# ============================================================

concordance <- mean(is_ll == 1(R_tilde <= 0))
# REPORT: concordance rate
# If concordance > 0.95: treat as sharp RDD
# If concordance < 0.95: investigate discrepancies, consider fuzzy RDD

# ============================================================
# STEP 5: Construct outcome variables
# ============================================================

FOR each observation (i, t) IN analysis_sample DO:
    event_date <- tourney_date[t]
    ranking_at_event <- ranking[i,t]

    # Immediate outcomes
    IF is_ll[i,t] == 1:
        matches_at_tourney <- count main draw matches for player i at tournament t
        rounds_advanced <- max(round reached) at tournament t
        points_at_tourney <- lookup(tourney_level[t], max_round_reached)
    ELSE:
        matches_at_tourney <- 0  # did not enter main draw
        rounds_advanced <- 0
        points_at_tourney <- 0
    END

    # Panel outcomes (from rankings)
    FOR weeks_ahead IN (4, 8, 12, 26, 52, 104) DO:
        target_date <- event_date + weeks_ahead * 7
        ranking_future <- ranking of player i at closest date to target_date
        ranking_change[weeks_ahead] <- ranking_future - ranking_at_event
        # Note: negative change = improvement (lower number = better rank)
        # Consider using points_change instead for interpretability
    END

    # Tournament access outcomes
    FOR months_ahead IN (3, 6, 12) DO:
        window_end <- event_date + months_ahead * 30
        main_draw_entries[months_ahead] <- count(main draw appearances for player i
                                                  between event_date and window_end
                                                  where entry != "Q")
        main_draw_wins[months_ahead] <- count(main draw wins for player i
                                               between event_date and window_end)
    END
END

# ============================================================
# STEP 6: Construct covariates for balance tests
# ============================================================

FOR each observation (i, t) DO:
    age <- player age at tourney_date (from players file)
    career_stage <- tourney_year - year of first tour-level match
    height <- player height (from players file)
    hand <- player handedness
    nationality <- player country code
    surface <- tournament surface
    tourney_tier <- tournament level (G, M, A)

    # Pre-event performance (for balance / lagged outcome tests)
    ranking_12w_before <- ranking of player i at (event_date - 84 days)
    ranking_26w_before <- ranking of player i at (event_date - 182 days)
    wins_3m_before <- main draw wins in 3 months before event_date
    qual_attempts_12m <- qualifying entries in 12 months before event_date
END
```

## 2. Primary Estimation: Local Randomization RDD

```
# ============================================================
# ESTIMATION A: Local Randomization Framework (PRIMARY)
# ============================================================

# A1. Select optimal window
# Uses rdwinselect to find the smallest window where
# covariates are balanced (as-if random assignment)

window_result <- rdwinselect(
    R = R_tilde,           # normalized running variable
    X = cbind(age, career_stage, height, ranking_12w_before,
              wins_3m_before, qual_attempts_12m),
    cutoff = 0,
    obsmin = 50,           # minimum observations per side
    wmin = 1,              # minimum window width (integer steps)
    wstep = 1,             # increment window by 1 position
    reps = 1000,           # permutation replications
    statistic = "ttest"    # test statistic for balance
)

optimal_window <- window_result$w_selected  # e.g., w = 2 or w = 3

# A2. Estimate treatment effect within optimal window
FOR each outcome Y IN (ranking_change_26w, main_draw_entries_12m,
                       ranking_change_4w, ranking_change_8w,
                       ranking_change_12w, ranking_change_52w,
                       points_at_tourney, rounds_advanced,
                       main_draw_wins_12m, career_survival_2yr) DO:

    result[Y] <- rdrandinf(
        Y = Y,
        R = R_tilde,
        cutoff = 0,
        wl = -optimal_window,
        wr = optimal_window,
        statistic = "diffmeans",
        p = 0.15,              # Bernoulli trial probability (for finite-sample)
        reps = 5000,           # permutation replications
        seed = 20260320
    )

    STORE: point_estimate[Y], p_value[Y], CI_lower[Y], CI_upper[Y],
           N_treated[Y], N_control[Y]
END
```

## 3. Robustness Estimation: Continuity-Based RDD

```
# ============================================================
# ESTIMATION B: rdrobust (ROBUSTNESS)
# ============================================================

FOR each outcome Y DO:
    result_robust[Y] <- rdrobust(
        y = Y,
        x = R_tilde,
        c = 0,
        p = 1,                 # local linear
        kernel = "triangular",
        bwselect = "mserd",    # MSE-optimal bandwidth
        cluster = player_id,   # cluster at player level
        covs = cbind(age, career_stage, tourney_tier_dummies, year_dummies),
        all = TRUE             # report conventional + robust + bias-corrected
    )

    STORE: tau_hat[Y], se_robust[Y], CI_robust[Y], bw_selected[Y],
           N_effective_left[Y], N_effective_right[Y]
END

# B2. Bandwidth sensitivity
FOR each outcome Y DO:
    FOR bw_mult IN (0.5, 0.75, 1.0, 1.25, 1.5, 2.0) DO:
        bw_manual <- bw_selected[Y] * bw_mult
        result_bw[Y, bw_mult] <- rdrobust(y = Y, x = R_tilde, c = 0,
                                           h = bw_manual, cluster = player_id)
    END
END

# B3. Polynomial sensitivity
FOR each outcome Y DO:
    FOR poly_order IN (1, 2) DO:
        result_poly[Y, poly_order] <- rdrobust(y = Y, x = R_tilde, c = 0,
                                                p = poly_order, cluster = player_id)
    END
END
```

## 4. Robustness: Honest Confidence Intervals (Discrete RV)

```
# ============================================================
# ESTIMATION C: Kolesar and Rothe (2018) Honest CIs (ROBUSTNESS)
# ============================================================

# Estimate the curvature bound M from the data
# M = bound on the second derivative of E[Y|R=r]
# Use the data-driven approach from Kolesar and Rothe (2018)

FOR each outcome Y DO:
    result_honest[Y] <- RDHonest(
        formula = Y ~ R_tilde,
        cutoff = 0,
        M = "FLCI",           # finite-sample optimal length CI
        kern = "triangular",
        se.method = "EHW",    # Eicker-Huber-White
        opt.criterion = "MSE"
    )

    STORE: tau_hat_honest[Y], CI_honest_lower[Y], CI_honest_upper[Y],
           M_used[Y], h_used[Y]
END
```

## 5. Grand Slam Lottery Estimation

```
# ============================================================
# ESTIMATION D: Grand Slam Lottery (VALIDATION)
# ============================================================

# D0. Construct Grand Slam lottery sample
gs_sample <- analysis_sample[tourney_level == "G" AND year >= 2006]
gs_pool <- gs_sample[rank_among_losers <= 4]  # the top-4 eligible pool

# D1. Verify randomness within pool
randomness_test <- chisq.test(
    table(gs_pool$rank_among_losers, gs_pool$is_ll)
)
# Also: Fisher exact test
randomness_fisher <- fisher.test(
    table(gs_pool$rank_among_losers, gs_pool$is_ll)
)
# REPORT: p-values; null = LL probability independent of rank within pool

# D2. ITT within pool: simple difference in means
FOR each outcome Y DO:
    result_itt[Y] <- lm(
        Y ~ is_ll + factor(tourney_id),
        data = gs_pool
    )
    # Cluster SEs at player level
    se_itt[Y] <- vcovCL(result_itt[Y], cluster = gs_pool$player_id)

    STORE: beta_itt[Y], se_clustered[Y], p_value[Y], N[Y]
END

# D3. IV / LATE: instrument with pool eligibility
# Full sample = all GS Q3 losers; instrument = (rank_among_losers <= 4)
FOR each outcome Y DO:
    result_iv[Y] <- ivreg(
        Y ~ is_ll + factor(tourney_id) | in_pool + factor(tourney_id),
        data = gs_sample
    )
    se_iv[Y] <- vcovCL(result_iv[Y], cluster = gs_sample$player_id)
END

# D4. Supplementary RDD at the 4th/5th boundary
# Among all GS Q3 losers, is there a discontinuity at rank_among_losers = 4?
gs_rdd <- rdrobust(
    y = gs_sample$Y,
    x = gs_sample$rank_among_losers,
    c = 4.5,               # cutoff between 4 and 5
    cluster = gs_sample$player_id
)
```

## 6. Event Study (Ranking Trajectory)

```
# ============================================================
# ESTIMATION E: Event Study around LL Entry (SUPPLEMENTARY)
# ============================================================

# Construct player-week panel centered on the LL event
# For each player-tournament in analysis_sample:
#   - Create weekly observations from t-26 to t+52
#   - Outcome: ranking position (or ranking points)

event_panel <- expand_grid(
    player_tournament = analysis_sample$id,
    relative_week = -26:52
) %>%
    merge with rankings on (player_id, date = event_date + relative_week * 7)

# Event study with player and calendar-week FEs
event_study <- lm(
    ranking ~ factor(relative_week) * is_ll +
              factor(player_id) + factor(calendar_week),
    data = event_panel
)

# Extract the interaction coefficients: beta[k] = effect of LL at week k
# Pre-period coefficients (k < 0) should be ~0 (parallel pre-trends)
# Post-period coefficients (k > 0) trace out the dynamic treatment effect

# Plot: event study graph with 95% CIs
# Pre-period: should show no differential trend
# Post-period: should show ranking improvement for LL recipients
```

## 7. Summary Table Construction

```
# ============================================================
# OUTPUT: Main Results Table
# ============================================================

# Table 1: First Stage and Balance
#   Panel A: Concordance between ranking position and LL assignment
#   Panel B: Covariate balance at the cutoff (rdrandinf on each X)

# Table 2: Main RDD Results
#   Rows: Outcomes (ranking change 26w, main draw entries 12m, ...)
#   Columns: (1) Local randomization, (2) rdrobust, (3) RDHonest
#   Report: point estimate, SE/CI, N, bandwidth/window

# Table 3: Grand Slam Lottery Results
#   Rows: Same outcomes
#   Columns: (1) ITT within pool, (2) IV/LATE
#   Report: point estimate, clustered SE, N

# Table 4: Heterogeneity
#   By tournament tier (G vs M vs A)
#   By career stage (early career vs established)
#   By proximity to financial break-even (~ranking 150)

# Table 5: Robustness
#   Bandwidth sensitivity, polynomial order, donut hole,
#   single-slot tournaments, sample period restrictions

# Figure 1: RDD Plot
#   Outcome (e.g., ranking change 26w) against R_tilde
#   Local polynomial fit on each side of cutoff
#   Binned scatter + CI bands

# Figure 2: Event Study
#   Relative week on x-axis, ranking (or ranking change) on y-axis
#   Coefficients from event study with 95% CIs
#   Vertical line at t=0 (LL event)

# Figure 3: Bandwidth Sensitivity
#   Point estimates + CIs across bandwidth multipliers (0.5x to 2x)
```
