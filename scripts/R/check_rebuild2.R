tr <- readRDS("Data/cleaned/tournament_rebuild_results.rds")
# Check event table columns
ev <- tr$gs_atp$events
cat("=== GS ATP Event table columns ===\n")
print(names(ev))
cat("\nSample:\n")
print(head(ev[, c("event_id", "player_id", "tourney_id", "got_ll", "pre_elo", "pre_rank_pts")], 5))

# Check match table columns
mt <- tr$gs_atp$matches
cat("\n=== GS ATP Match table columns ===\n")
print(names(mt))

# Check non-GS event table
ev2 <- tr$nongs_atp$events
cat("\n=== NonGS ATP Event table columns ===\n")
print(names(ev2))

# Check for next_ll_opp_date or similar censoring column
cat("\nCensoring columns in events:\n")
censor_cols <- grep("next|censor|cap|window", names(ev), value=TRUE, ignore.case=TRUE)
print(censor_cols)
censor_cols2 <- grep("next|censor|cap|window", names(ev2), value=TRUE, ignore.case=TRUE)
print(censor_cols2)
