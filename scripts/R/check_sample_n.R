tr <- readRDS("Data/cleaned/tournament_rebuild_results.rds")
cat("=== tournament_rebuild_results ===\n")
cat("GS-ATP events:", nrow(tr$gs_atp$events), "\n")
cat("GS-WTA events:", nrow(tr$gs_wta$events), "\n")
cat("GS-ATP unique players:", length(unique(tr$gs_atp$events$player_id)), "\n")
cat("GS-WTA unique players:", length(unique(tr$gs_wta$events$player_id)), "\n")

# Check if there's a had_prior_ll or first_ll filter
cat("\nGS-ATP had_prior_ll dist:\n")
print(table(tr$gs_atp$events$had_prior_ll))

# Find the FULL GS sample
gs_files <- list.files("Data/cleaned/", pattern="gs|skeleton", full.names=TRUE)
cat("\nAll relevant cleaned files:\n")
cat(paste(gs_files, collapse="\n"), "\n")

# Check the original skeleton files
for (f in c("Data/cleaned/gs_atp_skeleton.rds", "Data/cleaned/gs_wta_skeleton.rds",
            "Data/cleaned/skeleton_gs_atp.rds", "Data/cleaned/skeleton_gs_wta.rds")) {
  if (file.exists(f)) {
    d <- readRDS(f)
    cat("\n", f, ": N=", nrow(d), "\n")
    if ("got_ll" %in% names(d)) print(table(d$got_ll))
  }
}

# Check script 17 output
if (file.exists("Data/cleaned/skeleton_atp.rds")) {
  d <- readRDS("Data/cleaned/skeleton_atp.rds")
  cat("\nskeleton_atp.rds: N=", nrow(d), "\n")
  if ("tourney_level" %in% names(d)) {
    gs <- d[d$tourney_level == "G", ]
    cat("  GS subset: N=", nrow(gs), "\n")
  }
}
