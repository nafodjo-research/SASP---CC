est <- readRDS("Data/cleaned/skeleton_nongs_est_v2.rds")
cat("ATP events:", length(unique(est$tourney_id[est$tour == "ATP"])), "\n")
cat("WTA events:", length(unique(est$tourney_id[est$tour == "WTA"])), "\n")

atp_q <- readRDS("Data/raw/atp_qual_chall_matches.rds")
wta_q <- readRDS("Data/raw/wta_qual_itf_matches.rds")

atp_est_ids <- unique(est$tourney_id[est$tour == "ATP"])
wta_est_ids <- unique(est$tourney_id[est$tour == "WTA"])

atp_m <- atp_q[atp_q$tourney_id %in% atp_est_ids & grepl("^Q", atp_q$round), ]
wta_m <- wta_q[wta_q$tourney_id %in% wta_est_ids & grepl("^Q", wta_q$round), ]

cat("\nATP qual matches at est events:", nrow(atp_m), "\n")
print(table(atp_m$round))
cat("WTA qual matches at est events:", nrow(wta_m), "\n")
print(table(wta_m$round))

cat("\nATP winner_rank_points avail:", sum(!is.na(atp_m$winner_rank_points)), "/", nrow(atp_m), "\n")
cat("WTA winner_rank_points avail:", sum(!is.na(wta_m$winner_rank_points)), "/", nrow(wta_m), "\n")
cat("ATP w_svpt avail:", sum(!is.na(atp_m$w_svpt)), "/", nrow(atp_m), "\n")
cat("WTA w_svpt avail:", sum(!is.na(wta_m$w_svpt)), "/", nrow(wta_m), "\n")
