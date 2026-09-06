# Check the FULL GS estimation sample
for (f in c("skeleton_gs_est.rds", "skeleton_gs_est_v2.rds", "skeleton_gs_est_v3.rds")) {
  fp <- file.path("Data/cleaned", f)
  if (file.exists(fp)) {
    d <- readRDS(fp)
    cat(f, ": N=", nrow(d), "\n")
    if ("tour" %in% names(d)) {
      cat("  ATP:", sum(d$tour == "ATP"), " WTA:", sum(d$tour == "WTA"), "\n")
    }
    if ("got_ll" %in% names(d)) {
      cat("  got_ll dist:\n")
      if ("tour" %in% names(d)) {
        cat("  ATP: "); print(table(d$got_ll[d$tour == "ATP"]))
        cat("  WTA: "); print(table(d$got_ll[d$tour == "WTA"]))
      } else {
        print(table(d$got_ll))
      }
    }
    cat("  Columns:", paste(head(names(d), 10), collapse=", "), "...\n\n")
  }
}

# Also check pooled_gs_lottery_sample
if (file.exists("Data/cleaned/pooled_gs_lottery_sample.rds")) {
  d <- readRDS("Data/cleaned/pooled_gs_lottery_sample.rds")
  cat("pooled_gs_lottery_sample.rds: N=", nrow(d), "\n")
  if ("tour" %in% names(d)) {
    cat("  ATP:", sum(d$tour == "ATP"), " WTA:", sum(d$tour == "WTA"), "\n")
  }
}
