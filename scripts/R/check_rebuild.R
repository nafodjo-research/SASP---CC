tr <- readRDS("Data/cleaned/tournament_rebuild_results.rds")
cat("Names:", paste(names(tr), collapse=", "), "\n\n")

for (nm in c("gs_atp", "gs_wta", "nongs_atp", "nongs_wta")) {
  if (!nm %in% names(tr)) next
  df <- tr[[nm]]
  if (is.null(df)) { cat(nm, ": NULL\n"); next }
  cat("===", nm, "===\n")
  cat("  Class:", class(df), "\n")
  if (is.data.frame(df)) {
    cat("  Rows:", nrow(df), "\n")
    cat("  Columns:", paste(names(df)[1:20], collapse=", "), "...\n")
    cat("  got_ll dist:", paste(names(table(df$got_ll)), table(df$got_ll), sep="=", collapse=", "), "\n")
  } else if (is.list(df)) {
    cat("  List names:", paste(names(df), collapse=", "), "\n")
    for (sub in names(df)) {
      if (is.data.frame(df[[sub]])) cat("    ", sub, ": ", nrow(df[[sub]]), " rows\n")
    }
  }
  cat("\n")
}
