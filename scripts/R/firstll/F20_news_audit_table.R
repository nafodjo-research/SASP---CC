# ==============================================================================
# F20_news_audit_table.R
# Appendix table documenting the news audit of ambiguous LL assignments.
#
# The R4 domain referee asked for the audit trail behind the classification
# claims in Section 7.3 to be made visible: which entries were checked, who
# withdrew, on what date, and from what source. This script renders that
# record from the classification file so the table cannot drift from the data.
#
# Inputs:
#   Data/cleaned/firstll/ll_lottery_classification.rds
#
# Outputs:
#   Tables_FirstLL/table_news_audit.tex
#
# Dependencies: here
# ==============================================================================

library(here)
source(here("scripts", "R", "firstll", "firstll_helpers.R"))

cl <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))
r  <- cl[cl$classification == "ranking", ]
r  <- r[order(r$tour, r$year), ]

esc <- function(x) gsub("&", "\\\\&", x)

L <- c("\\begin{tabular}{lllllc}",
       "\\toprule",
       "Tour & Year & Slam & LL entrant & Withdrawn player & Date \\\\",
       "\\midrule")

for (i in seq_len(nrow(r))) {
  x  <- r[i, ]
  wd <- if (is.na(x$withdrawal_date) || x$withdrawal_date == "")
          "not dated" else x$withdrawal_date
  wp <- if (is.na(x$withdrawn_player)) "--" else x$withdrawn_player
  L <- c(L, sprintf("%s & %d & %s & %s & %s & %s \\\\",
                    esc(x$tour), x$year, esc(x$slam_name),
                    esc(x$player_name), esc(wp), wd))
}

n_unres <- sum(cl$classification == "unresolved")
L <- c(L, "\\midrule",
       sprintf("\\multicolumn{6}{l}{\\textit{Unresolved (no datable source found): %d entries}} \\\\",
               n_unres),
       "\\bottomrule", "\\end{tabular}")

writeLines(paste(L, collapse = "\n"),
           file.path(FIRSTLL_TABLES, "table_news_audit.tex"))
message("Wrote table_news_audit.tex with ", nrow(r),
        " classified rows and ", n_unres, " unresolved.")
