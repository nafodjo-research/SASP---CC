# ==============================================================================
# F14b_curation_checklist.R
# Produce a curation checklist for the 23 ambiguous GS LL entries left
# unclassified by F14. Each entry needs external evidence about when the
# withdrawal happened relative to the qualifying-round completion.
#
# For each ambiguous row, this script builds:
#   - Wikipedia tournament-article URLs (men's and women's singles main draw
#     and singles qualifying)
#   - Wayback Machine query URLs for the tournament's official press site
#     and draws page, timestamped around the tournament's start
#   - Google News search URLs (bounded by date) for the player and
#     tournament, and for the phrase "lucky loser" in that tournament
#   - A checklist row a curator can fill in with source + withdrawal_date
#
# METHOD
# ======
# The curator's task per row is: find any credible source that dates the
# main-draw withdrawal (the one that opened the lucky-loser slot the treated
# player received). Once dated, compare to qualifying-round completion
# (roughly two days before the tourney_date shown in Sackmann; the exact
# date can be read from the tournament's official schedule linked below).
#
#   withdrawal_date <= qualifying_completion_date  -> lottery
#   withdrawal_date  > qualifying_completion_date  -> ranking
#
# If no source dates the withdrawal, mark the row "unresolved" and note
# what was searched, so the audit trail is clean.
#
# INPUTS
# ======
#   Data/cleaned/firstll/ll_lottery_classification.rds  (from F14)
#
# OUTPUTS
# =======
#   Output_FirstLL/F14b_curation_checklist.md
#
# Dependencies: dplyr, here, utils::URLencode
# ==============================================================================

set.seed(20260416)

library(dplyr)
library(here)

source(here("scripts", "R", "firstll", "firstll_helpers.R"))
summary_log <- character()


# ==============================================================================
# STEP 1: LOAD
# ==============================================================================
classif <- readRDS(file.path(FIRSTLL_CLEANED, "ll_lottery_classification.rds"))
amb <- classif[classif$classification == "ambiguous", ]
message(sprintf("Loaded %d ambiguous rows for curation checklist.", nrow(amb)))


# ==============================================================================
# STEP 2: URL BUILDERS
# ==============================================================================

# Wikipedia slam-article base names for men's and women's singles.
wiki_singles <- function(year, slam, tour) {
  slam_norm <- switch(slam,
    "Australian Open" = "Australian_Open",
    "Roland Garros"   = "French_Open",
    "Wimbledon"       = "Wimbledon_Championships",
    "US Open"         = "US_Open_(tennis)",
    slam
  )
  gender <- ifelse(tour == "ATP", "Men%27s_Singles", "Women%27s_Singles")
  paste0("https://en.wikipedia.org/wiki/",
         year, "_", slam_norm, "_%E2%80%93_", gender)
}

wiki_qualifying <- function(year, slam, tour) {
  slam_norm <- switch(slam,
    "Australian Open" = "Australian_Open",
    "Roland Garros"   = "French_Open",
    "Wimbledon"       = "Wimbledon_Championships",
    "US Open"         = "US_Open_(tennis)",
    slam
  )
  gender <- ifelse(tour == "ATP", "Men%27s_Singles_Qualifying",
                                  "Women%27s_Singles_Qualifying")
  paste0("https://en.wikipedia.org/wiki/",
         year, "_", slam_norm, "_%E2%80%93_", gender)
}

# Wayback Machine URL for the tournament press site around the event date.
# Slam official domains at time of writing.
tournament_domain <- function(slam) {
  switch(slam,
    "Australian Open" = "ausopen.com",
    "Roland Garros"   = "rolandgarros.com",
    "Wimbledon"       = "wimbledon.com",
    "US Open"         = "usopen.org",
    NA_character_
  )
}

wayback <- function(domain, year_date) {
  if (is.na(domain)) return(NA_character_)
  # Wayback query for that domain around the tournament date
  paste0("https://web.archive.org/web/", year_date, "*/", domain)
}

# Google News search: player, tournament, and "withdraws OR withdrawal"
gnews <- function(player, slam, year) {
  q <- URLencode(sprintf('"%s" "%s" %d withdraws', player, slam, year))
  paste0("https://www.google.com/search?tbm=nws&q=", q)
}

gnews_ll <- function(slam, year) {
  q <- URLencode(sprintf('"lucky loser" "%s" %d', slam, year))
  paste0("https://www.google.com/search?tbm=nws&q=", q)
}

# ATP / WTA press site search
atp_wta_search <- function(tour, player, year) {
  q <- URLencode(sprintf('%s withdraws %d', player, year))
  if (tour == "ATP") {
    paste0("https://www.atptour.com/en/search?query=", q)
  } else {
    paste0("https://www.wtatennis.com/search?q=", q)
  }
}


# ==============================================================================
# STEP 3: WRITE CHECKLIST
# ==============================================================================
out <- character()
out <- c(out,
  "# F14b Curation Checklist — Ambiguous GS LL Assignments",
  "",
  sprintf("**Rows to classify:** %d (10 ATP, 13 WTA)  ", nrow(amb)),
  "**Task per row:** Find the date the main-draw withdrawal was announced. Compare to qualifying-round completion (~2 days before the tournament date below). Record the source, withdrawal date, and update classification in `Data/cleaned/firstll/ll_lottery_classification.csv`.",
  "",
  "**Classification rule:**",
  "",
  "- `withdrawal_date <= qualifying_end` → `lottery`",
  "- `withdrawal_date > qualifying_end` → `ranking`",
  "- No date-able source found → `unresolved` (leave in the audit set)",
  "",
  "---",
  ""
)

for (i in seq_len(nrow(amb))) {
  r <- amb[i, ]
  domain <- tournament_domain(r$slam_name)
  wb <- wayback(domain, r$tourney_date)
  wiki_q <- wiki_qualifying(r$year, r$slam_name, r$tour)
  wiki_s <- wiki_singles(r$year, r$slam_name, r$tour)
  gn <- gnews(r$player_name, r$slam_name, r$year)
  gn_ll <- gnews_ll(r$slam_name, r$year)
  atp_wta <- atp_wta_search(r$tour, r$player_name, r$year)

  out <- c(out,
    sprintf("## %d. %s — %s %d (%s)",
            i, r$player_name, r$slam_name, r$year, r$tour),
    "",
    sprintf("- **Player rank at event:** %d", r$player_rank),
    sprintf("- **Rank among losers:** %d (top-1: could be lottery OR ranking)", r$rank_among_losers),
    sprintf("- **LL slots at event:** %d", r$n_ll_slots),
    sprintf("- **Tournament date (Sackmann):** %s", format(r$event_date)),
    "",
    "**Sources to check** (in order of expected yield):",
    "",
    sprintf("1. [Wikipedia (main draw)](%s)", wiki_s),
    sprintf("2. [Wikipedia (qualifying)](%s)", wiki_q),
    if (!is.na(wb)) sprintf("3. [Wayback Machine (%s)](%s)", domain, wb)
                    else "3. Wayback Machine: n/a (unknown official domain)",
    sprintf("4. [Google News (player + withdraws)](%s)", gn),
    sprintf("5. [Google News (\"lucky loser\" + tournament)](%s)", gn_ll),
    sprintf("6. [%s press-site search](%s)", r$tour, atp_wta),
    "",
    "**Fields to fill in the CSV:**",
    "",
    "```",
    "source:           <URL of the citing article>",
    "withdrawal_date:  <YYYY-MM-DD>",
    "withdrawn_player: <name, if identifiable>",
    "classification:   lottery | ranking | unresolved",
    "confidence:       high | medium | low",
    "verifier_notes:   <one-line reason>",
    "```",
    "",
    "---",
    ""
  )
}

out <- c(out,
  "## After curation",
  "",
  "Run `F14c_manual_review.R` (to be written) to:",
  "",
  "1. Read the updated CSV back in",
  "2. Re-count lottery-verified sample sizes",
  "3. Re-estimate the headline stacked model on the expanded verified subsample",
  "4. Regenerate `Tables_FirstLL/table_verified_firstll.tex`",
  "5. Update the robustness section's verified-lottery numbers",
  ""
)

path <- file.path(FIRSTLL_OUTPUT, "F14b_curation_checklist.md")
writeLines(out, path)
message("Wrote curation checklist to ", path)
message("DONE.")
