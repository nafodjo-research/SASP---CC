library(ggplot2)
library(here)
theme_paper <- function(base_size = 14) {
  theme_minimal(base_family = "serif", base_size = base_size) +
    theme(legend.position = "bottom")
}
r <- readRDS(here("Data/cleaned/tournament_dose_results.rds"))
dose <- r[["dose"]]
dd <- do.call(rbind, lapply(names(dose), function(nm) {
  x <- dose[[nm]]
  imp <- x[["implied"]]
  imp[["sample"]] <- nm
  imp
}))
message("Rows: ", nrow(dd))
p <- ggplot(dd, aes(x = wins, y = total, color = sample)) +
  geom_ribbon(aes(ymin = total - 1.96 * se, ymax = total + 1.96 * se, fill = sample),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Matches Won at LL Event", y = "Total effect (log-odds)",
       color = NULL, fill = NULL) +
  theme_paper()
ggsave(here("Figures/fig_tournament_dose.pdf"), p, width = 6.5, height = 4.5,
       device = cairo_pdf)
message("Saved: fig_tournament_dose.pdf")
