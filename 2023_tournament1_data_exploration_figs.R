library(dplyr)
library(ggplot2)
library(googlesheets4)
library(here)
library(readr)
library(stringr)
library(tidyr)

theme_set(theme_light())

pal <- data.frame(
  opponent = c("aca", "mta", "smu", "stfx", "unb", "upei"),
  pal = c("#004077", "#AC8400", "#800000", "#001B40", "#c8102e", "#789D4A")
)

# acadia red: #c41424
# mta garnet: #8e3337

link <- "https://docs.google.com/spreadsheets/d/16XllZfyCBq3mYS4Je4-WY9oVEL_jU2SUfoHaRKT_Ni0/edit#gid=0"

dat_raw <- read_sheet(link)

pal <- pal %>%
  filter(opponent %in% unique(dat_raw$opponent))
pal <- pal$pal

dat <- dat_raw %>%
  mutate(
    field_goals_for = goals_for - corner_goals_for,
    field_goals_against = goals_against - corner_goals_against
  ) %>%
  pivot_longer(
    cols = -c(tournament, game, opponent),
    names_to = "statistic", values_to = "number"
  ) %>%
  mutate(
    statistic = ordered(
      statistic,
      levels = c(
        "goals_for", "goals_against",
        "circle_entries_for", "circle_entries_against",
        "shots_for", "shots_against",
        "corners_for", "corners_against",
        "corner_goals_for", "corner_goals_against",
        "field_goals_for", "field_goals_against")
    )
  )

ggplot(dat, aes(game, number, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~statistic, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 14))


ggsave(filename = here("figures/2023_tournament_1_stats_overview.png"),
       device = "png",
       units = "cm",
       height = 17,
       width = 22
)



# figure loop -------------------------------------------------------------

stats <- dat %>%
  mutate(statistic = as.character(statistic)) %>%
  distinct(statistic)
stats <- stats$statistic

for(i in seq(1, by = 2, length(stats))) {

  dat_i <- dat %>%
    filter(
      statistic == stats[i] | statistic == stats[i + 1]
    )

  p <- ggplot(dat_i, aes(game, number, fill = opponent)) +
    geom_col() +
    scale_fill_manual(values = pal) +
    facet_wrap(~statistic, ncol = 1) +
    theme(text = element_text(size = 14))


  ggsave(
    p,
    filename = here(paste0("figures/2023/2023_", stats[i], ".png")),
    device = "png",
    units = "cm",
    height = 10,
    width = 20
  )

}



# summary figs ------------------------------------------------------------

dat %>%
  filter(
    str_detect(statistic, "_for"),
         statistic != "goals_for"
    ) %>%
  ggplot(aes(statistic, number, group = opponent, col = opponent)) +
  geom_point(size = 4) +
  geom_line(linewidth = 0.75) +
  scale_colour_manual(values = pal) +
  scale_x_discrete(name = "") +
  theme(
    text = element_text(size = 16),
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(colour = 1)
  )

ggsave(
  filename = here("figures/2023/2023_summary_line.png"),
  device = "png",
  units = "cm",
  height = 10,
  width = 20
)


dat %>%
  filter(
    str_detect(statistic, "_for"),
    statistic != "goals_for"
  ) %>%
  ggplot(aes(statistic, number, group = opponent, fill = opponent)) +
  geom_col() +
  scale_x_discrete(name = "") +
  scale_fill_manual(values = pal, guide = "none") +
  facet_wrap(~opponent, ncol = 1)

ggsave(
  filename = here("figures/2023/2023_summary_bar.png"),
  device = "png",
  units = "cm",
  height = 10,
  width = 15
)
