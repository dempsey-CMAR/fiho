library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(stringr)
library(tidyr)

theme_set(theme_light())

pal <- c("#004077", "#AC8400", "#800000", "#001B40", "#c8102e", "#789D4A")

# acadia red: #c41424
# mta garnet: #8e3337

dat_raw <- read_csv(
  here("data/2023_dal_field_hockey_stats.csv"),
  show_col_types = FALSE,
  na = "na"
)

dat <- dat_raw %>%
  mutate(
    field_goals_for = goals_for - corner_goals_for,
    field_goals_against = goals_against - corner_goals_against
  ) %>%
  pivot_longer(
    cols = -c(tournament, game, opponent),
    names_to = "statistic", values_to = "value"
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

ggplot(dat, aes(game, value, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~statistic, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 16))


ggsave(filename = here("figures/2022_stats_overview.png"),
       device = "png",
       units = "cm",
       height = 17,
       width = 22
)




dat %>%
  filter(str_detect(statistic, "_goals_for")) %>%
  ggplot(aes(game, value, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~statistic, ncol = 1) +
  theme(text = element_text(size = 16))


ggsave(filename = here("figures/2022_goals_for.png"),
       device = "png",
       units = "cm",
       height = 12,
       width = 15
)


dat %>%
  filter(str_detect(statistic, "circle_entries")) %>%
  ggplot(aes(game, value, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~statistic, ncol = 2) +
  theme(text = element_text(size = 16))


ggsave(filename = here("figures/2022_circle_entries.png"),
       device = "png",
       units = "cm",
       height = 8,
       width = 22
)








dat_summary <- dat %>%
  group_by(statistic) %>%
  summarise(mean_stat = mean(value, na.rm = TRUE))




conversions <- dat_raw %>%
  mutate(
    non_corner_goals = goals_for - corner_goals_for,
    entry_shot_conversion = shots_for / circle_entries_for,
    entry_goal_conversion = goals_for / circle_entries_for,
    entry_corner_conversion = corners_for / circle_entries_for,
    corner_goal_conversion = corner_goals_for / corners_for,
    shot_goal_conversion = non_corner_goals / shots_for
  ) %>%
  select(opponent, game, circle_entries_for, contains("conversion")) %>%
  pivot_longer(
    cols = -c(game, opponent),
    names_to = "stat", values_to = "value"
  ) %>%
  mutate(
    stat = ordered(
      stat,
      levels = c(
        "circle_entries_for",
        "entry_shot_conversion",
        "entry_goal_conversion",
        "shot_goal_conversion",
        "entry_corner_conversion",
        "corner_goal_conversion"
      )
    )
  )

ggplot(conversions, aes(game, value, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~stat, scales = "free_y", ncol = 2)

ggplot(conversions, aes(game, value, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(~stat, ncol = 1)


# ggplot(dat, aes(game, value, col = opponent)) +
#   geom_point() +
#   scale_colour_manual(values = pal) +
#   facet_wrap(~stat, scales = "free_y")



