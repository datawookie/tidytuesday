library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(scico)
library(ggplot2)

bob_ross <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv") %>% 
  janitor::clean_names() %>%
  mutate(
    title = gsub('(^"|"$)', '', title)
  ) %>%
  select(-framed)

# Fixes.
#
bob_ross <- bob_ross %>%
  mutate(
    double_oval_frame = ifelse(episode == "S18E10", 1, double_oval_frame),
    florida_frame = ifelse(episode == "S21E13", 1, florida_frame)
  )

frames <- bob_ross %>%
  select(episode, matches("frame$")) %>%
  gather(frame, present, -episode) %>%
  mutate(
    frame = sub("_frame", "", frame)
  ) %>%
  filter(present == 1) %>%
  select(-present)


bob_ross <- bob_ross %>%
  # Remove all "frame" columns (including "framed").
  select(-matches("frame")) %>%
  left_join(frames)

# Remove a couple of specific guests (the "guest" column seems to capture when a guest was present).
bob_ross <- bob_ross %>%
  select(-diane_andre, -steve_ross)

bob_ross <- bob_ross %>%
  separate(episode, into = c("season", "episode"), sep = "E") %>%
  mutate(season = str_extract(season, "[:digit:]+")) %>%
  mutate_at(vars(season, episode), as.integer)

season_episode_element <- bob_ross %>%
  select(-title, -frame, -guest) %>%
  gather(element, present, -season, -episode) %>%
  filter(present == 1) %>%
  mutate(
    element = gsub("_", " ", element)
  )

season_episode_element %>%
  count(season, episode) %>%
  ggplot(aes(x = season, y = episode)) +
  geom_raster(aes(fill = n)) +
  geom_text(aes(label = n))

season_episode_element %>%
  count(season, element) %>%
  mutate(
    element = fct_reorder(element, n, sum, .desc = TRUE)
  ) %>%
  ggplot(aes(x = element, y = season)) +
  geom_raster(aes(fill = n), alpha = 0.75, show.legend = FALSE) +
  geom_text(aes(label = n), size = 3) +
  scale_y_continuous("Season", breaks = seq(1, 31, 1), expand = expand_scale(add = 0.5)) +
  scale_x_discrete("") + 
  scale_fill_scico(palette = "bilbao", direction = +1) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    title = "Bob Ross",
    subtitle = "Count of Visual Elements by Season"
  )

ggsave(here::here("2019-08-06", "bob-ross.png"), width = 16, height = 9)
