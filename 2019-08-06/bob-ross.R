library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(scico)
library(ggplot2)
# devtools::install_github('hrbrmstr/pluralize')
library(pluralize)

if (!exists("bob_ross_csv")) {
  bob_ross_csv <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")  
}

add_singular_rule("aurora_borealis", "aurora_borealis")
add_singular_rule("deciduous", "deciduous")

bob_ross <- bob_ross_csv %>% 
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

# The frame and guest data seem to be rather uninteresting, so we'll drop them here...

bob_ross <- bob_ross %>%
  gather(element, present, -episode) %>%
  # Reduce plural elements to singular.
  mutate(
    element = gsub("_", " ", singularize(element))
  ) %>%
  group_by(episode, element) %>%
  summarise(present = max(present)) %>%
  ungroup() %>%
  filter(present == 1) %>%
  select(-present)

bob_ross <- bob_ross %>%
  separate(episode, into = c("season", "episode"), sep = "E") %>%
  mutate(season = str_extract(season, "[:digit:]+")) %>%
  mutate_at(vars(season, episode), as.integer)

# ---------------------------------------------------------------------------------------------------------------------

bob_ross %>%
  count(season, episode) %>%
  ggplot(aes(x = season, y = episode)) +
  geom_raster(aes(fill = n)) +
  geom_text(aes(label = n))

bob_ross %>%
  select(season, episode, guest) %>%
  unique() %>%
  ggplot(aes(x = season, y = episode)) +
  geom_raster(aes(fill = guest)) +
  geom_text(aes(label = guest))

bob_ross %>%
  count(season, element) %>%
  mutate(
    element = fct_reorder(element, n, sum, .desc = TRUE)
  ) %>%
  ggplot(aes(x = element, y = season)) +
  geom_raster(aes(fill = n), alpha = 0.75, show.legend = FALSE) +
  geom_text(aes(label = n), size = 2) +
  scale_y_continuous("Season", breaks = seq(1, 31, 1), expand = expand_scale(add = 0.5)) +
  scale_x_discrete("") + 
  scale_fill_scico(palette = "bilbao", direction = +1) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(family = "Pacifico", vjust = 0, hjust = 1),
    # plot.subtitle = element_text(family = "Pacifico", hjust = 0.0, vjust = -1.0, lineheight = 12),
    panel.background = element_blank()
  ) +
  labs(
    title = "Bob Ross - Count of Visual Elements by Season"
  )

ggsave(here::here("2019-08-06", "bob-ross-count-visual-element.png"), width = 8, height = 4.5)
