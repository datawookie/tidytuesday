library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# Games are becoming more fractured with time: more games with fewer owners.

if (!exists("games_raw")) {
  games_raw <- read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-07-30/video_games.csv?raw=true")
}

games <- games_raw %>%
  mutate(
    year = mdy(release_date) %>% year() %>% as.integer() %>% as.factor(),
    owners = str_extract(owners, "[[:digit:],]+") %>%
      fct_other(keep = "0") %>%
      fct_recode(
        "less than 20,000" = "0",
        "more than 20,000" = "Other"
      )
  ) %>%
  # There is a single game with the release date in the "wrong" format.
  filter(!is.na(year))

p <- ggplot(games, aes(factor(year))) +
  labs(
    x = NULL
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.background = element_blank(),
    legend.position = c(0.7, 0.85)
  ) +
  scale_fill_manual("Number of owners", values = c("#3498db", "#8b9196"))

p1 <- p +
  geom_bar(aes(fill = owners)) +
  scale_y_continuous("Number of games") +
  labs(
    title = "Games on Steam",
    subtitle = "More games but fewer players per game"
  )

p2 <- p +
  geom_bar(aes(fill = owners), position = "fill", show.legend = FALSE) +
  scale_y_continuous("Proportion of games", labels = percent) +
  labs(
    caption = "data source: https://steamspy.com/"
  )

p1 + p2

ggsave(here::here("2019-07-30", "video-games.png"), width = 16, height = 9)
