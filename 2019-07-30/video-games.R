library(tidyverse)
library(lubridate)

games <- read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-07-30/video_games.csv?raw=true")

games <- games %>%
  mutate(
    year = mdy(release_date) %>% year()
  ) %>%
  mutate(
    owners = str_replace(owners, "\\.\\.", "-"),
    owners = fct_reorder(owners, str_extract(owners, "[[:digit:],]+") %>% str_replace_all(",", "") %>% as.numeric())
  ) %>%
  # There is a single game with the release date in the "wrong" format.
  filter(!is.na(year))

ggplot(games, aes(factor(year))) +
  geom_bar(aes(fill = owners))

ggplot(games, aes(factor(year))) +
  geom_bar(aes(fill = owners), position = "fill")
#
# Games are becoming more fractured with time: more games with fewer owners.