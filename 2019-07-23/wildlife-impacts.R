library(dplyr)
library(readr)
library(lubridate)
library(forcats)
library(ggplot2)
library(gganimate)

# HELPER FUNCTIONS ----------------------------------------------------------------------------------------------------

# https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
#
get_season <- function(dates) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

# DATA ----------------------------------------------------------------------------------------------------------------

states <- read_csv("https://github.com/datawookie/data-diaspora/raw/master/spatial/usa-states.csv")

airports <- read_csv("https://github.com/datawookie/data-diaspora/blob/master/airports.csv?raw=true", comment = "#", na = "\\N") %>%
  select(
    airport_id = icao,
    name,
    lat,
    lon
  )

CSVFILE = "wildlife-impacts.csv"

if (!file.exists(CSVFILE)) {
  download.file("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-07-23/wildlife_impacts.csv?raw=true", CSVFILE)
}

impacts <- read_csv(CSVFILE)

impacts <- impacts %>%
  select(-cost_repairs_infl_adj) %>%
  mutate(
    time = time %/% 100 + (time %% 100) / 60,
    state = ifelse(state == "N/A", NA, state)
  ) %>%
  rename(local_time = time) %>%
  rename_at(
    vars(starts_with("incident")),
    ~ sub("incident_", "", .)
  ) %>%
  mutate(
    season = get_season(date)
  ) %>%
  select(date, year, month, everything()) %>%
  inner_join(airports %>% select(airport_id, airport_name = name))

# ---------------------------------------------------------------------------------------------------------------------

impacts_summary <- impacts %>%
  group_by(year, season) %>%
  mutate(date = floor_date(min(date), unit = "month")) %>%
  group_by(airport_id, date, season) %>%
  summarise(
    # date = floor_date(min(date), unit = "month"),
    incidents = n(),
    species = length(unique(species_id))
  ) %>%
  ungroup() %>%
  inner_join(airports)

(ggplot(impacts_summary, aes(x = lon, y = lat)) +
    geom_polygon(data = map_data("usa"), aes(x=long, y = lat, group = group), fill = "#CCCCCC") +
    geom_point(aes(size = incidents, colour = species), alpha = 0.2, show.legend = FALSE) + 
    coord_fixed(1.3, xlim = c(-140, -60), ylim = c(25, 50)) +
    labs(
      title = "Monthly Count of Wildlife Strikes",
      subtitle = "{strftime(current_frame, '%Y')} {get_season(current_frame)}"
    ) +
    theme_void() +
    transition_manual(date)) %>%
  animate(fps = 1, width = 1000)

impacts_local_time <- impacts %>%
  filter(
    !is.na(local_time)
  ) %>%
  inner_join(states, by = c(state = "code"))

# impacts_local_time <- impacts_local_time %>%
#   # semi_join(
#   #   impacts_local_time %>% count(airport_name) %>% filter(n > 100)
#   # ) %>%
#   mutate(
#     airport_name = fct_drop(airport_name),
#     # Order according to count.
#     airport_name = fct_reorder(airport_name, airport_name, length)
#   )
# 
# (ggplot(impacts_local_time, aes(x = local_time)) +
#   geom_text(
#     data = impacts_local_time %>% count(airport_name),
#     mapping = aes(x = Inf, y = Inf, label = n),
#     hjust   = +1.0,
#     vjust   = +1.25,
#     size = 48,
#     color = "#6c757d"
#   ) +
#   geom_histogram(breaks = seq(0, 24, 1), alpha = 0.5, fill = "#3498db") +
#   scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24), labels = function(h) sprintf("%02d:00", h)) +
#   labs(
#     x = NULL,
#     y = "Number of Strikes",
#     title = "{closest_state}"
#   ) +
#   theme(
#     panel.background = element_blank(),
#     text = element_text(size = 18)
#   ) +
#   transition_states(airport_name)) %>%
#   animate(width = 1000, nframes = 132)

impacts_local_time <- impacts_local_time %>%
  # semi_join(
  #   impacts_local_time %>% count(airport_name) %>% filter(n > 100)
  # ) %>%
  mutate(
    name = fct_drop(name),
    # Order according to count.
    name = fct_reorder(name, name, length)
  )

(ggplot(impacts_local_time, aes(x = local_time)) +
    geom_histogram(breaks = seq(0, 24, 1), alpha = 0.75, fill = "#3498db") +
    geom_text(
      data = impacts_local_time %>% count(name),
      mapping = aes(x = 24, y = Inf, label = n),
      hjust   = +1.0,
      vjust   = +1.25,
      size = 48,
      color = "#6c757d"
    ) +
    annotate("text", x = 24, y = Inf, label = "Total",
             hjust   = +1.0,
             vjust   = +12.5, color = "#6c757d", size = 6) +
    scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24), labels = function(h) sprintf("%02d:00", h)) +
    labs(
      x = NULL,
      y = "Number of Strikes",
      title = "Wildlife Strikes versus Local Time",
      subtitle = "{closest_state}",
      caption = "data source: FAA Wildlife Strike Database"
    ) +
    theme(
      panel.background = element_blank(),
      text = element_text(size = 18)
    ) +
    transition_states(name)) %>%
  animate(fps = 5, width = 800, height = 450, end_pause = 10, nframes = 118)

anim_save("wildlife-strike.gif")
