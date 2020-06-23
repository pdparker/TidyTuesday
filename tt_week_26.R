# Stock packages
library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(magick)
library(purrr)
library(tidylog)
library(lubridate)
library(conflicted)
# Project Specific
library(gganimate)
library(geosphere)
library(maps)
library(ggmap)
# Specify conflicts
conflict_prefer("View", "base")
conflict_prefer("select", "tidylog")
conflict_prefer("summarise", "tidylog")
conflict_prefer("group_by", "tidylog")
conflict_prefer("arrange", "tidylog")
conflict_prefer("filter", "tidylog")
conflict_prefer("mutate", "tidylog")
#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 26)

# Data Munging ####
# Start with Caribou with most data
set.seed(42)
starting_caribou <- tuesdata$locations %>%
  group_by(animal_id) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  `[`(1,1) 

caribou_prime <- tuesdata$locations %>%
  filter(animal_id == starting_caribou$animal_id) %>%
  arrange(timestamp) %>%
  mutate(lag_longitude = dplyr::lag(longitude),
         lag_latitude = dplyr::lag(latitude),
         year = year(timestamp),
         month = month(timestamp, label = TRUE),
         day = wday(timestamp)) %>%
  mutate(distance_m = distHaversine(cbind(longitude,latitude),
                                    cbind(lag_longitude,lag_latitude)
                                    )
         ) 

glimpse(caribou_prime)

# What happened to him/Her? ###
caribou_prime_meta <-  tuesdata$individuals %>% 
  filter(animal_id == starting_caribou$animal_id) 

caribou_prime_meta %>% glimpse

# Graph 1 Seasonal Distance ####
plot1 <- caribou_prime %>%
  group_by(year, month) %>%
  summarise(distance = sum(distance_m, na.rm=TRUE)/1000) %>%
  tidylog::ungroup()

plot1 <- plot1 %>%
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%b-%d"),
         year = factor(year)
         ) 

glimpse(plot1)

ggplot(plot1,
       aes(month, distance, group = year, color = year)) + 
  geom_line() + 
  scale_x_discrete(expand = c(0,0), breaks = month.abb) + 
  theme_minimal() +
  coord_polar() +
   transition_manual(year) 

# Graph 2 Movement across Canada ####
canada <- map_data("world", region = "Canada")
#Where in Canada
ggplot() +
  geom_polygon(data = canada,
               aes(x = long, y = lat, group=group),
               fill="lightgray", colour = "white") +
  theme_minimal() +
  coord_map()+
  geom_point(data = caribou_prime, aes(x = longitude, y = latitude, colour = 'red'))
#Close in on specific position
long_range <- caribou_prime %>% summarise(range(longitude, na.rm=TRUE)) 
lat_range <- caribou_prime %>% summarise(range(latitude, na.rm=TRUE)) 

ggplot() +
  geom_polygon(data = canada,
               aes(x = long, y = lat, group=group),
               fill="lightgray", colour = "white") +
  theme_minimal() +
  coord_map(xlim = c(-122.5, -121),
            ylim = c(54.5, 55.5)) +
  geom_point(data = caribou_prime, aes(x = longitude, y = latitude,
                                       colour = 'red')) +
  labs(
    title = "Movement in Real Time {frame_time}",
    subtitle = glue("Caribou: {starting_caribou$animal_id}")
  ) +
  transition_time(timestamp) +
  shadow_mark(past = T, future=F, alpha=0.3)

# Graph 3 Lattitude ####
caribou_prime %>%
  ggplot(aes(x = timestamp, y = latitude)) +
  geom_line() + 
  theme_minimal() + 
  coord_flip() +
  labs(
    title = "Movement on latitude at {frame_along}",
    subtitle = glue("Caribou: {starting_caribou$animal_id}")
  ) +
  transition_reveal(timestamp)

# Graph 4 Longitude ####
caribou_prime %>%
  ggplot(aes(x = timestamp, y = longitude)) +
  geom_line() + 
  theme_minimal() + 
  labs(
    title = "Movement on longitude at {frame_along}",
    subtitle = glue("Caribou: {starting_caribou$animal_id}")
  ) +
  transition_reveal(timestamp)
