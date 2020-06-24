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
library(grid)
# Specify conflicts
conflict_prefer("View", "base")
conflict_prefer("select", "tidylog")
conflict_prefer("summarise", "tidylog")
conflict_prefer("group_by", "tidylog")
conflict_prefer("arrange", "tidylog")
conflict_prefer("filter", "tidylog")
conflict_prefer("mutate", "tidylog")
# Vintage 70s Style
extrafont::font_import("~/Library/Fonts")
pallet_70 <- c("#17224d", "#07698e","#2489a0","#f4d7ab",
               "#f4a665","#f26a2d","#ef3c23")

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

# What happened to Her? ####
caribou_prime_meta <-  tuesdata$individuals %>% 
  filter(animal_id == starting_caribou$animal_id) 

caribou_prime_meta %>% glimpse

# Graph 1 Seasonal Distance ####
round_background <- function(p){
  require(grid)
  require(ggplotify)
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                            r=unit(0.1, "snpc"),
                            just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
  g$grobs[[1]] <- round_bg
  g <- ggplotify::as.ggplot(g)
  return(g)
}

plot1 <- caribou_prime %>%
  group_by(year, month) %>%
  summarise(distance = sum(distance_m, na.rm=TRUE)/1000) %>%
  tidylog::ungroup()

plot1 <- plot1 %>%
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%b-%d"),
         year = factor(year)
         ) 

glimpse(plot1)
library(ggtext)
graph1 <- ggplot(plot1,
       aes(month, distance, group = year, color = year)) + 
  geom_line(lineend = "round", size = 1.2) + 
  scale_x_discrete(expand = c(0,0), breaks = month.abb) + 
  coord_polar() +
  theme(plot.background = element_rect(fill = pallet_70[[4]]),
        panel.background = element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        plot.title = element_text(family = "Alba Super",
                                  size = 20, color = pallet_70[[1]],
                                  ),
        text=element_text(size=14,  family="Alba", color = pallet_70[[1]]),
        legend.position = "none",
        plot.caption = element_markdown(size = 10)
        ) +
  scale_color_manual(values = pallet_70[c(-4:-5)]) +
  labs(
    title = "Total Distance Traveled",
    subtitle = glue("Caribou: {starting_caribou$animal_id}"),
    y = "Distance (km)",
    x = "Month",
    color = "Year",
    caption = "**Year:** <span style='color:#17224d;'>2011</span>,
    <span style='color:#07698e;'>2012</span>,
    <span style='color:#2489a0;'>2013</span>,
    <span style='color:#f26a2d;'>2014</span>,
    <span style='color:#ef3c23;'>2015</span>,
    "
  ) 


graph1 <- round_background(graph1) 

width = 800 
height = 500

img <- image_read(here::here("img", "Scratchy white.png"))
pomo_bg <- magick::image_resize(img, paste0(width, "x", height, "!"))
pomo_bg <- magick::image_crop(pomo_bg, paste0(width, "x", height))

gg_fig <- magick::image_graph(width, height, bg = "transparent", pointsize = 16)
print(graph1)
dev.off()
graph1 <- magick::image_composite(gg_fig, pomo_bg)
image_write(graph1, path = here::here("img","graph1.png"), format = "png")
dev.off()

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

graph2 <- ggplot() +
  geom_polygon(data = canada,
               aes(x = long, y = lat, group=group),
               fill="lightgray", colour = "white") +
  theme_minimal() +
  coord_map(xlim = c(-122.5, -121),
            ylim = c(54.5, 55.5)) +
  geom_hoof(data = caribou_prime, aes(x = longitude, y = latitude), size = .02) +
  labs(
    title = "Movement in Real Time {frame_time}",
    subtitle = glue("Caribou: {starting_caribou$animal_id}")
  ) +
  transition_time(timestamp) 

graph2 <- animate(graph2)

anim_save(animation = graph2,filename = here::here("img","geom_hoof_example.gif"))


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
