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
library(patchwork)
library(fuzzyjoin)
# Project Specific
library(gganimate)
library(geosphere)
library(maps)
library(ggmap)
library(grid)
library(cowplot)
library(sf)
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

wolf <- image_read(here::here("img", "wolf.png") )
wolf <- magick::image_resize(wolf, paste0(480, "x", 480, "!"))
wolf <- magick::image_crop(wolf, paste0(480, "x", 480))

anim_save(animation = graph2,
          filename = here::here("img","geom_hoof_example.gif"))

plot <- image_read(here::here("img", "geom_hoof_example.gif"))


anim_save(animation = c(plot, wolf, wolf), 
          filename = here::here("img","geom_hoof_example_2.gif"))

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



# Graph 5 ####
bc <- bcmapsdata::bc_bound
#bc <- as.data.frame(bc$geometry)
yogi <- c("#9dccf8","#6cb179",
          "#9a6939","#ffd38c",
          "#a3d29b","#000004",
          "#f36d74")

join <- tribble(
  ~ Killer, ~ reg,
  "Wolf", "Wolf",
  "Bear", "Grizzly"
)


bear_attacks <- tuesdata$individuals %>% 
  regex_inner_join(join, by = c(death_cause = "reg")) %>%
  filter(Killer %in% c("Wolf", "Bear"))

attack_bears <- tuesdata$locations %>%
  filter(animal_id %in% bear_attacks$animal_id) %>%
  tidylog::left_join(.,bear_attacks, by = "animal_id")

sites <- st_as_sf(attack_bears[,c("longitude", "latitude")],
                  coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant") %>%
  st_transform(crs = 3005)



attack_bears <- attack_bears %>%
  bind_cols(.,sites %>% st_coordinates() %>% as.data.frame())


yogi_img <- image_read(here::here("img","yogi2.png"))

plot_box <- tibble(xmin = 1045290,
                   xmax = 1477030,
                   ymin = 1000000,
                   ymax = 1320232,
                   xrange = xmax - xmin,
                   yrange = ymax - ymin)


graph5 <- ggplot(bc) +
  geom_sf(color = "#6cb179", fill="#6cb179") +
  geom_path(data = attack_bears,
            aes(x=X, y=Y, group = animal_id, color = Killer)) +
  scale_color_manual(values = yogi[3:4]) +
  coord_sf(xlim = c(1045290, 1477030), ylim = c(1000000, 1320232) ) +
  theme_minimal()+
  theme(panel.background =
          element_rect(fill = "#9dccf8"),
                       plot.title = element_text(family = "Risque",
                                                     size = 20, color = "#f46d74"),
        plot.subtitle  = element_text(family = "Risque",
                                  size = 16, color = "#f46d74"),
                       text=element_text(size=14,  family="Bubblegum Sans",
                                                          color = "#f46d74")
  ) +
  labs(
    title = "The Truth About Yogi",
    subtitle = "Caribou Travels Before being Killed by Bears or Wolves",
    color = "Killed By:",
    y = "Latitude",
    x = "Longitude"
    ) + 
  annotation_raster(yogi_img, 
                    ymin = -Inf, 
                    ymax = plot_box$ymin + plot_box$yrange*.2, 
                    xmin = -Inf, 
                    xmax = plot_box$xmin + plot_box$xrange * .2
                    ) +
  transition_reveal(timestamp)

graph5 <- animate(graph5, width = 480, height = 480)

wolf <- image_read(here::here("img", "wolf.png") )
wolf <- magick::image_resize(wolf, paste0(480, "x", 480, "!"))
wolf <- magick::image_crop(wolf, paste0(480, "x", 480))

anim_save(animation = graph5,
          filename = here::here("img","yogi.gif"))

plot <- image_read(here::here("img", "yogi.gif"))


anim_save(animation = c(plot, wolf, wolf), 
          filename = here::here("img","yogi.gif"))



