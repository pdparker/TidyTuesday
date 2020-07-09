# Stock packages
library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(magick)
library(purrr)
library(tidylog)
library(lubridate)
library(patchwork)
library(fuzzyjoin)
library(ggtext)
library(broom)
library(forcats)
#Load fonts
extrafont::font_import("~/Library/Fonts")
extrafont::fonts()
# Week specific packages
library(ggmap)
# color pallett options for this week
source(here("geom_bean.R"))
png(here("img","week28_palette.png"))
par(mfrow=c(3,1))
print.palette(coffee_col$pal1)
print.palette(coffee_col$pal2)
print.palette(coffee_col$pal3)
par(mfrow=c(1,1))
dev.off()
# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
# Explore data
#quick look
tuesdata$coffee_ratings %>%
  glimpse()
# detailed look
tuesdata$coffee_ratings %>% 
  skimr::skim() 

# Cleaning Tribble
cnt <- tribble(
  ~ regex, ~cnt,
  "Cote", "Ivory Coast",
  "United States", "USA",
  "Tanzania", "Tanzania",
)

# Clean country names to match ggmap
coffee <- tuesdata$coffee_ratings %>%
  regex_full_join(cnt, by = c(country_of_origin = "regex")) %>% 
  mutate(cnt = ifelse(is.na(cnt), country_of_origin, cnt)) %>%
  count(cnt)

# Remove countries with less than 3 ratings
removes <- coffee %>%
  filter(n < 3) %>%
  select(cnt) %>%
  unlist
 
# Get map data 
world <- map_data("world") %>%
  left_join(coffee, by = c(region = "cnt")) %>%
  mutate(coffee_country = ifelse(is.na(n), '0','1'))

# Plot coffee world
p1 <- ggplot(world) +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group, fill = coffee_country)) +
  scale_fill_manual(values = c(coffee_col$pal1[["mountain_mist"]],
                               coffee_col$pal1[["dark_slate_grey"]])) + 
  theme_minimal3() + 
  labs(
    title = "<span style='color:#154a25;'>Coffee</span> <span style='color:#0E0504;'>and</span> <span style='color:#929396;'>non-Coffee</span> <span style='color:#0E0504;'>Producing Countries</span>"
  )

# Plot rations
p2 <- tuesdata$coffee_ratings %>%
  regex_full_join(cnt, by = c(country_of_origin = "regex")) %>% 
  mutate(cnt = ifelse(is.na(cnt), country_of_origin, cnt)) %>%
  filter(!cnt %in% removes) %>%
  group_by(cnt) %>%
  summarise(t_test = list(t.test(total_cup_points))) %>%
  mutate(tidied = map(t_test, tidy)) %>%
  unnest(tidied) %>%
  mutate(cnt = fct_reorder(cnt, estimate)) %>%
  ggplot(aes(y = estimate, x = cnt)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 size = 1.5, color = coffee_col$pal1[["mountain_mist"]]) +
  geom_bean(size = .025) +
  coord_flip() +
  theme_minimal2()+
  labs(
    title = "Top Ranked Countries",
    caption = "With 95% Confidence Intervals. Country needs 3 ratings to qualify.",
    x = " ",
    y = "Total Cup points"
  ) 

# Concatenate plots
p1 + p2
ggsave(filename = here("img","week28_plot1.png"),width = 14,height = 10, dpi = 300)
