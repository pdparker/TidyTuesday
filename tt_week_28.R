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
library(ggforce)
library(ggdist)
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
ggsave(filename = here("img","week28_plot1.png"),width = 18,height = 10, dpi = 300)

ns <- names(tuesdata$coffee_ratings)[21:29] %>%
  str_replace(.,"_", "") %>%
  str_to_sentence()

cnts <- tuesdata$coffee_ratings %>%
  filter(!is.na(variety)) %>%
  mutate(variety = fct_lump(variety, 9)) %>%
  group_by(variety) %>%
  count

myAng <-
  seq(-40,-360,length.out = 9)

# Spider graphs
z <- function(x) scale(x, scale=FALSE)
p3 <- tuesdata$coffee_ratings %>%
  filter(!is.na(variety)) %>%
  mutate(variety = fct_lump(variety, 9)) %>%
  group_by(variety) %>%
  summarise(across(aroma:sweetness, list(m = mean), .names = "{col}")) %>%
  ungroup %>%
  mutate(across(aroma:sweetness, list(c = z), .names = "{col}")) %>%
  pivot_longer(cols = aroma:sweetness) %>%
  mutate(cont = rep(1:9,9)) %>%
  ggplot(aes(x=cont, y=value, group = variety, color = variety)) +
  geom_polygon(fill=NA) +
  coord_polar() +
  theme_minimal2() +
  scale_x_continuous(breaks=1:9,labels = ns) +
  expand_limits(x = 0) +
  facet_wrap(~variety) + 
  geom_text(
    data    = cnts,
    mapping = aes(x = -Inf, y = -Inf, label = n),
    family = "Sinkin Sans 900 X Black",
    size = 10,
    alpha = .5
  ) +
  labs(
    title = "Flavor Profile",
    caption = "Number of batches tested in the cirle center",
    y = " ",
    x = " "
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks =element_blank(),
    axis.text.y =element_blank(),
    axis.title=element_blank(),
    axis.text.x=element_text(size = 10,
                              angle = myAng)
  )
  
p3

p4 <- tuesdata$coffee_ratings %>%
  filter(!is.na(variety), total_cup_points > 50) %>%
  mutate(variety = fct_lump(variety, 9)) %>%
  ggplot(aes(x = total_cup_points, fill = variety, color = variety)) +
  geom_dots(alpha = .8) + 
  theme_minimal2() +
  coord_flip() +
  labs(
    title = "Total Cup Point Distribution by Variety",
    y = "Density",
    x=" Score"
  ) +
  facet_wrap(~variety) +
  geom_text(
    data = cnts,
    aes(x = 65,
        y = .55,
        label = variety
    ),
    family = "Sinkin Sans 900 X Black",
    alpha = .5,
    size = 6
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )



p3 + p4  

ggsave(filename = here("img","week28_plot2.png"),width = 16,height = 10, dpi = 300)

