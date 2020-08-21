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
library(janitor)
library(ggridges)
library(grid)
#Load fonts
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

# Pallet
pal1 <- c('#6d053f', '#9f186a', '#6cb9b7', '#4898a1', '#037b8e', '#086069')
# Load data
tuesdata <- tidytuesdayR::tt_load(2020,week = 34)
# Take guess at last year seen
extinct <- tuesdata$plants %>%
  mutate(year = str_sub(year_last_seen, -4) %>% as.numeric)
# Count by Continent, Country, and Group
extinct_sum <- extinct %>%
  group_by(continent, country, group) %>%
  summarise(n = n()) 
# Sunburst Plot ####
# Inner Circle Data
continent_circle <- extinct_sum %>%
  group_by(continent) %>%
  summarise(tot_cont = sum(n)) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum( tot_cont ),
    ymin = lag( ymax, n = 1, default = 0),
    mid = (ymin + ymax)/2,
    id = row_number(),
    angle = 90 - 360 * (id - 0.5) / n()
  )
#Outer Circle Data
country_circle <- extinct_sum %>%
  group_by(continent, country) %>%
  summarise(tot_cnt = sum(n)) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum( tot_cnt ),
    ymin = lag( ymax, n = 1, default = 0 ),
    mid = (ymin + ymax)/2,
         id = row_number(),
         angle = 90 - 360 * (id - 0.5) / n()
    )
# Inner Circle
continent_circle_plot <- geom_rect(
  data = continent_circle,
  aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = continent),
  color = "white") 
 # Outer  Circle 
country_circle_plot <- ggplot() +
  geom_rect(
    data = country_circle,
    aes( xmin = 4, xmax = 5, ymin = ymin, ymax = ymax, fill = continent),
    color = "white",
    show.legend = FALSE)
# Sunburst plot creation
p2 <- country_circle_plot +
  continent_circle_plot +
  geom_text(
    data = country_circle,
    aes(x = 5.2, y = mid, label = country),
    hjust = 0,size=2,
    angle = country_circle$angle,
    family = 'Merriweather'
  ) +
  geom_text(
    data = continent_circle,
    aes(x = 3.5, y = mid, label = tot_cont),
    hjust = 0.5,size=2, family = 'Merriweather',
    color = 'white'
  ) +
  geom_text(
    data = country_circle %>%
      filter(tot_cnt > 15),
    aes(x = 4.5, y = mid, label = tot_cnt),
    hjust = 0.5,size=2, family = 'Merriweather',
    color = 'white'
  ) +
  coord_polar( theta = "y" ) +
  xlim( 0, 6 ) + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(family = 'Merriweather Black',
                                  hjust = 0.1, size = 15, vjust = 0.7),
        text = element_text(family = 'Merriweather')) +
  annotate('text', 
           x = 0, y = 250,
           label = "500 Extinct Species\nMostly in Africa",
           family = 'Merriweather') +
  scale_fill_manual(values = pal1) +
  labs(fill = '',
       subtitle = 'Geography of Extinction')
# Area Plot ####
p1 <- extinct %>%
  arrange(year) %>%
  group_by(continent, year) %>%
  count() %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(tot = cumsum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=tot, fill=continent)) +
  geom_area(color='white') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text = element_text(family = 'Merriweather'),
        plot.subtitle = element_text(family = 'Merriweather Black',
                                  hjust = 0.1, size = 15, vjust = 0.7)) +
  scale_fill_manual(values = pal1) +
  labs(fill = '',
       subtitle = 'Timeline of Extinction') +
  geom_textbox(x=1910,y=435, hjust=0,
               maxwidth = 75,
               label = 
                  'Cumulative plant extinctions in
               in <span style="color:#6d053f" style="font-style:bold">Africa</span>,
               <span style="color:#9f186a" style="font-style:bold">Asia</span>,
               <span style="color:#6cb9b7" style="font-style:bold">Europe</span>,
               <span style="color:#4898a1" style="font-style:bold">North America</span>,
               <span style="color:#037b8e" style="font-style:bold">Oceania</span>, &
               <span style="color:#086069" style="font-style:bold">South America</span>
               ', size = 3,
                fill = NA, box.size = NA, family = 'Merriweather',
                label.padding = grid::unit(rep(0, 4), "pt"))
# Plot combination ####
p1 + p2 +
  plot_annotation(title = "Plants in Distress\n",
                  caption = '@philparker_IPPE | Week 34') +
  plot_layout(guides = "collect",
              design = 
              'AABB
              AABB
              AABB
              AABB
              AABB') &
  theme(legend.position = 'none',
        plot.title = element_text(family = 'Merriweather Black',
                                  hjust = 0, size = 20),
        plot.caption = element_text(family = 'Merriweather',
                                    hjust = 1, size = 10))

ggsave(here('img', 'week34_1.png'), dpi = 300, width = 14, height = 6)

