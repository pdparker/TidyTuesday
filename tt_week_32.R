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
library(ggrepel)
library(magrittr)
#Load fonts
extrafont::font_import("~/Library/Fonts",prompt = FALSE)
extrafont::fonts()

wp_col <- c('#f2f2f2','#feeab9','#edbb58')

tuesdata <- tidytuesdayR::tt_load(2020,week = 32)

tuesdata$energy_types %>%
  #skimr::skim()
  #View()
tuesdata$country_totals %>% skimr::skim()
  
  
tuesdata$country_totals %>%
  filter(type == 'Total net production') %>%
  mutate(country_name = case_when(
    country == 'EL' ~ 'Greece',
    TRUE ~ country_name
    )
  ) %>%
  pivot_longer(
    cols = `2016`:`2018`
  ) %>%
  ggplot(aes(x = name, y = value, label = country, group = country_name)) +
  geom_point() +
  geom_line() + 
  scale_y_log10() +
  geom_text_repel() + 
  theme_void()

per <- function(x) {x/sum(x)*100}

data_2016 <- tuesdata$energy_types %>% 
  filter(level != 'Level 2') %>%
  mutate(type = case_when(
    type == 'Conventional thermal' ~ 'Conventional thermal',
    type == 'Nuclear' ~ 'Nuclear',
    TRUE ~ 'Renewable'
  )) %>%
  group_by(country) %>%
  mutate(`2016_per` = per(`2016`)) %>%
  ungroup() %>%
  group_by(country, type) %>%
  filter(`2016` != 0) %>%
  summarise(`2016_per` = sum(`2016_per`)) %>%
  ungroup()

pos_2016 <- data_2016 %>%
  filter(type != 'Conventional thermal') %>%
  group_by(country) %>%
  mutate(top_2016 = cumsum(`2016_per`),
         bottom_2016 = top_2016 - `2016_per`)

neg_2016 <- data_2016 %>%
  filter(type == 'Conventional thermal') %>%
  group_by(country) %>%
  mutate(top_2016 = `2016_per`*-1,
         bottom_2016 = 0)

plot_2016 <- bind_rows(pos_2016, neg_2016) %>%
  mutate(country = factor(country)) 

plot_2016$country <- fct_reorder(plot_2016$country, plot_2016$top_2016, min)
plot_2016$country <- fct_rev(plot_2016$country)

width_2016 <- tuesdata$energy_types %>%
  mutate(country = fct_relevel(country, plot_2016$country %>% levels)) %>%
  arrange(country) %>%
  group_by(country) %>%
  summarise(count_2016 = sum(`2016`)) %>%
  mutate(right_2016 = cumsum(count_2016)/sum(count_2016)*37,
         left_2016 = lag(right_2016, default = 0))

plot_2016 <- plot_2016 %>%
  left_join(.,width_2016) %>% 
  arrange(country)

label_2016 <- pos_2016 %>%
  left_join(width_2016) %>%
  mutate(center_2016 = (left_2016 + right_2016)/2) %>%
  group_by(country) %>%
  filter(top_2016 == max(top_2016)) %>%
  ungroup() %>%
  mutate(top_2016 = top_2016 + 5)

rect_2016 <- tribble(
  ~xmin,~xmax,~ymin,~ymax, ~fill,
  28,30, 105,120, "Conventional thermal",
  28,30, 120,127, "Nuclear",
  28,30, 127,135, "Renewable"
)

low_percent_2016 <- tribble(
  ~y,~x, ~label,
  -99,-2, "100% conventional thermal energy",
  -74,-2, "75%",
  -49,-2, "50%",
  -24,-2, "25%",
  1,-2, "0%")

high_percent_2016 <- tribble(
  ~y,~x, ~label,
  1,38, "0%",
  26,38, "25%",
  51,38, "50%",
  76,38, "75%",
  101,38, "clean electricity 100%",
)

p1 <- plot_2016 %>%
  ggplot(aes(xmin = left_2016,
             xmax = right_2016,
             ymin = bottom_2016,
             ymax = top_2016,
             fill = type)) +
  geom_rect() +
  theme_void() +
  geom_rect(colour = I("black"),size = 0.1) +
  scale_fill_manual(values = wp_col) +
  labs(title = "How European countries generated electricity in 2016") +
  theme(plot.title = element_text(family = "Merriweather Black", size = 30),
        legend.position = 'none',
        plot.margin=unit(c(1,1,1,1),"cm")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(seq(-100,100, 25)), color = 'grey', alpha = .2) +
  geom_text_repel(data=label_2016, aes(y = top_2016, x = center_2016,
                                         label = country),
                direction    = "y",
                hjust        = 0.5,
                vjust        = 1, box.padding = 0,
                segment.color = NA, size = 5, force=.01,
                ylim = c(NA,110), family = 'Montserrat') +
  expand_limits(y = 140)+
  annotate("richtext", x = 2, y = 130, 
  label = "**Norway** had an electricity production<br>
           made up almost entirely of renewable<br>energy.",
           fill = NA, label.color = NA, hjust=0, family = 'Montserrat') +
  annotate(
    geom = "curve", xend = .8, yend = 110, x = 2, y = 130, 
    curvature = .5) +
  annotate("richtext", x = 5, y = -70, 
           label = "**France** is the second largest energy producer<br>
           in Europe but by far the largest nuclear energy.",
           fill = NA, label.color = NA, hjust=0, family = 'Montserrat') +
  annotate(
    geom = "curve", xend = 6, yend = -20, x = 6, y = -60, 
    curvature = 0)  +
  annotate("richtext", x = 25, y = -80, 
           label = "Most of **Poland's** electricity is<br>
           from conventional thermal energy.",
           fill = NA, label.color = NA, hjust=0, family = 'Montserrat') +
  annotate(
    geom = "curve", x = 30, y = -85, xend = 36, yend = -92, 
    curvature = 0.5) +
  annotate("richtext", x = 25.5, y = 70, 
           label = "**Germany** is the largest producing country in<br>
           Europe, and is also  the largest renewable<br>
           and conventional thermal energy producer.",
           fill = NA, label.color = NA, hjust=0, family = 'Montserrat') +
  annotate(
    geom = "curve", xend = 25, yend = 50, x = 25.5, y = 70, 
    curvature = .5) +
  geom_rect(data = rect_2016,aes(xmin=xmin,xmax=xmax,
                                 ymin=ymin,ymax=ymax,
                                 fill=fill)) +
  geom_text(x=29, y=145,label="Total eletric\nproduction\nof a country",
            size = 3, family = "Montserrat", hjust=.5, color = 'grey') +
  geom_segment(x = 28, xend = 30, 
               y = 137, yend = 137,
               colour = "grey") +
  geom_segment(x = 28, xend = 28, 
               y = 136, yend = 137,
               colour = "grey") +
  geom_segment(x = 30, xend = 30, 
               y = 136, yend = 137,
               colour = "grey") +
  geom_text(x=26.5, y=125,label="Clean\nelectricity",
            size = 3, family = "Montserrat", hjust=1, color = 'grey') +
  geom_segment(x = 27, xend = 27, 
               y = 121, yend = 135,
               colour = "grey") +
  geom_segment(x = 27, xend = 27.5, 
               y = 121, yend = 121,
               colour = "grey") +
  geom_segment(x = 27, xend = 27.5, 
               y = 135, yend = 135,
               colour = "grey") +
  geom_text(x=26.5, y=115,label="Conventional\nthermal",
            size = 3, family = "Montserrat", hjust=1, color = 'grey') +
  geom_segment(x = 27, xend = 27, 
               y = 105, yend = 119,
               colour = "grey") +
  geom_segment(x = 27, xend = 27.5, 
               y = 119, yend = 119,
               colour = "grey") +
  geom_segment(x = 27, xend = 27.5, 
               y = 105, yend = 105,
               colour = "grey") +
  geom_text(x=31.2, y=132,label="Renewable",
            size = 3, family = "Montserrat", hjust=0, color = 'grey') +
  geom_segment(x = 31, xend = 31, 
               y = 127, yend = 135,
               colour = "grey") +
  geom_segment(x = 31, xend = 30.5, 
               y = 135, yend = 135,
               colour = "grey") +
  geom_segment(x = 31, xend = 30.5, 
               y = 127, yend = 127,
               colour = "grey") +
  geom_text(x=31.2, y=123,label="Nuclear",
            size = 3, family = "Montserrat", hjust=0, color = 'grey') +
  geom_segment(x = 31, xend = 30.5, 
               y = 123, yend = 123,
               colour = "grey")


p1 + 
  geom_text(x=-1,y=-98.5, label = "100% conventional thermal energy", hjust = 0) +
  geom_text(x=-1,y=-73.5, label = "75%", hjust = 0) +
  geom_text(x=-1,y=-48.5, label = "50%", hjust = 0) +
  geom_text(x=-1,y=-23.5, label = "25%", hjust = 0) +
  geom_text(x=-1,y=1.5, label = "0%", hjust = 0) +
  geom_text(x=38,y=101.5, label = "clean energy 100%", hjust = 1) +
  geom_text(x=38,y=76.5, label = "75%", hjust = 1) +
  geom_text(x=38,y=51.5, label = "50%", hjust = 1) +
  geom_text(x=38,y=26.5, label = "25%", hjust = 1) +
  geom_text(x=38,y=1.5, label = "0%", hjust = 1)
  


ggsave(here("img","week32.png"), width=18,height=12, dpi=300)  


