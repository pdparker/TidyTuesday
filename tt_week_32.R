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
  
# Simple slope plot ####  
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

# Plot 1 ####
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

# Plot 2 ####
ann_text_1 <- data.frame(country = c("AL","AL",'AL'),
                         value = c(-4,-4,30), year = c(2016, 2018, 2017),
                         lab = c("2016", "2018", "Avg. clean energy"),
                         type = c('Nuclear','Nuclear','Nuclear'),
                         col = c('black','black','grey')) %>%
  mutate(country = factor(country, unique(tuesdata$energy_types$country)))

ann_text_2 <- data.frame(country =rep('SI',3),
                         value = c(15, 50, 85),
                         year = rep(2017,3),
                         type = c('Conventional thermal', 'Nuclear', 'Renewable')
                         ) %>%
  mutate(country = factor(country, unique(tuesdata$energy_types$country)))


tuesdata$energy_types %>% 
  filter(level != 'Level 2') %>%
  mutate(country_name = case_when(
    country == 'EL' ~ 'Greece',
    TRUE ~ country_name
  )) %>%
  mutate(type = case_when(
    type == 'Conventional thermal' ~ 'Conventional thermal',
    type == 'Nuclear' ~ 'Nuclear',
    TRUE ~ 'Renewable'
  )) %>%
  group_by(country, type) %>%
  summarise(across(`2016`:`2018`, sum)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(across(`2016`:`2018`, list(per = per),.names = "{fn}_{col}" ) ) %>%
  ungroup() %>%
  select(-`2016`:-`2018`) %>%
  pivot_longer(
    cols = per_2016:per_2018
  ) %>%
  tidyr::separate(name, into = c("stat", "year"), convert=TRUE) %>% 
  mutate(tmp = ifelse(type == 'Conventional thermal',NA,value)) %>% 
  group_by(year) %>%
  mutate(avg = mean(tmp, na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(year, country) %>%
  mutate(clean = sum(tmp, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(clean = case_when(
    year != 2018 ~ NA_real_,
    TRUE ~ clean
  ),
  country = fct_reorder(country, clean, .desc = TRUE,.fun = max)
  ) %>% 
  ggplot(aes(x = year, y = value, fill = type, group = type)) +
  geom_area() +
  geom_line(aes(y = avg, x = year), size = .1, color = 'dark grey') +
  geom_point(aes(x = 2018, y = clean), color = 'red') +
  geom_text(aes(x = 2018, y = clean, label = round(clean)),
            hjust = 0, nudge_x = .1, size = 4) +
  facet_wrap(~country) +
  scale_fill_manual(values = wp_col) +
  theme_void() +
  expand_limits(y = -1.5, x = c(2015.7,2018.3) ) + 
  geom_text(data = ann_text_1, aes(x = year, y = value, label = lab, col = col),
            size = 4) +
  scale_color_manual(values = c('black','black','grey')) +
  geom_text(data = ann_text_2, aes(x = year, y = value, label = type),
            size = 4) +
  theme(legend.position = 'none',
        plot.title = element_text(family = 'Montserrat', face = 'bold', size = 20),
        plot.subtitle = element_text(family = 'Montserrat', size = 20),
        strip.text = element_text(family = 'Montserrat', hjust = 0.15,
                                  face= 'bold', size = 12),
        plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(
    title = 'Change in fuel source for electricity generation',
    subtitle = 'Countries share of clean energy from 2016-2018\n'
  ) +
  geom_segment(x=2016, xend=2018, y=1, yend=1, color='black')
ggsave(here("img","week32_2.png"), width=18,height=12, dpi=300) 


