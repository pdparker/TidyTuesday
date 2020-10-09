# Stock packages
library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(refinr)
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

tuesdata <- tidytuesdayR::tt_load(2020,week = 39)

data <- full_join(tuesdata$peaks, tuesdata$members)

randomly <- function(x) sample(xtfrm(x))

set.seed(123)
d <- data %>%
  mutate(peak_name = str_remove(peak_name, " [IV].*")) %>%
  group_by(peak_name) %>%
  summarise(height_high = 0,
            height_middle = mean(height_metres, na.rm=TRUE),
            height_low = 0,
            deaths = sum(died, na.rm=TRUE)) %>%
  filter(deaths > 10) %>%
  #mutate(peak_name = fct_reorder(peak_name,deaths)) %>%
  mutate(peak_name = fct_shuffle(peak_name)) %>%
  arrange(peak_name) %>%
  mutate(deaths_middle = cumsum(deaths),
         deaths_low = deaths_middle - deaths/2,
         deaths_high = deaths_middle + deaths/2) %>%
  select(-deaths) %>%
  pivot_longer(cols = height_high:deaths_high) %>%
  tidyr::separate(name, c("stat", "location")) %>%
  relocate(location, .before = stat) %>%
  pivot_wider(names_from = stat, values_from = value,
              id_cols = peak_name:location) %>%
  mutate(location = fct_relevel(location, c("low", "middle", "high"))) %>%
  arrange(peak_name, location) 

d %>%
  ggplot() +
    geom_polygon(aes(x = deaths, y = height, group = peak_name,
                     fill = peak_name, color = peak_name), alpha = .2) +
  geom_text_repel(data = d %>% filter(location == "middle"),
                  aes(x = deaths, y = height, label = peak_name),size =3,
                  color = "grey90", segment.color = 'transparent') +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "grey90", family = "Sinkin Sans 300 Light"),
    axis.title.y = element_text(color = "grey90", family = "Sinkin Sans 300 Light", 
                                angle = 90, vjust = 0, hjust = 1),
    axis.title.x = element_text(color = "grey90", family = "Sinkin Sans 300 Light", hjust = 1),
    panel.background = element_rect(fill = "#43464B", color = "#43464B"),
    plot.background = element_rect(fill = "#43464B",color = "#43464B"),
    plot.title = element_text(color = "grey90", family = "Sinkin Sans 800 Black"),
    plot.subtitle = element_text(color = "grey90", family = "Sinkin Sans 700 Bold"),
    plot.caption = element_text(color = "grey90", family = "Sinkin Sans 300 Light"),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  ) +
  labs(
    x = "Width of Base: Deaths",
    y = "Summit (meters)",
    title = "Himalayan Climbing Expedition Deaths",
    subtitle = "Peaks with at least 10 deaths",
    caption = "@PhilParker_IPPE, Data: Alex Cookson"
  )
ggsave(filename = here("img", "week39.png"), width = 10, height=6, dpi = 300)

