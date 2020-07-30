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
library(ggdark)
extrafont::font_import("~/Library/Fonts")
extrafont::fonts()
tuesdata <- tidytuesdayR::tt_load(2020, week = 27)

kamala <- c("#fe5f92", "#683fa8", "#f5ee6b", "#1ba0c8",
            "#66175a","#f7c69b","#09fbc9")

kamala2 <- c("#fe5f92","#fef001", "#6941ac", "#ff280d")

# explore ####
glimpse(tuesdata$comic_bechdel)
table(tuesdata$comic_bechdel$writer)

table(tuesdata$comic_bechdel$series,
      tuesdata$comic_bechdel$pass_bechdel) %>%
  prop.table(margin = 1)

comic <- tuesdata$comic_bechdel %>%
  filter(!is.na(pass_bechdel)) %>%
  tidyr::separate(writer, into = c("Writer 1","Writer 2"), sep = ",") %>%
  mutate(`Writer 1` = str_remove(`Writer 1`,"[[:punct:]]")) %>%
  mutate(`Writer 1` = replace(`Writer 1`, 
                              `Writer 1` == 'Tony  Isabella',
                              'Tony Isabella') ) %>%
  filter(!is.na(`Writer 1`)) %>%
  add_count(`Writer 1`)
  
  
glimpse(comic)

plot_1 <-comic %>%
  filter(n > 2) %>%
  ggplot(aes(x = issue, y = 0, color =`Writer 1`, size = pass_bechdel)) +
  geom_point() +
  facet_wrap(~series, ncol = 1, scales = "free") +
  dark_theme_void() +
  labs(
    title = "    Does the Marvel Comic Issue pass the Bechdel Test?\n",
    color = "Writer*:",
    size = "Pass Bechdel?",
    caption = "*Writers who first authored more than two issues."
  ) +
  theme(text = element_text(family = "Marvel"),
        plot.title =  element_text(family = "Bangers", face = "bold", size = 16),
        strip.text = element_text(family = 'Marvel', size = 14, face = "bold")) +
  scale_color_manual(values = kamala2)

ggsave(plot = plot_1, filename = here::here("img", "week_27_plot_1.png"),width = 8, height = 7)

invert_geom_defaults()

