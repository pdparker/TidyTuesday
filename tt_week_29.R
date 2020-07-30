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
#Specif packages for this week
library(refinr)
library(streamgraph)
library(ggwaffle)
library(waffle)

tetris_pal <- c(bright_turquoise = "#0cefef", blue = "#1100f1",
                california = "#f09f00", titanium_yellow = "#f0f100",
                lime = "#03f000", vivid_violet = "#9f01f1",
                ku_crimson = "#f00401")

print.palette(tetris_pal)
# load data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')

astronauts_occ <- tuesdata$astronauts %>%
  mutate(occupation_clean = key_collision_merge(occupation),
         occupation_clean = fct_lump(occupation_clean, n = 4),
         decade = round(year_of_mission / 5) * 5) %>%
  group_by(decade) %>%
  count(occupation_clean) %>%
  mutate(percent = n/sum(n)*100,
         percent_2 = round(percent / 5) * 5)


astronauts_army <- tuesdata$astronauts %>%
  mutate(occupation_clean = key_collision_merge(occupation),
         occupation_clean = fct_lump(occupation_clean, n = 4),
         decade = round(year_of_mission / 5) * 5) %>%
  group_by(decade) %>%
  count(military_civilian) %>%
  mutate(percent = n/sum(n)*100,
         percent_2 = round(percent / 10) ) 

ast_percent <- scales::label_percent(
  scale = 10,
  suffix = " %"
)

splitstackshape::expandRows(astronauts_army,count = "percent_2") %>%
  ungroup() %>%
  mutate(y = rep(1:10, 13)) %>% 
  ggplot(aes(x=decade, y =y, fill = military_civilian)) +
    geom_waffle() +
    theme_minimal2() +
    labs(title =
            "<span style='color:#0df89b'>Military</span> and <span style='color:#f500f8'>Civilian </span> Astronauts",
         y = "%"
         ) +
  scale_fill_manual(values = c("#f500f8","#0df89b")) +
  scale_y_continuous(labels = ast_percent)

  
  
  theme_minimal2 <- function() {theme_minimal() %+replace%
      theme(text = element_text(family = "Tetris", color = "white"),
            plot.title = element_markdown(family = "Tetris", size = 20,face = 'bold',
                                          color = "white"),
            panel.background = element_rect(fill = "black", color = NA),
            plot.background = element_rect(fill = "black", color = "black"),
            panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                            colour = "black"), 
            panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                            colour = "black"),
            legend.position = "none"
      )
  }  
  
  GeomWaffle <- ggproto("GeomWaffle", GeomTile,
                        
                        default_aes = aes(colour = "black", size = 2, alpha = NA),
                        
                        required_aes = c("x", "y", "fill")
  )
  
  
  astronauts_occ <- tuesdata$astronauts %>%
    mutate(occupation_clean = key_collision_merge(occupation),
           occupation_clean = fct_lump(occupation_clean, n = 6),
           decade = round(year_of_mission / 5) * 5) %>%
    group_by(decade) %>%
    count(occupation_clean) %>%
    mutate(percent = n/sum(n)*100,
           percent_2 = round(percent / 10) ) 
  
  
  splitstackshape::expandRows(astronauts_occ,count = "percent_2") %>%
    ungroup() %>%
    group_by(decade) %>%
    mutate(y = 1:n()) %>% 
    ungroup() %>% #count(occupation_clean)
    ggplot(aes(x=decade, y =y, fill = occupation_clean)) +
    geom_waffle() +
    theme_minimal2() +
    labs(title = "Occupation of Astronauts",
         caption = "5-year Aggregated Percentage",
         y = "%") +
    #scale_fill_manual(values = tetris_pal[1:5]) +
    scale_y_continuous(labels = ast_percent)
  
  
  
  ast_percent <- scales::label_percent(
    scale = 10,
    suffix = " %"
  )
  
astronauts_occ %>%  
  streamgraph(key="occupation_clean", value="n",
              date="year_of_mission")

