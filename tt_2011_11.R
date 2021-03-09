library(tidyverse)
library(tidytuesdayR)
library(ggtern)

tuesdata <- tidytuesdayR::tt_load('2021-03-09')

mov <- tuesdata$movies
glimpse(mov)

mov %>% 
  filter(country == "USA", year > 2000) %>%
  filter(str_detect(genre, "Sci-Fi") ) %>%
  mutate(domgross_2013 = as.numeric(domgross_2013),
         intgross_2013 = as.numeric(intgross_2013)) %>%
  ggtern(aes(budget_2013,domgross_2013,intgross_2013)) +
  geom_point(aes(color = metascore)) +
  geom_crosshair_tern(aes(color = metascore)) +
  facet_wrap(~binary) +
  hrbrthemes::theme_modern_rc() +
  labs(
    x = "Budget",
    y = "US Earnings",
    z = "Int. Earnings",
    title = "Sci-Fi Movies that Pass the Bechdel Test Earn More",
    subtitle = "US Sci-Fi films from 2000",
    color = "Meta Score"
  ) +
  theme(strip.text = element_text(color = "#8e8e93")) 

ggsave(filename = here::here("img","2021_wk11.png"), dpi=300,width = 12,height = 6)  

