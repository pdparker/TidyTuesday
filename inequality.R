library(tidyverse)
library(scales)
library(glue)
library(ggrepel)
library(gganimate)
library(ggtext)
library(here)
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

wealth <- tribble(
  ~text, ~year, ~value,~decile,
  "",2000,-20,1, 
  "",2000,-20,2,
  "",2000,-20,3,
  "",2000,-20,4,
  "",2000,-20,5,
  "",2000,-20,6,
  "",2000,-20,7,
  "",2000,-20,8,
  "",2000,-20,9,
  "",2000,-20,10,
  "I",2007, -20,1, 
  "n",2007,1,2,
  "e",2007,7,3,
  "q",2007,28,4,
  "u",2007,84,5,
  "a",2007,158,6,
  "l",2007,233,7,
  "i",2007,336,8,
  "t",2007,508,9,
  "y",2007,1174,10,
  "I",2017,-22,1,
  "n",2017,2,2,
  "e",2017,14,3,
  "q",2017,44,4,
  "u",2017,117,5,
  "a",2017,231,6,
  "l",2017,366,7,
  "i",2017,548,8,
  "t",2017,847,9,
  "y",2017,2014,10
) %>%
  mutate(decile = factor(decile),
         description = glue("${format(value, big.mark = ',')}k") ) 

design <- wealth %>%
  filter(year != 2000) %>%
  group_split(year) %>%
  map_dfc(magrittr::extract, "description") %>%
  set_names(c("a", "b"))  %>%
  mutate(label = glue("<span style='color:grey'>{a}</span><br>--<br>{b}"))

p1 <- wealth %>%
  filter(year != 2000) %>%
  ggplot(aes(x = decile, y = value, label = text,
             group = year)) +
  geom_text(family = "Georgia", size = 18, 
            aes(alpha = year), vjust=0) +
  theme_void() + 
  expand_limits(y = c(-100, 3000)) + 
  theme(legend.position = "none",
        plot.title = element_markdown(vjust = 0, family = "Georgia", size = 14),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_markdown(family = "Georgia",  size = 10),
        axis.text.x = element_markdown(family = "Georgia",  size = 10)) +
  scale_x_discrete(labels = design$label) + 
  labs(title = "Australian wealth inequality by decile (<span style='color:grey'>2007</span>-2017)",
       subtitle = "Average net wealth",
       caption = "Source: Roy Morgan") +
  geom_hline(yintercept = -22) + 
  transition_states(
    year,
    transition_length = 1,
    state_length = 1
  ) + 
  enter_fly(y_loc = -20) 

anim_save(here("img", "inequality.gif"),animate(p1))
