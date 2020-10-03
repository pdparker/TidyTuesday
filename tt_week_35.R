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
library(tidytext)
library(ggraph)
library(tidygraph)
library(widyr)
#Load fonts
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

# Load data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
# Extract and convert the ingredients: appetizer
# Pivot longer would have been better
appetizer <- chopped %>%
  select(id = season_episode,appetizer) %>%
  unnest_tokens(word,appetizer,token = "regex",  pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))
#Extract and convert the ingredients: entree
entree <- chopped %>%
  select(id = season_episode,entree) %>%
  unnest_tokens(word,entree,token = "regex", pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))
#Extract and convert the ingredients: desert
dessert <- chopped %>%
  select(id = season_episode,dessert) %>%
  unnest_tokens(word,dessert,token = "regex",  pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))
#Combine and fine pairwise comparisons
words <- bind_rows(appetizer,entree,dessert) %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  filter(n > 5) %>%
  igraph::graph_from_data_frame() 
# Plot inspired by https://datavizproject.com/data-type/arc-diagram/
p1 <- ggraph(words, layout = 'linear') + 
  geom_edge_arc(aes(color=as.factor(n), width=n), alpha=.2) +
  theme_void() +
  scale_color_manual(values = rainbow(5)) +
  geom_node_text(aes(label = name), vjust = 1,
                 hjust = 1, angle = 45, size = 3,
                 color = 'white', family = "Montserrat", alpha = .8) +
  scale_edge_width(range = c(.5,1.5))+
  geom_textbox(label = "kumquats and asparagus were the most common co-occurance (n = 10)", 
                aes(x = -1.5, y = 20), hjust = 0,maxwidth = 10, color = 'white',
               fill = NA, box.size = NA, family = 'Montserrat', size = 3) +
  labs(title = "Creativity Gone Stale?",
       subtitle = "Co-occurance of ingedients in a season of Chopped (minimum of 5 co-occurances)",
       caption = '@philparker_IPPE | Week 35') + 
  theme(legend.position = 'none',
        plot.background = element_rect(fill='#1c2733', color = '#1c2733'),
        panel.background = element_rect(fill='#1c2733', color = '#1c2733'),
        plot.title = element_text(color = "white", family = "Bebas Neue", size = 24, hjust = 0.065),
        plot.subtitle = element_text(color = "white", family = "Bebas Neue", size = 14, hjust = 0.095),
        plot.caption = element_text(color = "white", family = "Bebas Neue", size = 10, hjust = 0.95, vjust = -.5),
        plot.margin=unit(c(0.5,0.5,1,0),"cm"))

ggsave(plot = p1, filename = here('img', 'week35_1.png'), dpi = 300, width = 12, height = 6)




