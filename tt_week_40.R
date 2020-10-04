library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(tidytext)
library(tidylog)
library(tidylo)
library(janitor)
library(scales)
library(refinr)
library(patchwork)
library(ggrepel)
library(ggtext)
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

pal <- c("#daeaed", "#efdfe8", "#faebc7",
         "#f9e6e5", "#dceee7")

tuesdata <- tidytuesdayR::tt_load(2020, week = 40)
# Clean data
queen_bee <- tuesdata$beyonce_lyrics %>%
  select(lyric = line, artist_name) %>%
  mutate(lyric = str_remove(lyric,"^.+\\)\\:"))

taetae <- tuesdata$taylor_swift_lyrics %>%
  clean_names() %>%
  select(lyric = lyrics, artist_name = artist) %>%
  mutate(artist_name = key_collision_merge(artist_name) %>% str_trim)
# Make Tidy
lyrics <- bind_rows(queen_bee, taetae)  %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  count(artist_name, word, sort = TRUE) %>%
  bind_log_odds(artist_name, word, n) %>%
  rename(odds = log_odds_weighted)
# Get only common words
lyrics <- lyrics %>%
  pivot_wider(id_cols = word,
              names_from = artist_name,
              values_from = n:odds) %>%
  drop_na() %>% 
  pivot_longer(
    cols = 2:5,
    names_to = c("stat", "artist"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )
# Right axis words
top_words <- lyrics %>% filter(odds > 3 & n > 10 |odds < -2.5 & n > 10) %>%
  arrange(odds) %>%
  mutate(sequence = seq(-8,8,16/n())[-1])
# Plot
lyrics %>%
  ggplot(aes(n,odds, color = artist, size = abs(odds))) +
  geom_point() +
  scale_x_log10() +
  ggthemes::theme_solarized_2() +
  expand_limits(y = -6) +
  geom_text(data = top_words,
                  aes(2500,sequence,label=word, size = abs(odds))) +
  scale_size_continuous(range = c(0, 4)) +
  theme(
    legend.position = "none",
    text = element_text(color = "#984c74", family = "Sinkin Sans 100 Thin"),
    plot.title = element_text(family = "charlotte", color = "#000040", hjust = 0),
    plot.subtitle = element_text(color = "#000040", hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggrepel::geom_text_repel(data = top_words %>% filter(odds > 4 & n > 100 |odds < -3 & n > 100),
                           aes(n,odds,label = word, color = artist),
                           nudge_x = .1, segment.color = 'transparent'
                           ) +
  labs(
    title = "Unique Word Usage",
    subtitle = "Taylor Swift & Beyoncé Lyrics",
    y = "Weighted Log Odds",
    x = "Word Frequency"
  ) +
  scale_color_manual(values=c("#000040","#cba135")) +
  annotate("text",x = 3,y = 5, label = "Taylor Swift",color = "#cba135",
           family = "charlotte") +
  annotate("text",x = 3,y = -5, label = "Beyoncé",color = "#000040",
           family = "charlotte")

ggsave(filename = here("img", "week40.png"), width = 10, height=8, dpi = 300)
  





