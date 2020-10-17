library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)

# extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
# extrafont::fonts()

z = function(x) (x - mean(x))/sd(x)

tuesdata <- tidytuesdayR::tt_load(2020, week = 42)

d <- tuesdata$datasaurus %>%
  filter( dataset %in% c('dino', 'star')) %>% 
  mutate(id = rep(1:142, 2)) %>%
  pivot_wider(id_cols = id, names_from = dataset, values_from = x:y) %>%
  mutate(across(where(is.numeric), z))

cs <- colorRampPalette(c("#FF1493","#39ff14"))
pal <- cs(11)
background = '#36454f'
for (i in seq(0,10,1)) {
  a = i/10; b = 1 - i/10
  p = ggplot(d, aes(x = a*x_star + b*x_dino, y = a*y_star + b*y_dino)) +
    geom_point(color = pal[i+1]) +
    theme_void() +
    theme(
      panel.background = element_rect(color = background, fill = background),
      plot.background = element_rect(color = background, fill = background)
    )
  
 ggsave(glue("{tempdir()}/fig{i}.png")) 
 
 a = 1- i/10; b = i/10
 p = ggplot(d, aes(x = a*x_star + b*x_dino, y = a*y_star + b*y_dino)) +
   geom_point(color = pal[12-i]) +
   theme_void() +
   theme(
     panel.background = element_rect(color = background, fill = background),
     plot.background = element_rect(color = background, fill = background)
   )
 ggsave(glue("{tempdir()}/fig{10+i}.png")) 
 
}

png_files <- glue("{tempdir()}/fig{1:20}.png")
gif_file <- gifski::gifski(png_files,
                           gif_file = here::here("img/week42.gif"),
                           delay = .5,
                           loop = TRUE)
unlink(png_files)


