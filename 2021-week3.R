library(tidytuesdayR)
library(tidyverse)
library(extrafont)
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

tt <- tidytuesdayR::tt_load('2021-01-12')

tt$artwork %>%
  group_by(artist) %>%
  count(sort = TRUE)

url <- tt$artwork %>% 
  filter(artist == 'Warhol, Andy') %>%
  select(thumbnailUrl, title,id, year) %>%
  drop_na()


library(RImagePalette)
library(jpeg)
library(progress)

pb <- progress_bar$new(
  format = "(:spin) [:bar] :percent",
  total = nrow(url), clear = FALSE, width = 60)

pal <- matrix(NA, ncol=3, nrow=nrow(url))

for(i in 1:nrow(url)){
  pb$tick() 
  download.file(url$thumbnailUrl[i], "img.png", mode = "wb")
  test <- readJPEG("img.png")
  file.remove("img.png")
  pal[i, 1:3] <- image_palette(test, 3, median, TRUE)
  Sys.sleep(.5)
}


pal_walhol <- pal %>%
  as.tibble() %>%
  set_names(glue::glue("color_{1:3}"))


pal_walhol <- bind_cols(url, pal_walhol) %>%
  select(-thumbnailUrl) %>%
  pivot_longer(cols = color_1:color_3)

library(ggwaffle)
library(grid)
library(ggimage)
library(ggdark)
library(colorspace)


pal_walhol <- pal_walhol %>%
  drop_na() %>%
  mutate(id=factor(id),
         soup = "soup.png",
         invert = invert_color(value),
         period = ifelse(year >1968,"Post Attempted Murder", "Pre Attempted Murder")) 

p1 <- pal_walhol %>%
  ggplot(aes(id,name, color = value)) + 
  geom_image(aes(image = soup), size = 0.15) +
  scale_color_manual(values = pal_walhol$value) +
  theme_void() +
  scale_y_discrete(expand=c(0,4) ) +
  labs(
    title = "Andy Walhol at the TATE",
    subtitle = "Three Most Used Colors Per Painting\n",
    caption = "| Tidy Tuesday | @PhilParker_IPPE |"
  ) +
  theme(legend.position = "none",
        plot.title = element_text(family = "BadaBoom BB",hjust = 0,vjust=.3),
        plot.subtitle = element_text(family = "Montserrat-SemiBold",hjust = 0,vjust=.3),
        plot.caption = element_text(family = "Montserrat-Light"),
        plot.tag.position = 'bottomleft',
        plot.tag = element_text(family = "Montserrat-Light", vjust = 0, hjust = 1),
        strip.text = element_text(family = "Montserrat-Light", vjust = 0, hjust = 0),
        #axis.text.x = element_text(family = "Montserrat-Light", angle = 45),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "cm")
        ) +
  facet_wrap(~period,ncol = 1, scales = "free")

ggsave(filename = "~/Dropbox/TMP/soup_walhol.png", plot = p1,
       width = 4, height = 6, dpi = 300)

