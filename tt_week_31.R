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
#Load fonts
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()

tuesdata <- tidytuesdayR::tt_load(2020,week = 31)
skimr::skim(tuesdata$penguins_raw)

d <- tuesdata$penguins_raw %>%
  clean_names()

d %>%
  count(individual_id) %>%
  count(n)

m1 <- brms::brm(body_mass_g ~ 1 + (1|individual_id), d)
summary(m1)

library(brms)
bayes <- posterior_samples(m1)

bayes2 <- bayes %>%
  mutate(across(contains(",Intercept"), ~magrittr::add(.,bayes$b_Intercept))) %>%
  select(-b_Intercept:-sigma, -lp__) %>%
  pivot_longer(cols = everything()) %>%
  tidyr::extract(col=name,into = "individual_id", regex = ".\\[(.+),.+") %>%
  group_by(individual_id) %>%
  filter(value > quantile(value, .025), value < quantile(value, .975)) %>% 
  left_join(.,d %>% select(individual_id, species)) 

color = c("#365854","#7cc4b4","#e3a079")
name = c("Adelie", "Chinstrap", "Gentoo")
div = glue("<span style='color:{color}'>{name}</span>")

penguin <- image_read(here("img", "Penguin.png")) 

plot_box <- tibble(xmin = 3000,
                   xmax = 6000,
                   ymin = 0,
                   ymax = 190,
                   xrange = xmax - xmin,
                   yrange = ymax - ymin)

bayes2 %>%
  group_by(individual_id) %>%
  mutate(med = median(value)) %>%
  ungroup() %>%
  mutate(individual_id = fct_reorder(individual_id, value)) %>%
  ggplot(aes(y = individual_id, x = value, fill = species, height = ..density..)) +
  geom_density_ridges(trim=TRUE, stat = "density") +
  theme_void() +
  theme(legend.position = "none",
        axis.text.y = element_text(family = "Avenir", color = "#3f455a",
                                   face = "bold", size = 14),
        plot.subtitle = element_markdown(family = "Avenir",color = "#3f455a",
                                         size = 18, face='plain'),
        plot.title = element_text(family = "Baskervville",color = "#3f455a",
                                  face = "bold", size = 30)) +
  scale_fill_manual(values = c("#365854","#7cc4b4","#e3a079")) +
  labs(
    title =  "Penguine Body Weight in Grams",
    subtitle = glue("{div[1]}, {div[2]}, and {div[3]}")
  ) + 
  coord_flip(clip = "off") +
  expand_limits(x = 5000) +
  geom_density_ridges(alpha = 0, quantile_lines = TRUE, color = NA,
                      vline_color = "black") +
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.background = element_rect(fill = "#eee5d5", color = NA),
        plot.background = element_rect(fill = "#eee5d5", color = NA)) + 
  annotate("richtext", x = 3400, y = 110, fill = NA, label.color = NA,
           label = "Likely weights in their adult lives.<br>
           95% confidence interval, 1<sup>st</sup>, 2<sup>nd</sup>, and
           3<sup>rd</sup><br> quartile of likely weights",
           size = 5, hjust = 0, family = "Baskervville",color = "#3f455a") +
  annotation_raster(penguin, 
                    ymin = Inf, 
                    ymax = plot_box$ymax - plot_box$yrange*.15, 
                    xmin = -Inf, 
                    xmax = plot_box$xmin + plot_box$xrange *.20
  )  
ggsave(here("img", "penguines.png"), width=15, height=10, dpi=300) 


  