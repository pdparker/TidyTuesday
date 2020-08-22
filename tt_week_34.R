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

# Pallet
pal1 <- c('#6d053f', '#9f186a', '#6cb9b7', '#4898a1', '#037b8e', '#086069')
# Load data
tuesdata <- tidytuesdayR::tt_load(2020,week = 34)
# Take guess at last year seen
extinct <- tuesdata$plants %>%
  mutate(year = str_sub(year_last_seen, -4) %>% as.numeric)
# Count by Continent, Country, and Group
extinct_sum <- extinct %>%
  group_by(continent, country, group) %>%
  summarise(n = n()) 
# Sunburst Plot ####
# Inner Circle Data
continent_circle <- extinct_sum %>%
  group_by(continent) %>%
  summarise(tot_cont = sum(n)) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum( tot_cont ),
    ymin = lag( ymax, n = 1, default = 0),
    mid = (ymin + ymax)/2,
    id = row_number(),
    angle = 90 - 360 * (id - 0.5) / n()
  )
#Outer Circle Data
country_circle <- extinct_sum %>%
  group_by(continent, country) %>%
  summarise(tot_cnt = sum(n)) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum( tot_cnt ),
    ymin = lag( ymax, n = 1, default = 0 ),
    mid = (ymin + ymax)/2,
         id = row_number(),
         angle = 90 - 360 * (id - 0.5) / n()
    )
# Inner Circle
continent_circle_plot <- geom_rect(
  data = continent_circle,
  aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = continent),
  color = "white") 
 # Outer  Circle 
country_circle_plot <- ggplot() +
  geom_rect(
    data = country_circle,
    aes( xmin = 4, xmax = 5, ymin = ymin, ymax = ymax, fill = continent),
    color = "white",
    show.legend = FALSE)
# Sunburst plot creation
p2 <- country_circle_plot +
  continent_circle_plot +
  geom_text(
    data = country_circle,
    aes(x = 5.2, y = mid, label = country),
    hjust = 0,size=2,
    angle = country_circle$angle,
    family = 'Merriweather'
  ) +
  geom_text(
    data = continent_circle,
    aes(x = 3.5, y = mid, label = tot_cont),
    hjust = 0.5,size=2, family = 'Merriweather',
    color = 'white'
  ) +
  geom_text(
    data = country_circle %>%
      filter(tot_cnt > 15),
    aes(x = 4.5, y = mid, label = tot_cnt),
    hjust = 0.5,size=2, family = 'Merriweather',
    color = 'white'
  ) +
  coord_polar( theta = "y" ) +
  xlim( 0, 6 ) + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(family = 'Merriweather Black',
                                  hjust = 0.1, size = 15, vjust = 0.7),
        text = element_text(family = 'Merriweather')) +
  annotate('text', 
           x = 0, y = 250,
           label = "500 Extinct Species\nMostly in Africa",
           family = 'Merriweather') +
  scale_fill_manual(values = pal1) +
  labs(fill = '',
       subtitle = 'Geography of Extinction')
# Area Plot ####
p1 <- extinct %>%
  arrange(year) %>%
  group_by(continent, year) %>%
  count() %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(tot = cumsum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=tot, fill=continent)) +
  geom_area(color='white') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text = element_text(family = 'Merriweather'),
        plot.subtitle = element_text(family = 'Merriweather Black',
                                  hjust = 0.1, size = 15, vjust = 0.7)) +
  scale_fill_manual(values = pal1) +
  labs(fill = '',
       subtitle = 'Timeline of Extinction') +
  geom_textbox(x=1910,y=435, hjust=0,
               maxwidth = 75,
               label = 
                  'Cumulative plant extinctions in
               in <span style="color:#6d053f" style="font-style:bold">Africa</span>,
               <span style="color:#9f186a" style="font-style:bold">Asia</span>,
               <span style="color:#6cb9b7" style="font-style:bold">Europe</span>,
               <span style="color:#4898a1" style="font-style:bold">North America</span>,
               <span style="color:#037b8e" style="font-style:bold">Oceania</span>, &
               <span style="color:#086069" style="font-style:bold">South America</span>
               ', size = 3,
                fill = NA, box.size = NA, family = 'Merriweather',
                label.padding = grid::unit(rep(0, 4), "pt"))
# Plot combination ####
p1 + p2 +
  plot_annotation(title = "Plants in Distress\n",
                  caption = '@philparker_IPPE | Week 34') +
  plot_layout(guides = "collect",
              design = 
              'AABB
              AABB
              AABB
              AABB
              AABB') &
  theme(legend.position = 'none',
        plot.title = element_text(family = 'Merriweather Black',
                                  hjust = 0, size = 20),
        plot.caption = element_text(family = 'Merriweather',
                                    hjust = 1, size = 10))

ggsave(here('img', 'week34_1.png'), dpi = 300, width = 14, height = 6)


# Circlize Plot ####
library(circlize) 
library(ComplexHeatmap)
library(countrycode)
extrafont::loadfonts()

col_fun1 = colorRamp2(c(1, 64, 127), c("blue", "white", "red"))

pdf(here('img', 'week34_2.pdf'), family="Merriweather")
circos.par(gap.after = c(2,2,2,2,2,15))
circos.heatmap(mat1, col = col_fun1, split = split,show.sector.labels = TRUE,
               dend.side = "inside",rownames.side = "outside")
circos.track(track.index = 1, panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 6) { # the last sector
    cn = colnames(mat1)
    n = length(cn)
    circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(10, "mm"), 
                1:n - 3, cn, 
                cex = 0.7, facing = "inside")
  }
}, bg.border = NA)
lgd = Legend(title = "N", col_fun = col_fun1, direction = 'horizontal')
grid.draw(lgd)
title('Most Similar Decades: Number of Threats and Actions', adj = 0, cex = 0.8)
circos.clear()
dev.off()


extinct3 <- extinct %>%
  select(continent, country, threat_AA:action_NA) %>%
  pivot_longer(cols = threat_AA:action_NA) %>%
  tidyr::separate(name, into=c('outcome', 'type')) %>%
  select(-type) %>%
  rownames_to_column() %>%
  pivot_wider(id_cols = c(rowname,continent,country),
              names_from = outcome, 
              values_from = value) %>%
  group_by(continent, country) %>%
  summarise(threat = sum(threat,na.rm=TRUE) %>% log10,
            action = sum(action,na.rm=TRUE) %>% log10) %>%
  drop_na() 


mat1 <- as.matrix(extinct3[,3:4])
rownames(mat1) <- countrycode(extinct3$country, origin = 'country.name', 'iso3c')
colnames(mat1) <- c('Actions','Threats')
split <- extinct3$continent


col_fun1 = colorRamp2(c(0, 1, 2.5), c("blue", "white", "red"))

pdf(here('img', 'week34_3.pdf'), family="Merriweather")
circos.par(gap.after = c(2,2,2,2,2,15))
circos.heatmap(mat1, col = col_fun1, split = split,show.sector.labels = TRUE,
               dend.side = "inside",rownames.side = "outside")
circos.track(track.index = 1, panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 6) { # the last sector
    cn = colnames(mat1)
    n = length(cn)
    circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(10, "mm"), 
                1:n-3, cn, 
                cex = 0.7, facing = "inside")
  }
}, bg.border = NA)
text(-0.25,0.3,"Most similar countries:\nNo. of threats and actions",
     cex = .8, adj = 0.1, face = 'italic', lines = -1)
lgd = Legend(title = "N (Log 10)", col_fun = col_fun1,
             direction = 'horizontal',legend_width = unit(40, "mm"))
grid.draw(lgd)
title('Plants in Distress (< 1900 - 2020)', adj = 0, cex = 0.8)
circos.clear()
dev.off()


