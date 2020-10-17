library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(rvest)
library(purrr)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load(2020, week = 41)
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()
# taken from https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Arial Narrow"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

data <- tuesdata$tournament %>%
  filter(str_detect(school, "UConn")|str_detect(school, "Tennessee$"))

url <- "https://en.wikipedia.org/wiki/Tennessee–UConn_women%27s_basketball_rivalry"
table_nodes = c('//*[@id="mw-content-text"]/div[1]/table[4]/tbody/tr/td/table[1]','//*[@id="mw-content-text"]/div[1]/table[4]/tbody/tr/td/table[2]')

rivals1 <- read_html(url) %>%
  html_nodes(xpath = table_nodes[[1]]
             ) %>%
  html_table(fill=TRUE)

rivals2 <- read_html(url) %>%
  html_nodes(xpath = table_nodes[[2]]
  ) %>%
  html_table(fill=TRUE)

rivals <- bind_rows(rivals1[[1]],
                    rivals2[[1]] %>%
                      `[`(,1:5) %>%
                      slice(-12) %>%
                      mutate(No. = as.integer(No.))
) %>%
  mutate(Date = as.Date(Date,"%b %d, %Y"),
         year = year(Date),
         Winner = case_when(
           Winner == 'Connecticut' ~ 'UConn',
           TRUE ~ 'Tennessee'
         )) %>%
  tidyr::separate(Score, into = c("win_score", "lose_score")) %>%
  mutate(across(win_score:lose_score, as.numeric)) %>%
  mutate(cumulative = win_score - lose_score,
         cumulative = ifelse(Winner == 'Tennessee', cumulative*-1,cumulative)
  ) %>%
  arrange(Date)

greatest_rivalry <- right_join(data, rivals, by = 'year') %>% 
  tidyr::separate(Score, into = c("win_score", "lose_score")) %>%
  mutate(across(win_score:lose_score, as.numeric)) %>%
  mutate(cumulative = win_score - lose_score,
         cumulative = ifelse(Winner == 'Tennessee', cumulative*-1,cumulative)
  ) %>%
  filter(year != 2020)

annotate <- tibble(x = as.Date("2020-01-23"),
                   y = 15,
                   label = "The rivalry resumes\nafter over a decade.\nUconn wins 60-45")
)

sc <- scale_colour_gradientn(colours = c("#000e2f","#f77f00"), limits=c(0, 1))
library(ggtext)
library(ggforce)

p1 <- rivals %>%
  mutate(
    year = year(Date),
    month = month(Date,label = TRUE)
  ) %>%
  ggplot(aes(x = Date, y = cumulative), color = Winner) +
  geom_step(aes(color = as.integer(cumulative < 0)) ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = 'none',
    plot.subtitle = element_markdown()
  ) +
  sc +
  labs(
    title = "The Greatest Rivalry",
    subtitle = '<span style="color:#000e2f">UConn</span> vs <span style="color:#f77f00">Tennessee</span> Winning Margin',
    y = ""
  ) +
  annotate(geom = "text", x = as.Date("1993-08-01"), y = 20, color = "#000e2f",
           label = "UConn", size = 4, angle = 90, family = "Arial Narrow") +
  annotate(geom = "text", x = as.Date("1993-08-01"),y = -15, color = "#f77f00",
           label = "Tennessee", size = 4, angle = 90, family = "Arial Narrow") +
  coord_cartesian(xlim = c(as.Date("1995-01-16"), as.Date("2020-01-23")),
                  expand = FALSE, clip = "off") +
  scale_y_continuous(labels = c("20","10","0","10","20"), breaks = seq(-20,20,10)) +
  geom_mark_circle(data = annotate,aes(x, y, label = label),
                   label.family = "Arial Narrow",
                   label.fontsize = 8,
                   label.fill = "transparent",
                   label.buffer = unit(5, "mm")) +
  geom_linerange(data = rivals,
                 aes(ymin = ifelse(Winner == 'UConn',25,-25 ),
                     ymax = ifelse(Winner == 'UConn',27,-27 ),
                     color = as.integer(cumulative < 0)
                     )
                 )
library(gt)

rivals %>%
  mutate(
    Score =glue("{win_score} - {lose_score}")
  ) %>%
  select(-No.,-year,-cumulative, -win_score,-lose_score) %>%
  gt() %>%
  gt_theme_538() %>%
  data_color(
    columns = vars(Winner),
    colors = c("#f77f00","#000e2f"),
    alpha = .8
  ) %>%
  tab_source_note(
    source_note = "Source: Wikipedia"
  ) %>%
  gtsave("week41_table.png", "img")
  
library(cowplot)
library(magick)
library(patchwork)
tmp <- image_read(here("img", "week41_table.png"))
df <- data.frame()
p2<- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) + theme_void()
p3 <- ggdraw(p2) +      
  draw_image(
    tmp)

p1 + p3
ggsave(here("img", "week41.png"),width = 12, height = 6, dpi=300)


