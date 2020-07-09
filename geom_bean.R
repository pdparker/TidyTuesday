# Color Palettes ####
 
#dark_slate_grey="#154a25"
coffee_col <- list(
  pal1 = c(nebula="#c8d8db",dark_slate_grey="#354f52",mountain_mist="#929396",putty="#cca46f"),
  pal2 = c(driftwood="#ad8253",pavalova="#d6c297",matallic_bronze="#554a3f",bull_shot="#845118"),
  pal3 = c(brown_derby="#4d2e12",schooner="#908679",monsoon="#7d7879",mckenzie="#875f35")
)

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text(1:n, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}



# Geom Bean ####
library(ggplot2)
library(grid)
library(EBImage)
#img <- readImage(here::here("img", "bean.png"))

read_img <- function(file, color = NULL){
  img <- image_read(file)
  if(!is.null(color)){
    img <- image_fill(img, color,point = "+100+100", fuzz = 100)
  }
}

img <- read_img(here::here("img", "bean.png"), color = coffee_col$pal1[["dark_slate_grey"]])




beanlogoGrob <- function(x, y, size, img) {
  rasterGrob(x = x, y = y, image = img, default.units = "native", height = size, 
             width = size)
}

Geombean <- ggproto("Geombean", Geom, draw_panel = function(data, panel_scales, 
                                                              coord, img, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_bean", beanlogoGrob(coords$x, coords$y, coords$size, 
                                           img))
}, non_missing_aes = c("size"), required_aes = c("x", "y"), default_aes = aes(size = 0.04), 
icon = function(.) {
}, desc_params = list(), seealso = list(geom_point = GeomPoint$desc), 
examples = function(.) {
})

geom_bean <- function(mapping = NULL, data = NULL, stat = "identity", 
                       position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
                       ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = Geombean, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, img = img, ...))
}

# ggplot(mtcars, aes(wt, mpg))+geom_bean(size = .02)


# Themes ####
theme_minimal2 <- function() {theme_minimal() %+replace%
  theme(text = element_text(family = "Ruluko"),
        plot.title = element_markdown(family = "Ruluko", size = 20,face = 'bold'),
        panel.background = element_rect(fill = coffee_col$pal1[["nebula"]], color = NA),
        plot.background = element_rect(fill = coffee_col$pal1[["nebula"]], color = NA),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")
  )
}

theme_minimal3 <- function() {theme_void() %+replace%
    theme(text = element_text(family = "Ruluko"),
          plot.title = element_markdown(family = "Ruluko", size = 20,face = 'bold'),
          panel.background = element_rect(fill = coffee_col$pal1[["nebula"]], color = NA),
          plot.background = element_rect(fill = coffee_col$pal1[["nebula"]], color = NA),
          legend.position = "none"
    )
}
