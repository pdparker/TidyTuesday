library(ggplot2)
library(grid)
library(EBImage)
img <- readImage(here::here("img", "hoof.png"))
HooflogoGrob <- function(x, y, size, img) {
  rasterGrob(x = x, y = y, image = img, default.units = "native", height = size, 
             width = size)
}

GeomHoof <- ggproto("GeomHoof", Geom, draw_panel = function(data, panel_scales, 
                                                              coord, img, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_hoof", HooflogoGrob(coords$x, coords$y, coords$size, 
                                           img))
}, non_missing_aes = c("size"), required_aes = c("x", "y"), default_aes = aes(size = 0.05), 
icon = function(.) {
}, desc_params = list(), seealso = list(geom_point = GeomPoint$desc), 
examples = function(.) {
})

geom_hoof <- function(mapping = NULL, data = NULL, stat = "identity", 
                       position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
                       ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomHoof, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, img = img, ...))
}


ggplot(mtcars, aes(wt, mpg))+geom_hoof(size = .02)
