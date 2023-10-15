library(dplyr)
library(tidyr)
library(h3forr)
library(tmap)
library(sf)
library(maptiles)
library(stringr)

proj_folder <- "~/Documents/code/30days2023"
data_folder <- str_glue("{proj_folder}/data")
this_map <- str_glue("{proj_folder}/09-hexes")
name <- "09-hexes"
setwd(this_map)

square <- c(1.74e6 + 1e4 * c(0, 0, 1, 1, 0), 
            5.42e6 + 1e4 * c(0, 1, 1, 0, 0)) %>%
  matrix(ncol = 2) %>%
  list() %>%
  st_polygon() %>%
  st_sfc() %>%
  st_sf(crs = 2193) %>%
  st_transform(4326)


get_hexes <- function(poly, resolution, distance) {
  poly %>% 
    st_buffer(distance) %>%
    polyfill(res = resolution) %>% 
    h3_to_geo_boundary() %>% 
    geo_boundary_to_sf()
}

h3_5 <- get_hexes(square, 5, 5000)
h3_6 <- get_hexes(square, 6, 2500)
h3_7 <- get_hexes(square, 7, 1500)
h3_8 <- get_hexes(square, 8, 1000)
h3_9 <- get_hexes(square, 9, 750)
h3_10 <- get_hexes(square, 10, 500)
h3_11 <- get_hexes(square, 11, 350)

tmap_mode("plot")

basemap <- get_tiles(square, zoom = 12, provider = "CartoDB.Positron")

map <- tm_shape(basemap, bbox = square) + 
  tm_rgb(tm_mv("red", "green", "blue")) +
  tm_shape(h3_5) + tm_borders(lwd = 3.5) +
  tm_shape(h3_6) + tm_borders(lwd = 2.5) +
  tm_shape(h3_7) + tm_borders(lwd = 1.5) +
  tm_shape(h3_8) + tm_borders(lwd = 1) +
  tm_shape(h3_9) + tm_borders(lwd = 0.7) +
  tm_shape(h3_10) + tm_borders(lwd = 0.5) +
  tm_shape(h3_11) + tm_borders(lwd = 0.35) +
  tm_credits(
    get_credit("CartoDB.Positron"), 
    position = tm_pos_out(pos.h = "RIGHT", pos.v = "TOP",
                          cell.h = "center", cell.v = "bottom")) + 
  tm_layout(frame = FALSE)
  
map
  
tmap_save(map, str_glue("{this_map}/{name}.png"))
