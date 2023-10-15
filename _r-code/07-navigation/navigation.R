library(sf)
library(tmap)
library(dplyr)
library(geosphere)

name <- "07-navigation"
proj_folder <- "~/Documents/code/30days2023"
data_folder <- str_glue("{proj_folder}/data")
this_map <- str_glue("{proj_folder}/{name}")

min_lat <- -41.3
max_lat <- 45
bearing <- -88.5
step_length <- 5e4
last_pt <- c(174.75, min_lat)
lox <- list()
transect <- last_pt
i <- 1
while (last_pt[2] < max_lat) {
  next_pt <- destPointRhumb(last_pt, bearing, step_length)
  if (next_pt[1] > last_pt[1]) {
    lox[[i]] <- transect
    i <- i + 1
    transect <- c(next_pt)
  } else {
    transect <- c(transect, next_pt)
  }
  last_pt <- next_pt
}
lox[[i]] <- transect

lox_sf <- lox %>%
  lapply(unlist) %>%
  lapply(matrix, ncol = 2, byrow = TRUE) %>%
  st_multilinestring() %>%
  st_sfc() %>%
  st_sf(crs = 4326) %>%
  st_cast("LINESTRING") %>%
  st_cast("POINT")

data("World")
world <- World %>%
  st_cast("POLYGON") %>%
  st_transform("+proj=ortho +lat_0=-41.3 +lon_0=174.75") %>%
  dplyr::filter(!(st_is_empty(geometry)))

tm_shape(world, bbox = lox_sf) +
  tm_fill() +
  tm_graticules(x = seq(-180, 180, 15), y = seq(-75, 75, 15),
                labels.show = FALSE, lwd = 0.5, col = "grey") +
  tm_shape(lox_sf) +
  tm_dots(fill = "firebrick", size = 0.001) +
  # tm_lines(col = "firebrick", lwd = .75) +
  tm_title("    Go (just north of) west! See also Day 22") +
  tm_layout(frame = FALSE)

tmap_save(str_glue("{this_map}/{name}.png"))
