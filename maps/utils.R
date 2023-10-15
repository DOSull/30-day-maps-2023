library(sf)
library(dplyr)

get_meridian <- function(lon = 0, spacing = 1) {
  st_linestring(matrix(cbind(lon, seq(-90, 90, spacing)), 
                       ncol = 2, byrow = FALSE)) %>%
    st_sfc(crs = 4326)
}

get_meridians <- function(spacing = 15) {
  g <- c()
  for (lon in seq(-180 + spacing / 2, 180 - spacing / 2, spacing)) {
    g <- c(g, get_meridian(lon = lon))
  }
  g
}

get_parallel <- function(lat = 0, spacing = 1) {
  st_linestring(matrix(cbind(seq(-180, 180, spacing), lat), 
                       ncol = 2, byrow = FALSE)) %>%
    st_sfc(crs = 4326)
}

get_parallels <- function(spacing = 15) {
  g <- c()
  for (lat in seq(-90 + spacing / 2, 90 - spacing / 2, spacing)) {
    g <- c(g, get_parallel(lat = lat))
  }
  g
}

graticule <- c(get_meridians(), get_parallels()) %>%
  st_sfc(crs = 4326) 
