library(sf)
library(dplyr)
library(stringr)

get_azimuthal_eq_dist <- function(centre) {
  str_glue("+proj=aeqd +lon_0={centre[1]} +lat_0={centre[2]}")
}

get_ortho_proj <- function(centre) {
  str_glue("+proj=ortho +lat_0={centre[2]} +lon_0={centre[1]}")
}

get_hemisphere <- function(centre = NULL, crs = NULL,
                           radius = 6378137 * pi / 2) {
  aeqd <- str_glue("+proj=aeqd +lon_0={centre[1]} +lat_0={centre[2]}")
  st_point(centre) %>% 
    st_buffer(radius) %>%
    st_sfc(crs = aeqd) %>% 
    st_transform(crs)
}

get_meridian <- function(lon = 0, res = 1) {
  st_linestring(matrix(cbind(lon, seq(-90, 90, res)), 
                       ncol = 2, byrow = FALSE)) %>%
    st_sfc(crs = 4326)
}

get_meridians <- function(spacing = 15) {
  g <- c()
  for (lon in seq(-180, 180 - spacing, spacing)) {
    g <- c(g, get_meridian(lon = lon))
  }
  g
}

get_parallel <- function(lat = 0, res = 1) {
  st_linestring(matrix(cbind(seq(-180, 180, res), lat), 
                       ncol = 2, byrow = FALSE)) %>%
    st_sfc(crs = 4326)
}

get_parallels <- function(spacing = 15) {
  g <- c()
  for (lat in seq(-90 + spacing, 90 - spacing, spacing)) {
    g <- c(g, get_parallel(lat = lat))
  }
  g
}

get_graticule <- function(spacing = 15, centre = NULL, 
                          radius = 6378137 * pi / 2) {
  if (is.null(centre)) {
    c(get_meridians(spacing = spacing), get_parallels(spacing = spacing)) %>%
      st_sfc(crs = 4326)
  } else {
    aeqd <- str_glue("+proj=aeqd +lon_0={centre[1]} +lat_0={centre[2]}")
    circ <- get_hemisphere(centre = centre, crs = aeqd, radius = radius)
    c(get_meridians(), get_parallels()) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(st_crs(circ)) %>% 
      st_intersection(circ) %>% 
      st_transform(4326)
  }
}
