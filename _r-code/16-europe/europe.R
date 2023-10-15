library(sf)
library(tmap)
library(dplyr)
library(stringr)

focus <- "Europe"
lon0 <- 30
lat0 <- 60
proj <- str_glue("+proj=ortho lon_0={lon0} lat_0={lat0}")

name <- "16-europe"

proj_folder <- "~/Documents/code/30days2023"
data_folder <- str_glue("{proj_folder}/data")
this_map <- str_glue("{proj_folder}/{name}")

setwd(this_map)

world <- st_read(str_glue("{data_folder}/world.gpkg")) %>%
  st_make_valid() %>%
  select(CONTINENT)

world_o <- world %>%
  st_transform(proj) %>%
  st_make_valid() %>%
  filter(!st_is_empty(.)) %>%
  group_by(CONTINENT) %>%
  summarise() 

map <- tm_shape(world_o) +
  tm_fill() + 
  tm_shape(world %>% filter(CONTINENT == focus)) +
  tm_fill(fill = "red") +
  tm_layout(frame.lwd = 0)

tmap_save(map, str_glue("{this_map}/{name}.png"))

map
