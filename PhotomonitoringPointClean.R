# Load libraries
library(tidyverse)
library(sf)


nabuphoto <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NABU_photomonitoring.csv")

names(nabuphoto)

sum <- nabuphoto %>%
  dplyr::group_by(Natural.Zone, State) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()


projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

nabuphoto <- sf::st_as_sf(x = nabuphoto,                         
               coords = c("Longitude", "Latitude"),
               crs = projcrs)

sf::st_write(nabuphoto, "C:\\Users\\TRAVAH\\Documents\\GIS\\NABUphotoclean.shp", driver = 'ESRI Shapefile', append = FALSE)
