# Applied follow up exercise:
# Mapping kelp occurences within defined
# Norwegian eco-regions

# SETUP ----
require(sdmpredictors)
require(leaflet)
require(raster)
library(httr)
require(ggplot2)
require(dplyr)
require(tidyverse)
require(rgdal)
require(broom)
library(raster)
library(terra)
library(geodata)
library(leaflet)
require(sf)
require(spocc)

setwd('C:/Users/06108182/OneDrive - Nord universitet/Courses/Downloading sdm data R/species occurence data download R')

## Read in shape ----
shape <- readOGR(
  dsn = './fjord_katalogen/fjordkatalogen_omrade.shp',
  verbose = FALSE
)
# Geographical extent: 
# (from metadata: https://kartkatalog.geonorge.no/metadata/miljodirektoratet/d4b28454-ebd6-4425-9a66-00cb2d7e57ed)
# North : 72.00
# South : 57.00
# East : 33.00
# Vest: 2,00

summary(shape)
length(shape)
head(shape@data)

# Plotting ----
fortified_shape <- tidy(shape, region = 'navn')
countries <- world(resolution = 5, level = 0, path = "./outputs/maps")  # if the map was previously downloaded to this folder, it will be imported from there rather than downloaded again


ggplot() +
  #geom_sf(data = country_pts)
  geom_polygon(data = fortified_shape, aes( x = long/(10^5), y = lat/(10^5), group = group), 
               fill = '#c1f2f5',
               color = '#5c8a5a')
shape@data
crs(shape)


  

## overlaying species occurrence ----
library(rgbif)
my_species <- "Saccharina"  # if you then choose a different species, change it here, and not all over the script!
my_window <- c(6, 11, 64, 79)
my_window_lon <- paste0(my_window[1:2], collapse = ", ")
my_window_lat <- paste0(my_window[3:4], collapse = ", ")
gbif_data <- occ_data(scientificName = my_species, hasCoordinate = TRUE, limit = 5000, decimalLongitude = my_window_lon, decimalLatitude = my_window_lat)
gbif_occurrences <- gbif_data$data # selecting useful part of large gbif object
gbif_occurrences

## try with spocc ----
spocc_data <- occ(my_species, from = c("gbif", "bison", "inat", "ebird", "vertnet", "idigbio", "obis", "ala"), has_coords = TRUE, limit = 5000)
# should revise to get more records, but only from target area
# save this object to disk:
saveRDS(spocc_data, paste0("./outputs/spocc_", my_species, "_raw.rds"))  # we name it "raw" because the data haven't been cleaned yet

spocc_data <- readRDS(paste0("./outputs/spocc_", my_species, "_raw.rds"))

# combine the 'spocc_data' list into one data frame:
?occ2df
spocc_df <- occ2df(spocc_data)
spocc_df

str(spocc_df)  # longitude and latitude are character...
# convert them to numeric for mapping:
spocc_df$longitude <- as.numeric(spocc_df$longitude)
spocc_df$latitude <- as.numeric(spocc_df$latitude)

# poor gbif occurrences

# map eco regions and species occurrence
leaflet() %>%
  addTiles() %>%
  flyTo(14.4, 67.28, zoom = 5) %>%
  addPolygons(data = spTransform(shape,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs")), 
              col = '#5c8a5a',
              fill = '#c1f2f5') %>%
  addCircleMarkers(data = spocc_df, lng = ~ longitude, lat = ~ latitude, radius = 5,
                   clusterOptions = markerClusterOptions())


