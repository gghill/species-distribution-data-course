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
my_window <- c(-1, 64, 11, 79)
my_window_lon <- paste0(my_window[1:2], collapse = ", ")
my_window_lat <- paste0(my_window[3:4], collapse = ", ")
gbif_data <- occ_data(scientificName = my_species, hasCoordinate = TRUE, limit = 5000, decimalLongitude = my_window_lon, decimalLatitude = my_window_lat)
gbif_occurrences <- gbif_data$data # selecting useful part of large gbif object
gbif_occurrences

## try with spocc ----
spocc_data <- occ(my_species, geometry = my_window, from = c("gbif", "bison", "inat", "ebird", "vertnet", "idigbio", "obis", "ala"), has_coords = TRUE, limit = 5000)
# this won't run, check geometry documentation again
# should revise to get more records, but only from target area
# geometry should do this
# save this object to disk:
saveRDS(spocc_data, paste0("./outputs/spocc_", my_species, "restricted_raw.rds"))  # we name it "raw" because the data haven't been cleaned yet

spocc_data_restricted <- readRDS(paste0("./outputs/spocc_", my_species, "restricted_raw.rds"))

# combine the 'spocc_data' list into one data frame:
?occ2df
spocc_df <- occ2df(spocc_data_restricted)
spocc_df

str(spocc_df)  # longitude and latitude are character...
# convert them to numeric for mapping:
spocc_df$longitude <- as.numeric(spocc_df$longitude)
spocc_df$latitude <- as.numeric(spocc_df$latitude)

# poor gbif occurrences

# read in Norwegian aquaculture registry data
aqua_sites <- read.delim('./data/aqua_sites_algae_exp.txt')
aqua_sites$label <- with(aqua_sites, paste(
  Site.Name,
  "<br>Capacity:", Capacity,
  "<br>Culture:", Culture
))
# doublecheck filtering approach, some sites being excluded
# such as blue harvest lofoten that have algae
kelp <- makeIcon('seaweed.png', iconWidth = 18, iconHeight = 18)
farm <- makeIcon('norway.png', iconWidth = 24, iconHeight = 24)
html_legend <- "<img src='https://i.ibb.co/4RVNHQW/norway.png'style='width:18px;height:18px;'>Norwegian Algae Farms<br/>
<img src='https://i.ibb.co/j4S7bB4/seaweed.png'style='width:18px;height:18px;''>Saccharina latissima occurences"
# map eco regions and species occurrence
leaflet() %>%
  addTiles() %>%
  flyTo(14.4, 67.28, zoom = 5) %>%
  addPolygons(data = spTransform(shape,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs")),
              col = '#5c8a5a',
              fill = '#c1f2f5') %>%
  addMarkers(data = spocc_df, lng = ~ longitude, lat = ~ latitude, icon = kelp, label = spocc_df$prov,
                   clusterOptions = markerClusterOptions()) %>%
  addMarkers(lng = aqua_sites$Long, lat = aqua_sites$Lat, icon = farm, 
             label = aqua_sites$Site.Name,
             popup = paste(aqua_sites$label),
             clusterOptions = markerClusterOptions()) %>%
  addControl(html = html_legend, position = 'bottomright')


