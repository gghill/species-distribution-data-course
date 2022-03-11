# Species occurrence data download in R #
# Offered by transmitting science #
# 10-11 March 2022 #
# DAY 1 -------------------------------------------------------------------
## SETUP ----
setwd("C:/Users/06108182/OneDrive - Nord universitet/Courses/Downloading sdm data R/species occurence data download R")
require(spocc) # powerful package for data retrieval, not always geo-referenced
require(terra) # useful for adapting/transforming projection of spatial data
require(raster)
require(geodata)
require(leaflet) # interactive map
require(neotoma) # fossil data
require(data.table)


