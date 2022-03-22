# R scripts for the course "Obtaining and cleaning species occurrence data with R"
# A. Marcia Barbosa (https://modtools.wordpress.com)


# set the working directory to the folder containing this script:
# (similar to RStudio menu "Session - Set Working Directory - To Source File Location"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# LOAD REQUIRED PACKAGES ####

library(leaflet)
library(spThin)
library(humboldt)
library(raster)
library(geodata)
library(fuzzySim)


# IMPORT THE DATA ####

# we'll use our cleaned GBIF data here
# you can instead import your cleaned homogenized data (if you have it)

my_species <- "Macrocystis pyrifera"  # choose a species for which you have exported data with yesterday's script
# if you then choose a different species, change it here, and not all over the script!

occurrences <- read.csv(paste0("./outputs/occurrences_", my_species, "_cleaned.csv"))

head(occurrences)


#############
# the following are ALTERNATIVE ways of reducing spatial bias
# try them all, then choose the one that best fits your purpose
#############


# THIN THE DATA WITH 'spThin' ####

?thin

nrow(occurrences)
names(occurrences)

min_distance <- 10  # in km, as per ?thin thin.par

set.seed(42)  # set a seed of random numbers, so that the next command always produces the same result
occurrences_thinned <- thin(occurrences, 
                            lat.col = "decimalLatitude", 
                            long.col = "decimalLongitude", 
                            spec.col = "species", 
                            thin.par = min_distance, 
                            reps = 10, 
                            locs.thinned.list.return = TRUE, 
                            write.files = FALSE)  # can take a long time for large datasets! remember to clean (including elimination of duplicates) beforehand
# notice this will create a 'log' file in your working directory on disk

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plotThin(occurrences_thinned)
# see if a plateau is reached in the first plots, otherwise increase 'reps' above, or use a different set.seed()

length(occurrences_thinned)  # you get as many tables of selected points as the number of 'reps' specified above
lapply(occurrences_thinned, head)
sapply(occurrences_thinned, nrow)  # as the algorithm includes randomization, each repetition produces a different selection of points, not all with the maximum possible number


# map to see the difference:
leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, col = "turquoise") %>%
  addCircles(data = occurrences_thinned[[1]], lng = ~ Longitude, lat = ~ Latitude, col = "blue")  # occurrences_thinned[[1]] is just one of the possible thinned datasets that were generated


# THIN THE DATA WITH 'humboldt.occ.rarefy' ####

?humboldt.occ.rarefy

occurrences_rarefied <- humboldt.occ.rarefy(in.pts = occurrences, colxy = c("decimalLongitude", "decimalLatitude"), rarefy.dist = min_distance, rarefy.units = "km")

head(occurrences_rarefied)

nrow(occurrences)
nrow(occurrences_rarefied)

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, col = "turquoise") %>%
  addCircles(data = occurrences_rarefied, lng = ~ decimalLongitude, lat = ~ decimalLatitude, col = "blue")


# THIN THE DATA WITH 'gridRecords' ####

?gridRecords
# this function uses a raster map to thin the occurrences based on pixel size
# this should be the raster map(s) that you will use for analysing species-environment relationships

# download a raster map of global elevation:
elev <- elevation_global(res = 5, path = "./outputs/maps")
# try it with bathymetry instead (in meters i think)
bathy <- raster("ETOPO1_Bed_g_geotiff.tif")


# plot the map:
par(mfrow = c(1, 1))
plot(elev)
plot(SST[[1]])

# cut the map to the region of interest, e.g. to a 200-km buffer around the occurrences:

# first, 'occurrences' needs to be converted to a spatial object:
occurrences_spatial <- vect(occurrences, geom = c("decimalLongitude", "decimalLatitude"), crs = "epsg:4326")  # because we KNOW this is the CRS of GBIF coordinates

myregion <- aggregate(buffer(occurrences_spatial, width = 200000))
myregion <- c(-130, -110, 10, 50)
w_coast_ext <- extent(-130, -110, 10, 50) # raster extent to match window (if desired)

plot(myregion, add = TRUE)

plot(myregion, col = "beige")
countries <- geodata::world(resolution = 5, level = 0, path = "./outputs/maps")
plot(countries, border = "darkgrey", add = TRUE)

# (you can replace or intersect 'myregion' with your choice  of countries or other area of interest - ask me if you need help!)

elev_myregion <- trim(mask(elev, myregion))
bathy_myregion <- crop(bathy,w_coast_ext)
plot(elev_myregion)
plot(bathy_myregion)


# plot within a limited region, to get a better look at the pixel resolution:
plot(bathy_myregion, xlim = c(-8, -6), ylim = c(40.5, 42))

# add the occurrence points:
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], col = "grey35", pch = 20, cex = 0.8)

# grid these points to the same resolution of the raster map (thinning distance will be the pixel size):
occurrences_gridded <- gridRecords(rst = bathy_myregion, pres.coords = occurrences[ , c("decimalLongitude", "decimalLatitude")])
head(occurrences_gridded)
# you get a column called 'presence' with 1 for pixels with and 0 for pixels without occurrence records, and two columns 'x' and 'y' with the centroid coordinates of those pixels (cells), plus the value/s in the raster map/stack for each pixel

sum(occurrences_gridded$presence)  # number of retained presences

# add the gridded occurrences to the map:
points(occurrences_gridded[occurrences_gridded$presence == 1, c("x", "y")], pch = 20, cex = 1.5, col = "blue")
# add also the gridded absences (of records):
points(occurrences_gridded[occurrences_gridded$presence == 0, c("x", "y")], pch = 1, col = "red")


# note that the previous plot was a close-up on a limited window of coordinates, but the gridded occurrences were computed for the complete 'rst' you provided to 'gridRceords':
plot(bathy_myregion)
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], col = "grey35", pch = 20, cex = 0.5)
points(occurrences_gridded[occurrences_gridded$presence == 1, c("x", "y")], pch = 20, col = "blue")
points(occurrences_gridded[occurrences_gridded$presence == 0, c("x", "y")], pch = 20, col = "red", cex = 0.1)


leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  #addPolygons(data = as(myregion, "Spatial"), col = "grey") %>%
  addRasterImage(raster(elev_myregion)) %>%
  addCircles(data = occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, col = "turquoise") %>%
  addCircles(data = occurrences_gridded[occurrences_gridded$presence == 1, ], lng = ~ x, lat = ~ y, col = "blue")

# need to double check projection/crs value and try to get functional legend
crs(bathy_myregion) <- "EPSG:4326"
leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  #addPolygons(data = as(myregion, "Spatial"), col = "grey") %>%
  addRasterImage(bathy_myregion) %>%
  addLegend() %>%
  addCircles(data = occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, col = "turquoise") %>%
  addCircles(data = occurrences_gridded[occurrences_gridded$presence == 1, ], lng = ~ x, lat = ~ y, col = "blue")


# (note that the raster pixels may be slightly displaced in the leaflet map)
