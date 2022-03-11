# R scripts for the course "Obtaining and cleaning species occurrence data with R"
# A. Marcia Barbosa (https://modtools.wordpress.com)


# set the working directory to the folder containing this script:
# (similar to RStudio menu "Session - Set Working Directory - To Source File Location"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# LOAD REQUIRED PACKAGES ####

library(raster)
library(terra)
library(geodata)
library(leaflet)


# import occurrences saved with previous script:

my_species <- "Alces alces"  # if you then choose a different species, change it here, and not all over the script!

gbif_occurrences <- read.csv(paste0("./outputs/GBIF_", my_species, "_raw.csv"))


# import map of world countries:
countries <- world(resolution = 5, level = 0, path = "./outputs/maps")  # if the map was previously downloaded to this folder, it will be imported from there rather than downloaded again

# import a polygon rangemap:
Mcab_rangemap <- vect("./data/Microtus_cabrerae_rangemap/data_0.shp")


# simple static map:
plot(Mcab_rangemap, col = "red", border = NA)
plot(countries, border = "darkgrey", lwd = 2, add = TRUE)
points(gbif_occurrences[ , c("decimalLongitude" ,"decimalLatitude")], pch = 20)

# interactive map:
leaflet() %>%
  addTiles() %>%
  # add rangemap polygon:
  addPolygons(data = as(Mcab_rangemap, "Spatial"), col = "red") %>%
  # add occurrence points (circles):
  addCircles(data = gbif_occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude)


# IMPORT SOME GRIDDED OCCURRENCE DATA ####

# e.g. presence cells of M. cabrerae from the Portuguese mammal atlas (https://github.com/AMBarbosa/AtlasMamPor):
Mcab_UTMcells_pt <- vect("./data/Microtus_cabrerae_atlas_pt/Microtus_cabrerae_atlas_pt.shp")
plot(Mcab_UTMcells_pt)
plot(countries, border = "darkgrey", add = TRUE)
head(Mcab_UTMcells_pt)
names(Mcab_UTMcells_pt)

# check the CRS of this map:
crs(Mcab_UTMcells_pt)  # EPSG 4326, the one we need for 'leaflet' map below

# interactive map:
leaflet() %>%
  addTiles() %>%
  addPolygons(data = as(Mcab_rangemap, "Spatial"), col = "red") %>%
  addCircles(data = gbif_occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude) %>%
  addPolygons(data = as(Mcab_UTMcells_pt, "Spatial"), col = "green")


# you may also have gridded data in a different CRS (coordinate reference system):
# e.g. Vespa velutina occurrence cells from the EASIN baseline data (https://easin.jrc.ec.europa.eu/easin/Documentation/Baseline):
Vvelutina <- vect("./data/EASIN/Vespa_velutina_nigrithorax.shp")
head(Vvelutina)
plot(Vvelutina)
plot(countries, border = "darkgrey", add = TRUE) # doesn't work bc wrong coord system
# if you click the "zoom" button on top of the map, you can see these are polygon grid cells, rather than points of occurrence


# check the CRS of this map:
crs(Vvelutina)  # EPSG 3035, NOT the one we need for 'leaflet' map below...

# interactive map:
leaflet() %>%
  addTiles() %>%
  addPolygons(data = as(Vvelutina, "Spatial"), col = "green")  # what's wrong? map doesn't overlap, because it's in a different CRS

# so, you first need to project/transform this map to the EPSG required by 'leaflet', which is '4326' (WGS84 unprojected geographic coordinates, the same as those in GBIF and most global datasets):
Vvelutina_4326 <- terra::project(Vvelutina, "epsg:4326")

plot(Vvelutina_4326)
plot(countries, border = "darkgrey", add = TRUE) # doesn't work bc wrong coord system

# now add the projected map to the interactive plot:
leaflet() %>%
  addTiles() %>%
  addPolygons(data = as(Vvelutina_4326, "Spatial"), col = "green")  # this one overlaps correctly

# zooming in, you can see these are polygon grid cells, rather than points of occurrence
# you can get the coordinates of their centroids:
Vvelutina_coords <- crds(centroids(Vvelutina_4326))
head(Vvelutina_coords)
class(Vvelutina_coords)
# convert to data.frame for the next commands:
Vvelutina_coords <- data.frame(Vvelutina_coords)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = as(Vvelutina_4326, "Spatial"), col = "green") %>%
  addCircles(data = Vvelutina_coords, lng = ~ x, lat = ~ y, col = "purple")
# zoom in to see the points up close


# mind that the species was recorded present somewhere within each grid cell, not at the centroid!
# so, these point coordinates have considerable spatial uncertainty, up to the maximum distance between the centroid and the corners of each grid cell
# add this information to the data set:

Vvelutina_coords$coord_uncertainty <- (10000 * sqrt(2)) / 2  # distance in meters from centroid to corner (half the diagonal) of a square with 10-km side))
head(Vvelutina_coords)


names(Mcab_UTMcells_pt)
Mcab_UTMcells_pt$coord_uncertainty <- (10000 * sqrt(2)) / 2
head(Mcab_UTMcells_pt)
