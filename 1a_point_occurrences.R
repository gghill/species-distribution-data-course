# R scripts for the course "Obtaining and cleaning species occurrence data with R"
# A. Marcia Barbosa (https://modtools.wordpress.com)


# the following command sets the working directory to the folder containing this script (similar to RStudio menu "Session - Set Working Directory - To Source File Location"):
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# create a folder for the outputs (if it doesn't already exist)
if (!file.exists("../outputs")) dir.create("../outputs")  # '../' goes up one level from the current working directory, so this creates the 'outputs' folder just outside the 'Rscripts' folder


# LOAD REQUIRED PACKAGES ####

# packages should first be installed USING THE COMMANDS in script '0_install_packages.R' (not with general commands or update menus!)

# every time you get an error about a missing or irremovable package, install that package -- e.g. install.packages("ridigbio") -- and try again

require(sdmpredictors)
require(tidyverse)
require(dplyr)
library(rgbif)
library(terra)
library(geodata)
library(leaflet)
library(spocc)
library(neotoma)
library(data.table)
library(raster)
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))



# DOWNLOAD OCCURRENCE DATA FROM GBIF ####

# we'll start by downloading GBIF data for an example species

# after running the script and observing how it works, you can choose another species and run it again, adapting where needed

# but note things can be quite slow when there are many occurrence points!

# define the species you want to get data for:
my_species <- "Macrocystis pyrifera"  # if you then choose a different species, change it here, not all over the script!

gbif_data <- occ_data(scientificName = my_species, hasCoordinate = TRUE, limit = 500)

gbif_data  # scroll up to see the full result. If "Records found" is larger than "Records returned", you need to increase the 'limit' argument above -- run '?occ_data' for options and limitations. Mind that data download takes a long time when there are many occurrences!

# if your species is widespread, you can download occurrences only within a specified window of longitude and latitude coordinates:

# get a map of world countries and get yourself situated:
countries <- world(resolution = 5, level = 0, path = "./outputs/maps")

plot(countries, border = "darkgrey")

# xmin, xmax, ymin, ymax coordinates of a region of interest:
#my_window <- c(-130, -110, 10, 50) # if you then choose a different window, change it here, and not all over the script!
my_window <- c(-10, 5, 35, 45)  # spain

plot(ext(my_window), border = "red", lwd = 2, add = TRUE)  # check that it's where you want it on the map
w_coast_ext <- extent(-130, -110, 10, 50) # raster extent to match window (if desired)

## incorporate bio-ORACLE layers ----
surf_vel_max <- load_layers(c("BO2_curvelmax_bdmin","BO2_curvelmin_bdmin"))
surf_vel_crop <- crop(surf_vel_max, w_coast_ext) # if mapping globally don't use crop
SST <- load_layers("BO2_tempmean_ss")


# convert to dataframes
vel_max <- as.data.frame(surf_vel_max[[1]],xy=TRUE)
vel_min <- as.data.frame(surf_vel_max[[2]],xy=TRUE)
names(vel_min) <- c("x","y","Minimum Surface Current v")


# plot countries map within 'my_window' only:
plot(countries, border = "darkgrey", xlim = my_window[1:2], ylim = my_window[3:4])

# if global data are too much, you can download GBIF data from this window only:
my_window_lon <- paste0(my_window[1:2], collapse = ", ")
my_window_lat <- paste0(my_window[3:4], collapse = ", ")

gbif_data <- occ_data(scientificName = my_species, hasCoordinate = TRUE, limit = 5000, decimalLongitude = my_window_lon, decimalLatitude = my_window_lat)

gbif_data  # remember to check if "Records found" is larger than "Records returned", in which case you need to increase the 'limit' above

# see how to cite the sources of these data:
gbif_citation(gbif_data)
# NOTE: If you plan to use these data in any report or publication, instead of including these multiple citations, you can download the data directly from www.gbif.org (then import the .csv to R) and note down the DOI and citation for the complete dataset. It is very important to properly cite the data sources! GBIF is not a source, just a repository for many people who put in very hard work to collect these data and make them available.


# check how the data are organized:
names(gbif_data)
names(gbif_data$meta)
names(gbif_data$data)
head(gbif_data$data)
head(as.data.frame(gbif_data$data)) # LOTS of NAs, but 119 columns so makes sense

gbif_occurrences <- gbif_data$data # selecting useful part of large gbif object

unique(sapply(gbif_occurrences, class)) # get unique classes represented in gbif occurences table
# useful for identifying problem data types like lists

# some columns can be lists, which cannot be saved in a .csv (and may also cause problems elsewhere)
which(sapply(gbif_occurrences, class) == "list")
# remove list columns (if any):
gbif_occurrences[which(sapply(gbif_occurrences, class) == "list")] <- NULL

# save the GBIF occurrence data as a .csv file:
write.csv(gbif_occurrences, paste0("./outputs/GBIF_", my_species, "_raw.csv"), row.names = FALSE)  # we name it "raw" because the data haven't been cleaned yet

# from here on, you don't need to download the data again from GBIF - you can just import from your .csv:
gbif_occurrences <- read.csv(paste0("./outputs/GBIF_", my_species, "_raw.csv"))
head(gbif_occurrences)


# MAP THE GBIF DATA ####

# static maps:

plot(countries, border = "darkgrey")
points(gbif_occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "red")

# close-up to my_window:
plot(countries, xlim = my_window[1:2], ylim = my_window[3:4], border = "darkgrey")
points(gbif_occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "red")


# interactive map (where you can zoom and pan):

## plot with added bio-ORACLE raster ----
vel_comb <- cbind(vel_min,vel_max$BO2_curvelmax_bdmin)
names(vel_comb) <- c("x","y","Minimum Surface Current v","Maximum Surface Current v")

names(vel_comb)

vel_comb_mut <- mutate(vel_comb,
                       threshold = ifelse(100 > vel_comb$`Minimum Surface Current v`*100 & vel_comb$`Minimum Surface Current v`*100 > 10 & 10 < vel_comb$`Maximum Surface Current v`*100 & vel_comb$`Maximum Surface Current v`*100 < 100,
                                          (vel_comb$`Minimum Surface Current v`*100+vel_comb$`Maximum Surface Current v`*100)/2,0)
) %>%
  drop_na()
vel_comb_mut <- vel_comb_mut[,c("x","y","threshold")]
vel_raster <- rasterFromXYZ(vel_comb_mut, crs = "EPSG:4326")
vel_raster_max <- rasterFromXYZ(vel_comb_mut[,c("x","y","Maximum Surface Current v")])
# goal is to plot and label the threshold value
# vel_raster is a raster form x,y,threshold surface velocity (0 if it falls outside the range, otherwise average of max/min)
pal <- colorNumeric(
  palette = my.colors(100),
  domain = vel_comb_mut$`Maximum Surface Current v`
)
SST_df <- as.data.frame(SST@layers[[1]]) %>%
  drop_na()
pal_SST <- colorNumeric(
  palette = my.colors(100),
  domain = SST_df$BO2_tempmean_ss
)


leaflet() %>%
  # add background map (default OpenStreetMap):
  addTiles() %>%
  addRasterImage(vel_raster, colors = my.colors(10), opacity = .8) %>% # add surface velocity raster
  addLegend(pal = pal, values = vel_comb_mut$threshold) %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = gbif_occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode)  # you can choose another column for the "label", depending on what information you want to see when you mouse over each point on the map

# plot with SST mean instead
leaflet() %>%
  # add background map (default OpenStreetMap):
  addTiles() %>%
  addRasterImage(SST[[1]], colors = my.colors(100), opacity = .8) %>% # add surface velocity raster
  addLegend(pal = pal_SST, values = SST_df$BO2_tempmean_ss) %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = gbif_occurrences, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode)  # you can choose another column for the "label", depending on what information you want to see when you mouse over each point on the map


# use the mouse to zoom and pan on the map

# you can click the "show in new window" icon on the plotting pane to get a larger map


# DOWNLOAD DATA FROM MULTIPLE SOURCES SIMULTANEOUSLY ####

?occ # search for species occurrences across many data sources by one or more names

spocc_data <- occ(my_species, from = c("gbif", "bison", "inat", "ebird", "vertnet", "idigbio", "obis", "ala"), has_coords = TRUE, limit = 5000)
spocc_data  # remember to check 'found' vs. 'returned', to make sure all existing records were downloaded; if not, increase the 'limit' above and run 'occ' again

# see how the data are organized:
names(spocc_data) # sorted by data source
names(spocc_data$gbif)
names(spocc_data$gbif$data) # organized by species, in the example case only one
names(spocc_data$gbif$data[[1]]) # 129 columns of data


# save this object to disk:
saveRDS(spocc_data, paste0("./outputs/spocc_", my_species, "_raw.rds"))  # we name it "raw" because the data haven't been cleaned yet

spocc_data <- readRDS(paste0("./outputs/spocc_", my_species, "_raw.rds"))

# combine the 'spocc_data' list into one data frame:
?occ2df
spocc_df <- occ2df(spocc_data)
spocc_df

table(spocc_df$prov)  # providers with records for my_species

str(spocc_df)  # longitude and latitude are character...
# convert them to numeric for mapping:
spocc_df$longitude <- as.numeric(spocc_df$longitude)
spocc_df$latitude <- as.numeric(spocc_df$latitude)


# interactive map:
leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = spocc_df, lng = ~ longitude, lat = ~ latitude)


# you may want to give different colours to points from different sources / providers:

unique(spocc_df$prov)

## plot data from different sources and add raster enviro layer ----
leaflet() %>%
  addTiles() %>%
  # add global raster + legend
  addRasterImage(vel_raster, colors = my.colors(100), opacity = .8) %>% # add surface velocity raster
  addLegend(pal = pal, values = vel_comb_mut$`Maximum Surface Current v`) %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = subset(spocc_df, prov == "gbif"), lng = ~ longitude, lat = ~ latitude, col = "blue") %>%
  addCircles(data = subset(spocc_df, prov == "bison"), lng = ~ longitude, lat = ~ latitude, col = "red") %>%
  addCircles(data = subset(spocc_df, prov == "idigbio"), lng = ~ longitude, lat = ~ latitude, col = "yellow") %>%
  addCircles(data = subset(spocc_df, prov == "obis"), lng = ~ longitude, lat = ~ latitude, col = "bright green") %>%
  addCircles(data = subset(spocc_df, prov == "ala"), lng = ~ longitude, lat = ~ latitude, col = "purple") %>%
addCircles(data = subset(spocc_df, prov == "inat"), lng = ~ longitude, lat = ~ latitude, col = "pink")

pal_prov <- colorFactor(
  palette = "viridis",
  domain = spocc_df$prov
)
# try again with SST instead
leaflet() %>%
  addTiles() %>%
  # add global raster + legend
  addRasterImage(SST[[1]], colors = my.colors(100), opacity = .8, group = "SST") %>% # add surface velocity raster
  addLegend("bottomright",title = "mean SST in deg C",pal = pal_SST, values = SST_df$BO2_tempmean_ss, group = "SST") %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
    addCircles(data = spocc_df, lng = ~ longitude, lat = ~ latitude, col = ~pal_prov(prov), group = "Kelp") %>%
    addLegend("bottomright", pal = pal_prov, title="Kelp obs source",
            values = spocc_df$prov, group="Kelp") %>%
  addLayersControl(overlayGroups = c("SST","Kelp"),
                     options = layersControlOptions(collapsed = FALSE))  
  
  # addCircles(data = subset(spocc_df, prov == "gbif"), lng = ~ longitude, lat = ~ latitude, col = "blue") %>%
  # addCircles(data = subset(spocc_df, prov == "bison"), lng = ~ longitude, lat = ~ latitude, col = "red") %>%
  # addCircles(data = subset(spocc_df, prov == "idigbio"), lng = ~ longitude, lat = ~ latitude, col = "yellow") %>%
  # addCircles(data = subset(spocc_df, prov == "obis"), lng = ~ longitude, lat = ~ latitude, col = "bright green") %>%
  # addCircles(data = subset(spocc_df, prov == "ala"), lng = ~ longitude, lat = ~ latitude, col = "purple") %>%
  # addCircles(data = subset(spocc_df, prov == "inat"), lng = ~ longitude, lat = ~ latitude, col = "pink") %>%

# but BEWARE! the 'occ2df' function excludes columns that may be crucial for data cleaning, like those specifying the spatial uncertainty of the coordinates (e.g. "coordinateUncertaintyInMeters" in GBIF, "positional_accuracy" in iNaturalist) or even the "occurrence status", where the species may actually be defined as absent
# that's why it's important to keep the complete 'spocc_data' object with all the information


# DOWNLOAD PALEO DATA FROM NEOTOMA DATABASE ####

neot_dataset <- get_dataset(taxonname = my_species)
# 0 results for the example species...
# let's try with the genus and a wildcard (asterisk), just to see what's out there for other species in the same genus:
neot_dataset <- get_dataset(taxonname = "Microtus*")
neot_sites <- get_site(neot_dataset)
head(neot_sites)

# interactive map:
leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = neot_sites, lng = ~ long, lat = ~ lat, col = "orange")


# HOMOGENIZE COORDINATES FROM DIFFERENT DATASETS ####

# if you have geographic coordinates in degree-minute-second (DMS) format (e.g., 35º15'33.62" W), you need to convert them to numeric form in decimal degrees, e.g. with the 'dms2dec' function from GitHub:
source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/dms2dec", encoding = "UTF-8")

# import some data with DMS coordinates:
# still just Microtus cabrerae occurrence data, but different format
Mcabr_dms <- read.csv("./data/Microtus_cabrerae_coords/Mcabrerae_coords_dms.csv")
head(Mcabr_dms)

# convert the DMS coordinates to decimal format:
Mcabr_dms$lat_decimal <- dms2dec(Mcabr_dms$lat)
Mcabr_dms$lon_decimal <- dms2dec(Mcabr_dms$lon)
Mcabr_dms  # confirm that the new columns look right

# you can also check out the 'dms2dd' function in the 'biogeo' package, or the 'degree' function in the 'OSMscale' package

# map these data to make sure the locations make sense:
leaflet() %>%
  addTiles() %>%
  addCircles(data = Mcabr_dms, lng = ~ lon_decimal, lat = ~ lat_decimal)


# if some of your coordinates are in a different CRS (coordinate reference system, including geographic projection, datum and ellipsoid), you need to 1) convert your data to spatial format; 2) tell R which is the CRS of those coordinates; and 3) transform it to the same CRS of your remaining data:

Mcabr_utm <- read.csv("./data/Microtus_cabrerae_coords/Mcabrerae_coords_UTM29N.csv")
head(Mcabr_utm)  # you can see these coordinates don't look like they can be in geographic degrees, which can only vary between -90 and 90 degrees latitude, and between -180 and 180 degrees longitude

# so, first convert the dataset to a spatial object, specifying which columns contain the geometry, i.e. the longitude and latitude coordinates, respectively:
Mcabr_utm <- vect(Mcabr_utm, geom = c("X", "Y"))
# check if it has a CRS:
crs(Mcabr_utm)
# as it was imported from a plain text (.csv) file and not a spatial format, there isn't any included info on the CRS
# we must find out which is the CRS of these coordinates, and assign it to our spatial object
# for these coordinates, we know it's EPSG:32629, i.e. WGS 84 / UTM zone 29N
crs(Mcabr_utm) <- "epsg:32629"

# now, you can transform the original CRS of these coordinates to your target CRS, to match the other coordinates in your study:
Mcabr_utm_4326 <- terra::project(Mcabr_utm, "epsg:4326") # project(vector,OUTPUT projection)
head(Mcabr_utm_4326)
# add the spatial coordinates to the attribute table:
Mcabr_utm_4326 <- data.frame(Mcabr_utm_4326, crds(Mcabr_utm_4326))
head(Mcabr_utm_4326)

# map these data to make sure the locations make sense:
leaflet() %>%
  addTiles() %>%
  addCircles(data = Mcabr_utm_4326, lng = ~ x, lat = ~ y)


# COMBINE DATA FROM DIFFERENT SOURCES ####

# first, select the relevant columns from each data set:

names(gbif_occurrences)
names(Mcabr_dms)
names(Mcabr_utm_4326)

gbif_selected_columns <- gbif_occurrences[ , c("species", "decimalLatitude", "decimalLongitude", "occurrenceStatus", "coordinateUncertaintyInMeters")]
Mcabr_dms <- Mcabr_dms[ , c("lat_decimal", "lon_decimal")]
Mcabr_utm_4326 <- Mcabr_utm_4326[ , c("Species", "x", "y")]


# add relevant columns to datasets that may be missing them:

Mcabr_dms$species <- "Microtus cabrerae"
Mcabr_dms$source <- "DMS coords"
Mcabr_utm_4326$source <- "UTM coords"
gbif_selected_columns$source <- "GBIF"


# then, give all datasets matching column names:

names(gbif_selected_columns)
names(Mcabr_dms)
names(Mcabr_utm_4326)

# one at a time renaming using gsub(replace, with, in)
names(Mcabr_dms) <- gsub("lat_decimal", "decimalLatitude", names(Mcabr_dms))
names(Mcabr_dms) <- gsub("lon_decimal", "decimalLongitude", names(Mcabr_dms))
names(Mcabr_dms)

names(Mcabr_utm_4326)
names(Mcabr_utm_4326) <- gsub("x", "decimalLongitude", names(Mcabr_utm_4326))
names(Mcabr_utm_4326) <- gsub("y", "decimalLatitude", names(Mcabr_utm_4326))
names(Mcabr_utm_4326) <- gsub("Species", "species", names(Mcabr_utm_4326))
names(Mcabr_utm_4326)


# join the data, matching columns by name and filling with NA values the columns that aren't present in all datasets (fill=TRUE for 'data.table' class objects, which you can obtain with 'setDT'):
data_joined <- data.frame(rbind(setDT(gbif_selected_columns), setDT(Mcabr_dms), setDT(Mcabr_utm_4326), fill = TRUE))
head(data_joined)
tail(data_joined)


# map the data to make sure everything seems to be in place:

leaflet() %>%
  addTiles() %>%
  addCircles(data = data_joined, lng = ~ decimalLatitude, lat = ~ decimalLongitude)  # WHAT HAPPENED?!

# lat and lng arguments were switched... do it right this time:
leaflet() %>%
  addTiles() %>%
  addCircles(data = data_joined, lng = ~ decimalLongitude, lat = ~ decimalLatitude)

# check that it looks OK!
