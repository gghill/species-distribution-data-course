# R scripts for the course "Obtaining and cleaning species occurrence data with R"
# A. Marcia Barbosa (https://modtools.wordpress.com)


# set the working directory to the folder containing this script:
# (similar to RStudio menu "Session - Set Working Directory - To Source File Location"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# LOAD REQUIRED PACKAGES ####

library(leaflet)
library(scrubr)
library(CoordinateCleaner)
library(terra)
library(geodata)


# IMPORT THE DATA TO BE CLEANED ####

# in the examples we'll use our GBIF data exported with yesterday's script (in case we didn't manage to join more data yet)
# you can instead import your homogenized table with data from other datasets too (if you have it)

my_species <- "Microtus cabrerae"  # choose a species for which you have exported data with yesterday's script
# if you then choose a different species, change it here, and not all over the script!

occurrences_raw <- read.csv(paste0("./outputs/GBIF_", my_species, "_raw.csv"))

head(occurrences_raw)


# MAP AND VISUALLY INSPECT THE DATA ####

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences_raw, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "red")  # you can choose another column for the "label", depending on what information you want to see when you mouse over each point on the map
# note some points may not have info in a particular column


# CLEAN THE OCCURRENCE DATA ####

# the following are COMPLEMENTARY ways of cleaning occurrence data
# use them all, and also explore additional options in the help files


# remove records of absence:

names(occurrences_raw)
unique(occurrences_raw$occurrenceStatus)  # check for different indications of "absent", which could be in different languages!
occurrences_clean <- subset(occurrences_raw, !(occurrenceStatus %in% c("ABSENT", "absent", "Absent", "AUSENTE", "ausente", "Ausente")))
nrow(occurrences_raw)
nrow(occurrences_clean)


# map both raw and cleaned points, to see the difference:

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences_raw, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "red") %>%
  addCircles(data = occurrences_clean, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "blue")


# remove records with missing coordinates:

names(occurrences_clean)
nrow(occurrences_clean)
occurrences_clean <- subset(occurrences_clean, !is.na(decimalLatitude) & !is.na(decimalLongitude))
nrow(occurrences_clean)  # if 'nrow' doesn't change, it means the data were already free of missing coordinates


# remove duplicate points:

nrow(occurrences_clean)
dups <- which(duplicated(occurrences_clean[ , c("decimalLongitude", "decimalLatitude")]))
length(dups)
if (length(dups) > 0)  occurrences_clean <- occurrences_clean[-dups, ]
nrow(occurrences_clean)


# remove points where both longitude and latitude are zero:

occurrences_clean <- subset(occurrences_clean, !(decimalLatitude == 0 & decimalLongitude == 0))
nrow(occurrences_clean)


# do some automatic data cleaning with functions of the 'scrubr' package:

?coord_incomplete

nrow(occurrences_clean)
occurrences_clean <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(occurrences_clean))))
nrow(occurrences_clean)


# map again to see the difference:

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences_raw, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "red") %>%
  addCircles(data = occurrences_clean, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "blue")


# remove occurrences with reported coordinate uncertainty (location error) larger than what you're willing to use (depending on the spatial resolution of the analysis you plan to do with your data), e.g. 10x10 km2:

# first, let's check the values of coordinate uncertainty in our data:
summary(occurrences_clean$coordinateUncertaintyInMeters)
hist(occurrences_clean$coordinateUncertaintyInMeters)

# map the coordinate uncertainty with proportionally sized circles:
countries <- geodata::world(resolution = 5, level = 0, path = "./outputs/maps")
plot(countries, xlim = c(-10, 5), ylim = c(35, 45), border = "darkgrey")
points(occurrences_clean[ , c("decimalLongitude", "decimalLatitude")], cex = occurrences_clean$coordinateUncertaintyInMeters / 10000)
# click the "zoom" button on top of the map, to see circles sized according to the spatial uncertainty of each occurrence record


# choose your maximum allowed coordinate uncertainty:
max_uncertainty <- (10000 * sqrt(2)) / 2  # meters from centroid to corner (half the diagonal) of a square with 10-km side))
# max_uncertainty <- 10000

occurrences_clean <- coord_uncertain(occurrences_clean, coorduncertainityLimit = max_uncertainty)
nrow(occurrences_clean)
# but note that this will only remove records where coordinate uncertainty is adequately reported in the dataset, which may not always be the case! careful mapping and visual inspection are necessary


# map again:

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences_raw, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "red") %>%
  addCircles(data = occurrences_clean, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "blue")


# do some more automatic data cleaning with functions of the 'CoordinateCleaner' package:

?clean_coordinates  # read the help file to know what each test is doing, and what other tests are available that you may want to explore!

names(occurrences_clean)

occurrences_coordcleaner <- clean_coordinates(occurrences_clean, 
                                              lon = "decimalLongitude", 
                                              lat = "decimalLatitude", 
                                              countries = "countryCode", 
                                              tests = c("centroids", "equal", "institutions", "zeroes"), 
                                              inst_rad = 100, 
                                              outliers_method = "quantile")  # but watch out for wrongly interpreted "outliers" with e.g. scattered / coastal species!

head(occurrences_coordcleaner) # the function added new columns with flagged records from each test, and overall '.summary'


# map again:

leaflet() %>%
  addTiles() %>%
  # add occurrence points (circles), defining which TABLE and COLUMNS contain their longitude and latitude coordinates:
  addCircles(data = occurrences_raw, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "red") %>%
  addCircles(data = occurrences_clean, lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "blue") %>%
  addCircles(data = subset(occurrences_coordcleaner, .summary == TRUE), lng = ~ decimalLongitude, lat = ~ decimalLatitude, label = ~ institutionCode, col = "turquoise")


# if you're happy with the CoordinateCleaner results (but DO INSPECT THEM CAREFULLY!), you can remove all flagged records (otherwise, rerun 'clean_coordinates' with your own preferred options):
nrow(occurrences_clean)
occurrences_clean <- occurrences_clean[occurrences_coordcleaner$.summary == TRUE, ]
nrow(occurrences_clean)


# REMOVE RECORDS OUTSIDE YOUR REGION OF INTEREST ####

# this is not necessarily necessary, and not necessarily "error" cleaning, but you may want to keep only points that fall within particular regions in a map
# first, get a map of the region you want to restrict you records to, or exclude your records from (e.g. by subsetting our world countries map as below)
head(countries)
myregion <- subset(countries, countries$NAME_0 %in% c("Portugal", "Spain"))
head(myregion)
plot(myregion)

# convert the occurrences dataframe to a spatial object and specify its CRS (so that you can spatially overlap the points to the region of interest):
occurrences_spatial <- vect(occurrences_clean, geom = c("decimalLongitude", "decimalLatitude"), crs = "epsg:4326")  # because we KNOW this is the CRS of GBIF coordinates

# select the occurrences that overlap 'myregion':
occurrences_myregion <- occurrences_spatial[myregion, ]
nrow(occurrences_spatial)
nrow(occurrences_myregion)

# you can instead delete the records that overlap either 'myregion' or the entire countries map (e.g. if you want to delete records from particular countries, or select only marine records):
occurrences_sea <- erase(occurrences_spatial, countries)

# always map each result!
plot(myregion)
plot(occurrences_spatial, col = "red", add = TRUE)
plot(occurrences_myregion, col = "blue", add = TRUE)
plot(occurrences_sea, col = "seagreen", add = TRUE)

plot(occurrences_sea, col = "seagreen")
plot(countries, add = TRUE)
# not that this may target terrestrial records that were just slightly displaced, due to spatial error or to inaccurate coast line! always inspect your data

# convert back to regular (non-spatial) dataframe:
occurrences_myregion <- data.frame(occurrences_myregion)
head(occurrences_myregion)


# save the cleaned data to disk as a .csv file (use 'occurrences_myregion' instead of 'occurrences_clean' if you want to export the spatially delimited data above):
write.csv(occurrences_clean, paste0("./outputs/occurrences_", my_species, "_cleaned.csv"), row.names = FALSE)
