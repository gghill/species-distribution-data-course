# install recent versions of R and RStudio, open RStudio and run the commands below, one at a time

# if you get an error message about Rtools, follow the instructions in that message -- it may involve installing the Rtools program (from the link provided in the error message) outside R, and adding it to the path following the instructions in that website

# every time you get a message about a missing or irremovable package, install that package and try again

# after installing all packages, load each package with library() and check if there are additional error messages, in which case do as above


# day 1
install.packages("spocc", repos = "https://dev.ropensci.org")
install.packages("raster", repos = "https://rspatial.r-universe.dev")
install.packages("terra", repos = "https://rspatial.r-universe.dev")
install.packages("geodata", repos = "https://rspatial.r-universe.dev")
install.packages("leaflet")
install.packages("neotoma")
install.packages("data.table")

# day 2
install.packages("scrubr", repos = "https://dev.ropensci.org")
# if the previous command fails, try devtools::install_github("ropensci/scrubr")
install.packages("CoordinateCleaner")
install.packages("spThin")
devtools::install_github("jasonleebrown/humboldt")
install.packages("fuzzySim", repos = "http://R-Forge.R-project.org")
