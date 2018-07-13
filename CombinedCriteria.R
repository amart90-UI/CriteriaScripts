#=======================================================================#
#=======================================================================#
#                   Criteria for Ranking Fire Refugia                   #
#                                                                       #
#                            Anthony Martinez                           #
#=======================================================================#
#=======================================================================#




#=======================================================================#
# Startup
#=======================================================================#

# Set working directory
setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")

# Load libraries
library(rgdal)
library(rgeos)

# Load files
pnw <- readOGR("PNW/pnw.shp")
fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp")
ui <- readOGR("UnburnedIsland/unburned_areas.shp")

scores.df <- data.frame(ID = ui@data$ID)


#=======================================================================#
# Stand age
#=======================================================================#
age <- raster("Datasets/Stand Age/StandAge.clip.tif")


