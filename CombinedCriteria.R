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

# Fix UI - resave it
ui <- spTransform(ui, proj4string(fire.perim))
ui <- gBuffer(ui, byid=T, width=0)
ui <- ui[order(match(as.numeric(as.character(ui@data$ID)),sort(as.numeric(as.character(ui@data$ID))))),]
ui@data <- ui@data[, -2]
writeOGR(ui, dsn= "UnburnedIsland", layer = "unburned_areas", driver = "ESRI Shapefile", overwrite_layer = T)


scores.df <- data.frame(ID = ui@data$ID)


#=======================================================================#
# Stand age
#=======================================================================#
age <- raster("Datasets/Stand Age/StandAge.clip.tif")


