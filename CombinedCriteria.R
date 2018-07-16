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
library(raster)

# Load files
pnw <- readOGR("PNW/pnw.shp")
fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp")
ui <- readOGR("UnburnedIsland/unburned_areas.shp")

# Build blank score data frame
scores.df <- data.frame(ID = ui@data$ID)


#=======================================================================#
# Stand age
#=======================================================================#

# load data
age <- raster("StandAge/StandAge.clip.tif")

# Transform projection
fire.proj <- spTransform(fire.perim, projection(age))
ui.proj <- spTransform(ui, projection(age))

# Clip to fire extent
age <- crop(age, extent(fire.proj))

# Mask stand age to UIs
age.ui <- mask(age, ui.proj)

# Extract mean stand age values for each UI
ui.age <- extract(age.ui, ui.proj, small=T, fun=mean, na.rm=T)
ui.age[is.na(ui.age)] <- 0

# Write stand age to UI and df
scores.df$score.age <- as.numeric(ui.age)
ui@data$score.age <- as.numeric(ui.age)
#writeOGR(ui, dsn= "StandAge", layer = "StandAge", driver = "ESRI Shapefile", overwrite_layer = T)
writeRaster(age.ui, "StandAge.tif")


#=======================================================================#
# Size
#=======================================================================#

# Define Size function (0.03 sec)
Size <- function(ui){
  # Calculate size of unburned islands
  size.ui <- gArea(ui, byid = T)
}

#=======================================================================#
# Isolation
#=======================================================================#

# Distance to the nearest live forest edge (2.50 mins)
Isolation <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(rgeos)
  require()
  require(matrixStats)
  
  # Calculate distance to fire perimeter for each UI
  s.dist.u <- as.matrix(gDistance(ui, byid=T))
  diag(s.dist.u) <- NA
  s.min.u <- rowMins(s.dist.u, na.rm = T)
  
  
  # Calculate distance to nearest UI for each UI
  s.min.p <- as.numeric(gDistance(ui, as(fire.perim, "SpatialLines"), byid=T))
  max(s.min.u)
  # Calculate distance to nearest live tree edge
  s.min <- as.matrix(cbind(s.min.u, s.min.p))
  s.min <- rowMins(as.matrix(s.min))
  
  return(s.min)
}




#=======================================================================#
# Seedling
#=======================================================================#

# Define Seedling function (7.00 min)
Seedling <- function(ui, fire.perim) {
  # Load packages
  require(dismo)
  require(raster)
  require(rgdal)
  require(rgeos)
  
  # Create voronoi polygons
  cen <- gCentroid(ui, byid=T)
  ext <- extent(ui)
  ui.alloc <- voronoi(cen, ext = c(ext[1], ext[2], ext[3], ext[4]))
  ui.alloc <- crop(ui.alloc, fire.perim)
  ui.alloc@data$ID <- ui@data$ID
  
  # Create blank raster
  mround <- function(x,base,method){ 
    if(method == "min") {
      return(base*floor(x/base))
    } else if(method == "max"){
      return(base*ceiling(x/base))
    } else {
      stop("Unrecognized method: choose 'min' or 'max'.")
    }
  }
  
  xmin <- mround(xmin(fire.perim), 30, "min")
  xmax <- mround(xmax(fire.perim), 30, "max")
  ymin <- mround(ymin(fire.perim), 30, "min")
  ymax <- mround(ymax(fire.perim), 30, "max")
  r1 <- (xmax - xmin) / 30
  r2 <- (ymax - ymin) / 30
  blank <- raster(resolution = 30, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, crs = projection(fire.perim), vals = 1)
  blank <- mask(blank, fire.perim)
  
  # Rasterize UI and perimeter
  ui.r <- rasterize(ui, blank, field=2)
  fire.line <- as(fire.perim, 'SpatialLines')
  perim.r <- rasterize(fire.line, blank, field=2)
  
  # Distance to UI
  ui.r.2 <- sum(ui.r, perim.r, na.rm = T)
  ui.r.2[ui.r.2 >= 2] <- 2
  ui.r.2[ui.r.2 == 0] <- NA
  dist <- distance(ui.r.2)
  dist <- mask(dist, fire.perim)
  
  # Calculate probability of seedling presence
  seed <- (-1 / (1 + 35 * exp(-0.016 * dist))) + 1
  
  # Assign seedling presence probability to UI
  seed.sum <- extract(seed, ui.alloc, method = 'simple', small = T, fun = sum, na.rm = T, df = T, sp = T)
  return(seed.sum@data$layer)
}


