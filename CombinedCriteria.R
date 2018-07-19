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
writeRaster(age.ui, "StandAge.tif")
age.ui <- raster("StandAge.tif")

# Extract mean stand age values for each UI
ui.age <- extract(age.ui, ui.proj, small=T, fun=mean, na.rm=T)
ui.age[is.na(ui.age)] <- 0

# Extract mean stand age values for each UI
ui.age <- extract(age.ui, ui.proj, small=T, fun=mean, na.rm=T)
ui.age[is.na(ui.age)] <- 0

# Write stand age to UI and df
scores.df$score.age <- as.numeric(ui.age)
ui@data$score.age <- as.numeric(ui.age)
#writeOGR(ui, dsn= "StandAge", layer = "StandAge", driver = "ESRI Shapefile", overwrite_layer = T)
writeRaster(age.ui, "StandAge.tif")
age.ui <- raster("StandAge.tif")

################

# Define function to calculate mean stand age
StandAge <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(raster)
  
  # Load data
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
  
  return(as.numeric(ui.age))
}


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
  blank <- raster(resolution = 30, xmn = xmin, xmx = xmax, ymn = ymin, 
                  ymx = ymax, crs = projection(fire.perim), vals = 1)
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
  seed.sum <- extract(seed, ui.alloc, method = 'simple', small = T, 
                      fun = sum, na.rm = T, df = T, sp = T)
  return(seed.sum@data$layer)
}



#=======================================================================#
# Infrastructure
#=======================================================================#

# Number of federal structures (1 min)
Infrastructure <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(rgeos)
  require(matrixStats)
  
  # Load buildings
  fcc <- readOGR("Infrastructure/Intermediates/FCC_Point.shp")
  nps <- readOGR("Infrastructure/Intermediates/NPS_Point.shp")
  usfs <- readOGR("Infrastructure/Intermediates/USFS_Point.shp")
  blm.p <- readOGR("Infrastructure/Intermediates/BLM_Point.shp")
  blm.l <- readOGR("Infrastructure/Intermediates/BLM_Line.shp")
  
  # Test if UI contains a building
  fcc.ui <- rowSums(gIntersects(fcc, ui, byid=T))
  nps.ui <- rowSums(gIntersects(nps, ui, byid=T))
  usfs.ui <- rowSums(gIntersects(usfs, ui, byid=T))
  blm.ui <- rowSums(gIntersects(blm.p, ui, byid=T)) + 
    rowSums(gIntersects(blm.l, ui, byid=T))
  infra.ui <- fcc.ui + nps.ui + usfs.ui + blm.ui
  
  return(infra.ui)
}


#=======================================================================#
# Critical Habitat
#=======================================================================#

# Critical habitat function (1.04 min)
CritHabitat <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(raster)
  require(rgeos)
  
  # Load Critical Habitat data
  hab <- readOGR("CriticalHabitat/CriticalHabitat.shp")
  
  # Identify UIs with Critical Habitat
  int <- intersect(hab, ui) # 59 sec
  
  # Add score based upon type of species
  h.score <- data.frame(listing_st= c("Endangered", "Threatened", 
                                      "Recovery", "Proposed Endangered"),
                        score.habitat= c(1,.5,.2,.2))
  hab.ui <- merge(int@data[, c("ID", "listing_st")], h.score, by= "listing_st")
  hab.ui <- merge(ui@data, hab.ui, by="ID", all=T)$score.habitat
  hab.ui[is.na(hab.ui)] <- 0
  return(hab.ui)
}


#=======================================================================#
# Invasive plant species cover
#=======================================================================#

# Calculate proportion of invasive species cover
Invasive <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(rgeos)
  require(raster)
  
  # Load invasive species data
  inv <- readOGR("Invasive/Invasive.shp")
  
  # Clip data to fire perimeter
  inv <- crop(inv, fire.perim)
  
  # Find area of total invasive species range (considering multiple species) within UI
  inv <- gIntersection(ui, inv, byid = T)
  ui.over <- over(inv, ui, returnList = F)
  inv.area <- sapply(slot(inv, "polygons"), function(i) slot(i, "area"))
  inv.df <- data.frame(ID = ui.over$ID,
                       inv.area = inv.area)
  inv.df <- aggregate(inv.area ~ ID, data = inv.df, FUN = sum)
  
  # Calculate proportion of each UI that is covered by invasive species
  inv.df$ui.area <- sapply(slot(ui[ui@data$ID %in% inv.df$ID,], "polygons"), 
                           function(i) slot(i, "area"))
  inv.df$prop.inv <- inv.df$inv.area / inv.df$ui.area
  inv.df <- merge(data.frame(ID = ui@data$ID), inv.df[, c(1,4)], all = T)
  inv.df$prop.inv[is.na(inv.df$prop.inv)] <- 0
  return(inv.df$prop.inv)
}

#=======================================================================#
# Land cover relative abundance
#=======================================================================#

# Define land cover realative abundance function (3.78 mins)
LandCover <- function(ui, fire.perim){
  # Load packages
  require(rgdal)
  require(raster)
  
  # Identify state and load land cover data
  pnw <- readOGR("PNW/pnw.shp")
  state <- data.frame(intersect(pnw, fire.perim))
  state <- toupper(substr(state$NAME_1, start = 1, stop = 2))
  lcover.list <- paste0("lcover.", state)
  for(i in 1:length(state)) {
    assign(paste0(lcover.list[i]), raster(
      paste0("LandCover/gaplandcov_", tolower(state[i]), "/gaplndcov_", state[i], ".img")))
  }
  
  # Transform projections
  fire.proj <- spTransform(fire.perim, projection(get(lcover.list[1])))
  ui.proj <- spTransform(ui, projection(get(lcover.list[1])))
  
  # Clip land cover data to fire perimeter
  clip.list <- paste0("lcover.", state, ".clip")
  for(i in 1:length(state)) {
    assign(clip.list[i], crop(get(lcover.list[i]), extent(fire.proj)))
    assign(clip.list[i], round(mask(get(paste0(clip.list[i])), fire.proj)))
  }
  if(length(state) > 1){
    assign("lcover.fire", merge(get0(clip.list[1]), get0(clip.list[2]), get0(clip.list[3])))
  } else {
    assign("lcover.fire", get(clip.list))
  }
  
  # Get all land cover IDs present in UIs
  lcover.ui <- mask(lcover.fire, ui.proj)
  lcover.freq <- as.data.frame(freq(lcover.ui))
  lcover.freq <- lcover.freq[!is.na(lcover.freq$value), ]
  lcover.sort <- lcover.freq[order(lcover.freq$count), ]
  
  # Find mode land cover ID for each UI
  #   Create mode (central tendency) function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #   Extract raster values by polgon (UI) and calculate mode
  r.mode <- extract(lcover.ui, ui.proj)
  r.mode <- lapply(r.mode, function(x) x[!is.na(x)])
  r.mode <-  lapply(r.mode, FUN=Mode)
  r.mode <- as.numeric(r.mode)
  
  # Calculate relative abundance by area of landcover type
  #   Assign mode land cover value to each UI polygon
  r.uniq <- unique(r.mode)
  r.area <- sapply(r.uniq, function(x) sum(values(lcover.ui) == x, na.rm = T))
  r.rel <- r.area/sum(r.area)
  r.abun <- data.frame(lcover.mode=r.uniq, r.rel=r.rel)
  
  # Assign score to each UI based on mode landcover type
  ui.list <- data.frame(ID = ui@data$ID, r.mode)
  ui.list <- merge(ui.list, r.abun, by.x="r.mode", by.y="lcover.mode")
  ui.list <- ui.list[order(ui.list$ID), ]
  
  return(ui.list$r.rel)
}


#=======================================================================#
# Table Mountain fire
#=======================================================================#
require(rgdal)
setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")
fire.perim <- readOGR("UnburnedIsland/Table_perim.shp")
ui <- readOGR("UnburnedIsland/Table_ui.shp")

Times <- data.frame(Function = NULL, Time = NULL)

s <- Sys.time()
scores.df$CritHab <- CrtiHabitat(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "CrtiHabitat", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$Infrstr <- Infrastructure(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "Infrastructure", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$Invasiv <- Invasive(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "Invasive", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$Isolatn <- Isolation(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "Isolation", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$LandCvr <- LandCover(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "LandCover", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$Seed <- Seedling(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "Seedling", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$Size <- Size(ui)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "Size", Time = difftime(e, s, units='mins')))
Times$Time

s <- Sys.time()
scores.df$StndAge <- StandAge(ui, fire.perim)
e <- Sys.time()
Times <- rbind(Times, data.frame(Function = "StandAge", Time = difftime(e, s, units='mins')))
Times$Time

Times$Time <- round(Times$Time, 2)
Times
sum(Times$Time)

scores.df$StandAge <- as.numeric(scores.df$StandAge)
Table <- ui
Table@data <- merge(Table@data, scores.df, by = "ID")
names(Table@data)[4:11] <- c("CritHab", "Infrstr", "Invasiv", "Isolatn", "LandCvr", "Seed", "StndAge", "Size")
writeOGR(Table, dsn = "Output", layer = "TableMtn", driver = "ESRI Shapefile", overwrite_layer = T)

### End ###
summary(scores.df$Isolation)
summary(scores.df$Isolation)
