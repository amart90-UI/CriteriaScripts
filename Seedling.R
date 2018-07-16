# Create Thiessen polygons
ui.centroid <- gCentroid(ui, byid=T)
ui.cords.x <- ui.centroid@coords[,1]
ui.cords.y <- ui.centroid@coords[,2]
ext <- extent(ui)
ui.alloc <- voronoi(ui.centroid, ext = c(ext[1], ext[2], ext[3], ext[4]))
ui.alloc <- crop(ui.alloc, fire.perim)
ui.alloc@data$ID <- ui@data$ID

# Create blank raster
x1 <- round_any(xmin(fire.perim), 30, f = floor)
x2 <- round_any(xmax(fire.perim), 30, f = ceiling)
y1 <- round_any(ymin(fire.perim), 30, f = floor)
y2 <- round_any(ymax(fire.perim), 30, f = ceiling)
r1 <- (x2 - x1) / 30
r2 <- (y2 - y1) / 30
blank <- raster(resolution = 30, xmn = x1, xmx = x2, ymn = y1, ymx = y2, crs = projection(fire.perim), vals = rnorm(r1*r2))
values(blank) <- rep(1, times = length(blank))
blank <- mask(blank, fire.perim)

# Rasterize UI and perimeter
ui.r <- rasterize(ui, blank, field=2)
fire.line <- as(fire.perim, 'SpatialLines')
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

# Add score
scores.df$score.seeding <- seed.sum@data$layer
ui@data$score.seeding <- seed.sum@data$layer

# Plot Seedling success
dev.set(which = map.matrix)
plot(fire.perim, main= "Seedling")
id <- as.numeric(as.character(ui@data$score.seeding))
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(ui@data$score.seeding)))
col3 <- cbind(as.numeric(as.character(ui@data$ID)), ui@data$score.seeding)
col4 <- cbind(sort(unique(ui@data$score.seeding)), col2)
col5 <- merge(col3, col4, by.x="V2", by.y="V1")
col5$col2 <- as.character(col5$col2)
col6 <- col5[order(col5$V1),]
col7 <- as.character(col6$col2)
plot(ui, add=T, col=col7, border=col7)
#legend("topleft", title= "Mean Stand Age (yrs)", 
#       legend = c(round(min(scores.df$score.seeding, na.rm = T), 2), "", "", 
#                  round(max(scores.df$score.seeding, na.rm=T), 2)), 
#       fill = c("green", "yellow", "orange", "red"), cex = 0.9)
dev.set(which = 2)

# Remove intermediates
rm(ui.centroid, ui.cords.x, ui.cords.y, ui.alloc, ext, x1, x2, y1, y2, r1, r2, blank, ui.r, fire.line, 
   perim.r, ui.r.2, dist, seed, seed.sum)


writeOGR(obj = ui, dsn= paste0(getwd(), "/Output"), layer = "UI", driver = "ESRI Shapefile", overwrite_layer = T)
