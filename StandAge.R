# Load Stand age raster
age <- raster("Datasets/Stand Age/StandAge.clip.tif")

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

# Plot stand age 
dev.set(which = map.matrix)
plot(fire.perim, main= "Stand Age")
id <- as.numeric(as.character(ui@data$ID))
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(ui.age)))
col3 <- cbind(as.numeric(as.character(ui@data$ID)), ui.age)
col4 <- cbind(sort(unique(ui.age)), col2)
col5 <- merge(col3, col4, by.x="V2", by.y="V1")
col5$col2 <- as.character(col5$col2)
col6 <- col5[order(col5$V1),]
col7 <- as.character(col6$col2)
plot(ui, add=T, col=col7, border=col7)
legend("topleft", title= "Mean Stand Age (yrs)", legend = c(min(ui.age, na.rm = T), "", "", max(ui.age, na.rm=T)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)
dev.set(which = 2)

# Plot score distribution
hist(ui.age, main="Distribution of Scores", xlab="Stand age")

# Cleanup intermediates
rm(resil, resil.ui, col1, col2, col3, col4, col5, col6, col7, fire.proj, ui.proj, id, ui.resil)
