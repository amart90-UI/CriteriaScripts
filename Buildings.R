# Load buildings
usfs.bldg <- readOGR("Datasets/Buildings/USFS.clip.shp")
nps.bldg <- readOGR("Datasets/Buildings/NPS.clip.shp")
comm <- readOGR("Datasets/Buildings/CommTwr.clip.shp")

# Transform projections
fire.proj <- spTransform(fire.perim, projection(usfs.bldg))
ui.proj <- spTransform(ui, projection(usfs.bldg))

# Test if UI contains a building
usfs.ui <- gIntersects(usfs.bldg, ui.proj, byid=T)
nps.ui <- gIntersects(nps.bldg, ui.proj, byid=T)
comm.ui <- gIntersects(comm, ui.proj, byid=T)

# Sum number of buildings
usfs.sum <- rowSums(usfs.ui)
nps.sum <- rowSums(nps.ui)
comm.sum <- rowSums(comm.ui)
bldg.sums <- usfs.sum + nps.sum

# Assign score to DF and UI
scores.df$score.building <- bldg.sums
scores.df$score.comm <- comm.sum
ui@data$score.building <- bldg.sums
ui@data$score.comm <- comm.sum

# Plot UIs and Buildings
usfs.proj <- spTransform(usfs.bldg, projection(fire.perim))
nps.proj <- spTransform(nps.bldg, projection(fire.perim))
comm.proj <- spTransform(comm, projection(fire.perim))
plot(fire.perim, main = "Buildings - Values at Risk")
plot(ui, col=((scores.df$score.building + scores.df$score.comm) > 0)*4, border=((scores.df$score.building + scores.df$score.comm) > 0)*4, add=T)
plot(fire.perim, add=T)
plot(usfs.proj, pch=18, col="green", add=T)
plot(nps.proj, col="pink", pch=18, add=T)
plot(comm.proj, col="orange", pch=18, add=T)
legend("topleft", 
       legend = c("USFS buildings", "NPS buildings", "Comm. towers", "UI with buildings"), 
       fill = c("green", "pink", "orange", 4), cex = 0.9)

# Plot UI importance by buildings
dev.set(which = map.matrix)
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(scores.df$score.building))
col3 <- col2[rank(scores.df$score.building, ties.method = "min")]
plot(fire.perim, main="Infrastructure: Buildings")
plot(ui, add=T, col=col3, border=col3)


# Plot UI importance by communication tower
col2 <- col1(length(scores.df$score.comm))
col3 <- col2[rank(scores.df$score.comm, ties.method = "min")]
plot(fire.perim, main="Infrastructure: Comm. Towers")
plot(ui, add=T, col=col3, border=col3)
dev.set(which = 2)

# Plot score distribution
hist(ui@data$score.building, main="Distribution of Scores", 
     xlab="Building score", cex.axis=0.8, cex=0.9)

# Cleanup intermediates
rm(usfs.bldg, nps.bldg, comm, fire.proj, ui.proj, usfs.ui, nps.ui, comm.ui, 
   usfs.sum, nps.sum, comm.sum, bldg.sums, nps.proj, usfs.proj, comm.proj, col1, col2, col3)
