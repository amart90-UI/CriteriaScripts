# Create dataframe from UI
score <- data.frame(ui@data)

# Weight data?

# Sum scores, relative them, and rank them
score$Total <- as.numeric(apply(score[, grepl("score", names(score))], 1, FUN=sum))
score$RelativeTotal <- round((score$Total / sum(score$Total)) * 100, 2)
score$Rank <- rank(-(score$Total))

# Add to SpatialPolygonsDataFrame
ui@data$Total <- score$Total
ui@data$RelativeTotal <- score$RelativeTotal
ui@data$Rank <- score$Rank

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UIs and color by rank
plot(fire.perim, main="Unburned Islands Importance rank")
col1 <- colorRampPalette(c("red", "orange", "yellow", "green"))
col2 <- col1(length(score$Rank))
col3 <- col2[rank(score$Rank, ties.method = "min")]
plot(ui, col=col3, border=col3, add=T)
legend("topleft", title= "Unburned Island Importance", legend = c("Highly important", "", "", "Less important"), 
       fill = c("red", "orange", "yellow", "green"), cex = 0.9)

# Plot top 10%
plot(fire.perim, main="Most important (10%)")
plot(ui[ui$Rank < round(length(ui@data$Total)/10),], col="red", border="red", add=T)

# Plot score distribution
hist(score$Total, main="Distribution of Final Scores", xlab="Importance score")

# Plot distributions in 2x3 grid
# Setup plot layout
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))

# Plot histograms
hist(ui@data$score.isol, main="Isolation", xlab="Isolation score")
hist(ui@data$score.habitat, main="Critical Habitat", xlab="Critical Habitat score", ylab="", 
     breaks=seq(from=0, to=1, by=0.1))
hist(ui@data$score.invasive, main="Invasive Species", xlab="Invasive Species score", ylab="")
hist(ui@data$score.RelAbundance, main="Cover TypeAbundance", xlab="Relative Abundance score", 
     breaks=seq(from=0, to=1, by=0.1))
hist(ui@data$score.building, main="Buildings", xlab="Building score", ylab="", 
     breaks=seq(from=0, to=1, by=0.1))
hist(ui@data$score.age, main="Stand Age", xlab="Stand age score", ylab="", 
     breaks=seq(from=0, to=1, by=0.1))


# Write to file
writeOGR(obj = ui, dsn= paste0(getwd(), "/Output"), layer = "UI.shp", driver = "ESRI Shapefile", overwrite_layer = T)
write.csv(score, file="Output/RefugiaRanking.csv", row.names=F)

# Cleanup intermediates
rm(score, col1, col2, col3)
