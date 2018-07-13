# Calculate size of unburned islands
size.ui <- gArea(ui, byid = T)

# Write size to ui and df
# Write stand age to UI and df
scores.df$score.size <- size.ui
ui@data$score.size <- size.ui

# Plot area 
dev.set(which = map.matrix)
plot(fire.perim, main= "Area")
id <- as.numeric(as.character(ui@data$ID))
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(size.ui)))
col3 <- cbind(id, size.ui)
col4 <- cbind(sort(unique(size.ui)), col2)
col5 <- merge(col3, col4, by.x="size.ui", by.y="V1")
col5$col2 <- as.character(col5$col2)
col6 <- col5[order(col5$id),]
col7 <- as.character(col6$col2)
plot(ui, add=T, col=col7, border=col7)
#legend("topleft", title= "UI Area (m^2)", legend = c(min(size.ui, na.rm = T), "", "", max(size.ui, na.rm=T)), 
#       fill = c("green", "yellow", "orange", "red"), cex = 0.9)
dev.set(which = 2)

# Plot size distribution
hist(size.ui, main="Distribution of Scores", xlab="Area (m^2)")
