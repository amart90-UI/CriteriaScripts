# Calculate distance to fire perimeter for each UI
s.dist.u <- as.matrix(gDistance(ui, byid=T))
diag(s.dist.u) <- NA
s.min.u <- rowMins(s.dist.u, na.rm = T)

# Calculate distance to nearest UI for each UI
s.min.p <- as.numeric(gDistance(ui, as(fire.perim, "SpatialLines"), byid=T))

# Calculate distance to nearest live tree edge
s.min <- as.matrix(cbind(s.min.u, s.min.p))
s.min <- rowMins(as.matrix(s.min))

# Write to UI and DF
scores.df$score.isol <- s.min
ui@data$score.isol <- s.min

# Plot relative isolation
plot(fire.perim, main="Relative Isolation")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(scores.df$score.isol))
col3 <- col2[rank(scores.df$score.isol, ties.method = "min")]
plot(ui, add=T, col=col3, border=col3)
legend("topleft", title= "Distance to intact forest (m)", legend = c(round(min(s.min),1), "", "", round(max(s.min),1)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot score distribution
hist(scores.df$score.isol, main="Distribution of Scores", xlab="Isolation score")

# Cleanup intermediates
rm(s.dist.u, s.min.u, s.min.p, s.min, col1, col2, col3)


####
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

# Rasterize UI and fore perimeter
fire.line <- as(fire.perim, 'SpatialLines')
perim.r <- rasterize(fire.line, blank, field=1)
ui.r <- rasterize(ui, blank, field = "ID")
edge.r <- sum(perim.r, ui.r, na.rm = T)
edge.r[edge.r == 0] <- NA

# Calculate distance
dist <- distance(edge.r)

plot(dist)
dist <- distance()
hist(edge.r)
gDistance()