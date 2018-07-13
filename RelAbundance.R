# Identify state and load land cover data
state <- data.frame(intersect(pnw, fire.perim))
state <- state$NAME_1
state <- toupper(substr(state, start = 1, stop = 2))
lcover.list <- paste0("lcover.", state)
for(i in 1:length(state)) {
  assign(paste0(lcover.list[i]), raster(
    paste0("Datasets/Land Cover/gaplandcov_", tolower(state[i]), "/gaplndcov_", state[i], ".img")))}

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
  assign("lcover.fire", merge(get0(clip.list[1]), get0(clip.list[2]), get0(clip.list[3])))} 
if(length(state) == 1){
  assign("lcover.fire", get(clip.list))}

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
r.vals <- extract(lcover.ui, ui.proj)
r.vals <- lapply(r.vals, function(x) x[!is.na(x)])
r.mode <-  lapply(r.vals, FUN=Mode)
r.mode <- as.numeric(r.mode)

# Calculate relative abundance by area of landcover type
#   Assign mode land cover value to each UI polygon
r.uniq <- unique(r.mode)
r.area <- sapply(r.uniq, function(x) sum(values(lcover.ui) == x, na.rm = T))
r.rel <- r.area/sum(r.area)
r.abun <- data.frame(lcover.mode=r.uniq, r.rel=r.rel)

# Assign score to each UI based on mode landcover type
ui.list <- data.frame(r.mode, ID = ui@data$ID)
ui.list <- merge(ui.list, r.abun, by.x="r.mode", by.y="lcover.mode")
ui.list <- ui.list[order(ui.list$ID), ]

# Write to DF and UI
scores.df$score.RelAbundance <- ui.list$r.rel
ui@data$score.RelAbundance <- ui.list$r.rel

# Plot Landcover types and UI
dev.set(which = map.matrix)
plot(fire.perim, main= "Rel. Abundance of Cover Type")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(ui.list$r.rel)))
col3 <- cbind(unique(ui.list$r.rel), col2)
col4 <- merge(ui.list, col3, by.x= "r.rel", by.y = "V1")
col4 <- col4[order(col4$ID),]
col5 <- as.character(col4$col2)
plot(ui, add=T, col=col5, border=col5)
#legend("topleft", title= "Proportion of cover type", 
#       legend = c(paste0("High (", round(100*max(r.rel)), "%)"), "", "", paste0("Low (", round(100*min(r.rel)), "%)")), 
#       fill = c("green", "yellow", "orange", "red"), cex = 0.9)
dev.set(which = 2)

# Plot score distribution
hist(ui@data$score.RelAbundance, main="Distribution of Scores", xlab = "Relative Abundance score", breaks=seq(from=0, to=1, by=0.1))
hist(ui.list$r.mode, main="Distribution of Land Cover Types", xlab = "Land Cover ID")

# Plot landcover types - turned off (#)
#plot(lcover.fire, breaks = unique(lcover.fire), col= rainbow(24), 
#     legend=F, axes=F, box=F, main="Landcover Types")

#Cleanup intermediates
rm(fire.proj, lcover.fire, lcover.ui, lcover.freq, lcover.sort, Mode,
   r.vals, r.mode, r.uniq, r.area, r.rel, r.abun, ui.list, ui.proj, col1, col2, col3, col4, 
   col5, score.abun, list = c(ls(pattern = "lcover*"), ls(pattern = "clip*")), i, state)
