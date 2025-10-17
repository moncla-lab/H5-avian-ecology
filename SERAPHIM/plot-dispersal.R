# Installations
install.packages("devtools"); library(devtools)
install_github("sdellicour/seraphim/unix_OS") # for Unix systems
install_github("sdellicour/seraphim/windows") # for Windows systems
install.packages("diagram")

# Set directory
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/nov2021-2024/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024_no-suliformes/")
setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024_no-suliformes_contphyl-orderDTA/")
getwd()

# Load packages
library(seraphim)
library(diagram)
library(geodata)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(terra)
library(dplyr)

# Step 1 - extract spatio-temporal information from posterior distribution of trees
localTreesDirectory = "./Extracted_trees"
nberOfTreesToSample = 96
mostRecentSamplingDatum = 2024.9262295081967
#trees = readAnnotatedNexus("NAm_wild-ave_2022tree_ha_contphyl_100combined.trees")
#trees = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_ha_contphyl_2025-07-10_combined_100.trees")
trees = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_no-suliformes_ha_contphyl_combined_2025-08-18_96trees.trees")
dir.create(localTreesDirectory, showWarnings=F)
for (i in 1:length(trees))
{
  tab = postTreeExtractions(post_tre=trees[[i]], mostRecentSamplingDatum)
  write.csv(tab, paste0(localTreesDirectory,"/TreeExtractions_",i,".csv"), row.names=F, quote=F)
}

# Step 2 - extract spatio-temporal information from MCC tree
#mcc_tre = readAnnotatedNexus("NAm_wild-aves_2022tree_ha_contphyl_MCC_2025-02-05.tree")
#mcc_tre = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_ha_contphyl_2025-07-10_MCC.tree")
#mcc_tre = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_no-suliformes_ha_contphyl_2025-08-18_MCC.tree")
mcc_tre = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_contphyl_order-DTA_2025-09-24_MCC.tree")
mcc_tab = mccTreeExtractions(mcc_tre, mostRecentSamplingDatum)
#write.csv(mcc_tab, "NAm_wild-aves_nov2021-2024_ha_MCC.csv", row.names=F, quote=F)
#write.csv(mcc_tab, "NAm_wild-aves_nov2021-2024_no-suliformes_ha_MCC.csv", row.names=F, quote=F)
write.csv(mcc_tab, "NAm_wild-aves_nov2021-2024_contphyl_order-DTA_MCC_seraph.csv", row.names=F, quote=F)
#mcc_tab = read.csv("NAm_wild-aves_nov2021-2024_ha_MCC.csv", head=T)
#mcc_tab = read.csv("NAm_wild-aves_nov2021-2024_no-suliformes_ha_MCC.csv", head=T)
mcc_tab = read.csv("NAm_wild-aves_nov2021-2024_contphyl_order-DTA_MCC_seraph.csv", head=T)


# Step 3 - estimate HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
#startDatum = min(mcc_tab[,"startYear"])
startDatum = min(mcc_2021_2022pt1[,"startYear"])
polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))

# Step 4 - defining a color scale
colour_scale = colorRampPalette(brewer.pal(11,"RdYlGn"))(141)[21:121]
#minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
minYear = min(mcc_2021_2022pt1[,"startYear"]); maxYear = max(mcc_2021_2022pt1[,"endYear"])
#endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
endYears_indices = (((mcc_2021_2022pt1[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))
for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"40")
}

# Step 5 - plot HPD regions and MCC tree
north_america <- ne_countries(continent = "north america", returnclass = "sf")
north_america_vect <- vect(north_america)
r_template <- rast(ext(north_america_vect), resolution = 0.3) # To use the full raster, use this line of code
#custom_ext <- ext(-171, -45, 7, 75) # if you want to make custom dimensions in the raster, use this code
#r_template <- rast(custom_ext, resolution = 0.3) # This code implements any custome dimensions for your raster
template_raster <- rasterize(north_america_vect, r_template, field = 1)

countries = c("USA", "Canada", "Mexico") #, "Guatemala", "Belize", "Honduras", "El Salvador", "Costa Rica", "Panama"

#borders = crop(gadm(country = "USA", level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/plot-dispersal/"), extent(r_rasterized))
#borders = gadm(country = "USA", level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/plot-dispersal/")
#borders = gadm(country = "USA", level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/nov2021-2024/")
borders = gadm(country = countries, level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024/")

dev.new(width=6, height=6.3) # This line of code opens a new window for the figure
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o") # This line of code sets up dimensions for the window?
plot(template_raster, col="grey", box=F, axes=F, colNA="lightblue", legend=F) # This plots template_raster, removes the box and axes, and colors land grey and water blue

for (i in 1:length(polygons))
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
plot(borders, add=T, lwd=0.1, border="gray10")

#for (i in 1:dim(mcc_tab)[1])
#{
#  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
#              arr.width=0, lwd=0.2, lty=1, lcol="gray10", arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
#}
for (i in 1:dim(mcc_2021_2022pt1)[1])
{
  curvedarrow(cbind(mcc_2021_2022pt1[i,"startLon"],mcc_2021_2022pt1[i,"startLat"]), cbind(mcc_2021_2022pt1[i,"endLon"],mcc_2021_2022pt1[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=0.2, lty=1, lcol="gray10", arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
}

#for (i in dim(mcc_tab)[1]:1)
#{
#  if (i == 1)
#  {
#    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=0.8)
#    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=0.8)
#  }
#  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=0.8)
#  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=0.8)
#}
for (i in dim(mcc_2021_2022pt1)[1]:1)
{
  if (i == 1)
  {
    points(mcc_2021_2022pt1[i,"startLon"], mcc_2021_2022pt1[i,"startLat"], pch=16, col=colour_scale[1], cex=0.3)
    points(mcc_2021_2022pt1[i,"startLon"], mcc_2021_2022pt1[i,"startLat"], pch=1, col="gray10", cex=0.3)
  }
  points(mcc_2021_2022pt1[i,"endLon"], mcc_2021_2022pt1[i,"endLat"], pch=16, col=endYears_colours[i], cex=0.3)
  points(mcc_2021_2022pt1[i,"endLon"], mcc_2021_2022pt1[i,"endLat"], pch=1, col="gray10", cex=0.3)
}

rect(xmin(template_raster), ymin(template_raster), xmax(template_raster), ymax(template_raster), xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
#rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab[,"startYear"]); rast[2] = max(mcc_tab[,"endYear"])
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_2021_2022pt1[,"startYear"]); rast[2] = max(mcc_2021_2022pt1[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.15),
     legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))