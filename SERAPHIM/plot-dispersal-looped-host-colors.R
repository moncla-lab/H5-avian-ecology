# Set directory
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024_no-suliformes_DTA/duckduckgoose_dta/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_equal-time-host-3000/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_equal-time-host-1000/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_prop-time-loc/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_prop-time-host/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_liveharvest/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_equal-time-loc/")

#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/equal-time-location/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/equal-time-host-3000/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/equal-time-host-1000")
setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/equal-time-location_shuffled")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/proportional-time-host/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/proportional-time-location/")
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/main-intro_six-datasets_mjr/live-harvest/")

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

# Step 1 - set trees directory (this assumes that you have already extracted information for the posterior distribution)
localTreesDirectory = "./Extracted_trees"
nberOfTreesToSample = 827

# mostRecentSamplingDatum = 2025.054794520548 # eq-time-host-1000
# mostRecentSamplingDatum = 2025.054794520548 # eq-time-host-3000
# mostRecentSamplingDatum = 2024.9453551912568 # prop-time-loc
# mostRecentSamplingDatum = 2024.9453551912568 # prop-time-host 
# mostRecentSamplingDatum = 2025.0438356164384 # liveharvest
 mostRecentSamplingDatum = 2025.054794520548 # eq-time-loc

# Step 2 - extract spatio-temporal information from whole MCC tree
#mcc_tre = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_ha_contphyl_2025-07-10_MCC.tree")
#mcc_tab = mccTreeExtractions(mcc_tre, mostRecentSamplingDatum)
#write.csv(mcc_tab, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC.csv", row.names=F, quote=F)
mcc_tab = read.csv("./main-intro_eq-time-loc_ha_traits_MCC.csv", head = T)

# Step 3 - option 1 - subset MCC tree by month

# mcc_2021_jan2022 <- mcc_tab %>%
#   filter(endYear >= 2021.744036 & endYear < 2022.084932) # November 2021-January 2022
# write.csv(mcc_2021_jan2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_2021-jan2022.csv", row.names=F, quote=F)
# 
# mcc_feb2022 <- mcc_tab %>%
#   filter(endYear >= 2022.084932 & endYear < 2022.161644) # February 2022
# write.csv(mcc_feb2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_feb2022.csv", row.names=F, quote=F)
# 
# mcc_mar2022 <- mcc_tab %>%
#   filter(endYear >= 2022.161644 & endYear < 2022.246575) # March 2022
# write.csv(mcc_mar2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_mar2022.csv", row.names=F, quote=F)
# 
# mcc_apr2022 <- mcc_tab %>%
#   filter(endYear >= 2022.246575 & endYear < 2022.328767) # April 2022
# write.csv(mcc_apr2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_apr2022.csv", row.names=F, quote=F)
# 
# mcc_may2022 <- mcc_tab %>%
#   filter(endYear >= 2022.328767 & endYear < 2022.413699) # May 2022
# write.csv(mcc_may2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_may2022.csv", row.names=F, quote=F)
# 
# mcc_jun2022 <- mcc_tab %>%
#   filter(endYear >= 2022.413699 & endYear < 2022.495890) # June 2022
# write.csv(mcc_jun2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jun2022.csv", row.names=F, quote=F)
# 
# mcc_jul2022 <- mcc_tab %>%
#   filter(endYear >= 2022.495890 & endYear < 2022.580822) # July 2022
# write.csv(mcc_jul2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jul2022.csv", row.names=F, quote=F)
# 
# mcc_aug2022 <- mcc_tab %>%
#   filter(endYear >= 2022.580822 & endYear < 2022.665753) # August 2022
# write.csv(mcc_aug2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_aug2022.csv", row.names=F, quote=F)
# 
# mcc_sep2022 <- mcc_tab %>%
#   filter(endYear >= 2022.665753 & endYear < 2022.747945) # September 2022
# write.csv(mcc_sep2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_sep2022.csv", row.names=F, quote=F)
# 
# mcc_oct2022 <- mcc_tab %>%
#   filter(endYear >= 2022.747945 & endYear < 2022.832877) # October 2022
# write.csv(mcc_oct2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_oct2022.csv", row.names=F, quote=F)
# 
# mcc_nov2022 <- mcc_tab %>%
#   filter(endYear >= 2022.832877 & endYear < 2022.915068) # November 2022
# write.csv(mcc_nov2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_nov2022.csv", row.names=F, quote=F)
# 
# mcc_dec2022 <- mcc_tab %>%
#   filter(endYear >= 2022.915068 & endYear < 2023.000000) # December 2022
# write.csv(mcc_dec2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_dec2022.csv", row.names=F, quote=F)
# 
# mcc_jan_jun2023 <- mcc_tab %>%
#   filter(endYear >= 2023.00000 & endYear < 2023.495890) # First half of 2021
# write.csv(mcc_jan_jun2023, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jan-jun2023.csv", row.names=F, quote=F)
# 
# mcc_jul_dec2023 <- mcc_tab %>%
#   filter(endYear >= 2023.495890 & endYear <= 2023.99878) # Second half of 2023
# write.csv(mcc_jul_dec2023, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jul-dec2023.csv", row.names=F, quote=F)
# 
# mcc_2024 <- mcc_tab %>%
#   filter(endYear >= 2024.00542 & endYear <= 2024.929) # should be 764
# write.csv(mcc_2024, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_2024.csv", row.names=F, quote=F)

# Step 3 - option 2 - subset MCC by season
if (!dir.exists("mcc_csvs")) dir.create("mcc_csvs")

mcc_fall21 <- mcc_tab %>%
  filter(endYear >= 2021.752859 & endYear <= 2021.96986) # Autumn 2021
write.csv(mcc_fall21, "./mcc_csvs/fall2021.csv", row.names = F, quote = F)

mcc_winter21_22 <- mcc_tab %>%
  filter(endYear > 2021.96986 & endYear <= 2022.21096) # Winter 2021-22
write.csv(mcc_winter21_22, "./mcc_csvs/winter2021-22.csv", row.names = F, quote = F)

mcc_spring22 <- mcc_tab %>%
  filter(endYear > 2022.21096 & endYear <= 2022.46575) # Spring 2022
write.csv(mcc_spring22, "./mcc_csvs/spring2022.csv", row.names = F, quote = F)

mcc_summer22 <- mcc_tab %>%
  filter(endYear > 2022.46575 & endYear <= 2022.72055) # Summer 2022
write.csv(mcc_summer22, "./mcc_csvs/summer2022.csv", row.names = F, quote = F)

mcc_fall22 <- mcc_tab %>%
  filter(endYear > 2022.72055 & endYear <= 2022.96712) # Autumn 2022
write.csv(mcc_fall22, "./mcc_csvs/fall2022.csv", row.names = F, quote = F)

mcc_winter22_23 <- mcc_tab %>%
  filter(endYear > 2022.96712 & endYear <= 2023.21096) # Winter 2022-23
write.csv(mcc_winter22_23, "./mcc_csvs/winter2022-23.csv", row.names = F, quote = F)

mcc_spring23 <- mcc_tab %>%
  filter(endYear > 2023.21096 & endYear <= 2023.46575) # Spring 2023
write.csv(mcc_spring23, "./mcc_csvs/spring2023.csv", row.names = F, quote = F)

mcc_summer23 <- mcc_tab %>%
  filter(endYear > 2023.46575 & endYear <= 2023.72329) # Summer 2023
write.csv(mcc_summer23, "./mcc_csvs/summer2023.csv", row.names = F, quote = F)

mcc_fall23 <- mcc_tab %>%
  filter(endYear > 2023.72329 & endYear <= 2023.96712) # Autumn 2023
write.csv(mcc_fall23, "./mcc_csvs/fall2023.csv", row.names = F, quote = F)

mcc_winter23_24 <- mcc_tab %>%
  filter(endYear > 2023.96712 & endYear <= 2024.21038) # Winter 2023-24
write.csv(mcc_winter23_24, "./mcc_csvs/winter2023-24.csv", row.names = F, quote = F)

mcc_spring_summer_autumn_winter_24 <- mcc_tab %>%
  filter(endYear > 2024.21038) # Rest of 2024
write.csv(mcc_spring_summer_autumn_winter_24, "./mcc_csvs/spring-summer-autumn2024.csv", row.names = F, quote = F)

# Plot each CSV in the folder and save each plot as a png to the output_plots directory (Chat GPT helped a lot with this)
mcc_files <- list.files("./mcc_csvs", pattern = "\\.csv$", full.names = TRUE)
if (!dir.exists("maps")) dir.create("maps")

for (file_path in mcc_files)
{
  k <- read.csv(file_path)
  nberOfExtractionFiles = nberOfTreesToSample
  prob = 0.95; precision = 0.025
  startDatum = min(k[,"startYear"]) 
#  polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))
  
  
  # Step 5 - defining a color scale
  
  #ddg_host_colors <- c("#A66F6F", "#879E82", "#D69E49", "#68A0A6", "#D9A7B0")
  #ddg_host_names <- c("accipitriformes/strigiformes", "duck", "geese_swans", "charadriiformes/shorebirds", "other")
  #names(ddg_host_colors) <- ddg_host_names
  
  #host_colors <- c("#7E3636", "#566F5E", "#FEC260", "#65518A")
  #host_names <- c("accipitriformes/strigiformes", "anseriformes", "charadriiformes/shorebirds", "other")

  host_colors <- c("#C43110", "#679C48", "#DBB730", "#68A0A6", "#D9A7B0", "#E08907", "#503A63", "#A0A0A3")
  host_names <- c("bird-of-prey", "duck", "goose-swan", "shorebird", "songbird-etc", "ground-bird", "diving-bird", "other")
  names(host_colors) <- host_names

  colour_scale = colorRampPalette(brewer.pal(11,"RdYlGn"))(141)[21:121] # This line provides a gradient of 100 colors from red > yellow > green
  #minYear = min(k[,"startYear"]); maxYear = max(k[,"endYear"])
  minYear = min(k[,"endYear"]); maxYear = max(k[,"endYear"])
  endYears_indices = (((k[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
  endYears_colours = colour_scale[endYears_indices]
#  polygons_colours = rep(NA, length(polygons))
#  for (i in 1:length(polygons))
#  {
#    date = as.numeric(names(polygons[[i]]))
#    polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
#    polygons_colours[i] = paste0(colour_scale[polygon_index],"40")
#  }

  # Step 6 - plot HPD regions and MCC tree
  north_america <- ne_countries(continent = "north america", returnclass = "sf")
  north_america_vect <- vect(north_america)
  #r_template <- rast(ext(north_america_vect), resolution = 0.3) # To use the full raster, use this line of code
  custom_ext <- ext(-171, -45, 7, 75) # if you want to make custom dimensions in the raster, use this code
  r_template <- rast(custom_ext, resolution = 0.3) # This code implements any custome dimensions for your raster
  template_raster <- rasterize(north_america_vect, r_template, field = 1)
  
  states = c("USA", "Canada", "Mexico")
  countries = c("Guatemala", "Belize", "Honduras", "El Salvador", "Costa Rica", "Panama", "Cuba", "Haiti", "Puerto Rico", "Nicaragua", "Dominican Republic", "Greenland")
  borders1 = gadm(country = states, level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024_no-suliformes/")
  borders2 = gadm(country = countries, level = 0, path = "/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024_no-suliformes/")

  # Define output PNG filename
  output_file <- paste0("plot_", tools::file_path_sans_ext(basename(file_path)), ".pdf")
  pdf(file = file.path("./maps/", output_file), width = 6, height = 6.3)
  par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
  plot(template_raster, col="#DEDEDE", box=F, axes=F, colNA="#C3E0DF", legend=F)
  
#  for (i in 1:length(polygons))
#  {
#    plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
#  }
  plot(borders1, add=T, lwd=0.1, border="#B5B3B3")
  plot(borders2, add=T, lwd=0.1, border="#B5B3B3")
  
#  for (i in 1:dim(k)[1])
#  {
#    curvedarrow(cbind(k[i,"startLon"],k[i,"startLat"]), cbind(k[i,"endLon"],k[i,"endLat"]), arr.length=0,
#                arr.width=0, lwd=0.2, lty=1, lcol="gray32", arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
#  }
  # This loop plots the points, with size 0.15
  # According to CHAT GPT: color is based on endLon/endLat, not startLon/startLat, except for a one-time marker at the first iteration
  for (i in dim(k)[1]:1)
  {
    #curvedarrow(cbind(k[i,"startLon"],k[i,"startLat"]), cbind(k[i,"endLon"],k[i,"endLat"]), arr.length=0,
    #            arr.width=0, lwd=0.2, lty=1, lcol="gray32", arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
    curvedarrow(cbind(k[i,"startLon"],k[i,"startLat"]), cbind(k[i,"endLon"],k[i,"endLat"]), arr.length=0,
                arr.width=0, lwd=0.5, lty=1, lcol=host_colors[(k[i, "startHost"])], arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
#     if (i == 1)
#    {
#      points(k[i,"startLon"], k[i,"startLat"], pch=16, col=colour_scale[1], cex=0.3)
#      points(k[i,"startLon"], k[i,"startLat"], pch=1, col="gray10", cex=0.3)
#    }
#    points(k[i,"startLon"], k[i,"startLat"], pch=16, col="gray10", cex=0.13) # The "if" loop above colors the first starting point, but this ensure no starting points are colored
    #points(k[i,"startLon"], k[i,"startLat"], pch=16, col=endYears_colours[i], cex=0.3) # Louise wanted the starting points colored too, so this line and the one below do that
    points(k[i,"startLon"], k[i,"startLat"], pch=16, col = host_colors[(k[i, "startHost"])], cex=0.3)
    points(k[i,"startLon"], k[i,"startLat"], pch=1, col="gray10", cex=0.3)
    #points(k[i,"endLon"], k[i,"endLat"], pch=16, col=endYears_colours[i], cex=0.3) # These next two lines color the ending points
    points(k[i,"endLon"], k[i,"endLat"], pch=16, col = host_colors[(k[i, "endHost"])], cex=0.3)
    points(k[i,"endLon"], k[i,"endLat"], pch=1, col="gray10", cex=0.3)
  }
  
  rect(xmin(template_raster), ymin(template_raster), xmax(template_raster), ymax(template_raster), xpd=T, lwd=0.2)
  axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
  axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
  legend("bottom", legend = names(host_colors), col = host_colors, pch = 16, bty = "n", cex = 0.7, pt.cex = 0.6, ncol = 3)
#  rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(k[,"startYear"]); rast[2] = max(k[,"endYear"]) # This line ascribes the dates to the color scale
#  rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(k[,"endYear"]); rast[2] = max(k[,"endYear"]) # I want the scale to only show endYear colors
#  plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.15),
#       legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
#       axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
  
  dev.off()
}
