# Set directory
#setwd("/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/")
setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024/")
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
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2024.929

# Step 2 - extract spatio-temporal information from whole MCC tree
mcc_tre = readAnnotatedNexus("NAm_wild-aves_nov2021-2024_ha_contphyl_2025-07-10_MCC.tree")
mcc_tab = mccTreeExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC.csv", row.names=F, quote=F)
mcc_tab = read.csv("./NAm_wild-aves_nov2021-2024_ha_MCC.csv", head=T)

# Step 3 - subset MCC tree
#assign_month <- function(df, column) {
  df <- df %>%
    mutate(
      rounded = round(.data[[column]], 6),
      month = case_when(
        rounded >= 2021.832877 ~ "november",
        rounded >= 2021.915068 & rounded < 2022.000000 ~ "december",
        rounded >= 2022.000000 & rounded < 2022.084932 ~ "january",
        rounded >= 2022.084932 & rounded < 2022.161644 ~ "february",
        rounded >= 2022.161644 & rounded < 2022.246575 ~ "march",
        rounded >= 2022.246575 & rounded < 2022.328767 ~ "april",
        rounded >= 2022.328767 & rounded < 2022.413699 ~ "may",
        rounded >= 2022.413699 & rounded < 2022.495890 ~ "june",
        rounded >= 2022.495890 & rounded < 2022.580822 ~ "july",
        rounded >= 2022.580822 & rounded < 2022.665753 ~ "august",
        rounded >= 2022.665753 & rounded < 2022.747945 ~ "september",
        rounded >= 2022.747945 & rounded < 2022.832877 ~ "october",
        rounded >= 2022.832877 & rounded < 2022.915068 ~ "november",
        rounded >= 2022.915068 & rounded < 2023.000000 ~ "december",
        rounded >= 2023.000000 & rounded < 2023.084932 ~ "january",
        rounded >= 2023.084932 & rounded < 2023.161644 ~ "february",
        rounded >= 2023.161644 & rounded < 2023.246575 ~ "march",
        rounded >= 2023.246575 & rounded < 2023.328767 ~ "april",
        rounded >= 2023.328767 & rounded < 2023.413699 ~ "may",
        rounded >= 2023.413699 & rounded < 2023.495890 ~ "june",
        rounded >= 2023.495890 & rounded < 2023.580822 ~ "july",
        rounded >= 2023.580822 & rounded < 2023.665753 ~ "august",
        rounded >= 2023.665753 & rounded < 2023.747945 ~ "september",
        rounded >= 2023.747945 & rounded < 2023.832877 ~ "october",
        rounded >= 2023.832877 & rounded < 2023.915068 ~ "november",
        rounded >= 2023.915068 & rounded < 2024.000000 ~ "december",
        rounded >= 2024.000000 & rounded < 2024.084699 ~ "january",
        rounded >= 2024.084699 & rounded < 2024.163934 ~ "february",
        rounded >= 2024.163934 & rounded < 2024.248634 ~ "march",
        rounded >= 2024.248634 & rounded < 2024.330601 ~ "april",
        rounded >= 2024.330601 & rounded < 2024.415301 ~ "may",
        rounded >= 2024.415301 & rounded < 2024.497268 ~ "june",
        rounded >= 2024.497268 & rounded < 2024.581967 ~ "july",
        rounded >= 2024.581967 & rounded < 2024.666667 ~ "august",
        rounded >= 2024.666667 & rounded < 2024.748634 ~ "september",
        rounded >= 2024.748634 & rounded < 2024.833333 ~ "october",
        rounded >= 2024.833333 & rounded < 2024.915301 ~ "november",
        rounded >= 2024.915301 ~ "december",
        TRUE ~ "unknown"
      )
    ) %>%
    select(-rounded)  # remove the intermediate column, like drop("rounded") in Python
  
  return(df)
}

mcc_2021_jan2022 <- mcc_tab %>%
  filter(endYear >= 2021.744036 & endYear < 2022.084932) # November 2021-January 2022
write.csv(mcc_2021_jan2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_2021-jan2022.csv", row.names=F, quote=F)

mcc_feb2022 <- mcc_tab %>%
  filter(endYear >= 2022.084932 & endYear < 2022.161644) # February 2022
write.csv(mcc_feb2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_feb2022.csv", row.names=F, quote=F)

mcc_mar2022 <- mcc_tab %>%
  filter(endYear >= 2022.161644 & endYear < 2022.246575) # March 2022
write.csv(mcc_mar2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_mar2022.csv", row.names=F, quote=F)

mcc_apr2022 <- mcc_tab %>%
  filter(endYear >= 2022.246575 & endYear < 2022.328767) # April 2022
write.csv(mcc_apr2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_apr2022.csv", row.names=F, quote=F)

mcc_may2022 <- mcc_tab %>%
  filter(endYear >= 2022.328767 & endYear < 2022.413699) # May 2022
write.csv(mcc_may2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_may2022.csv", row.names=F, quote=F)

mcc_jun2022 <- mcc_tab %>%
  filter(endYear >= 2022.413699 & endYear < 2022.495890) # June 2022
write.csv(mcc_jun2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jun2022.csv", row.names=F, quote=F)

mcc_jul2022 <- mcc_tab %>%
  filter(endYear >= 2022.495890 & endYear < 2022.580822) # July 2022
write.csv(mcc_jul2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jul2022.csv", row.names=F, quote=F)

mcc_aug2022 <- mcc_tab %>%
  filter(endYear >= 2022.580822 & endYear < 2022.665753) # August 2022
write.csv(mcc_aug2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_aug2022.csv", row.names=F, quote=F)

mcc_sep2022 <- mcc_tab %>%
  filter(endYear >= 2022.665753 & endYear < 2022.747945) # September 2022
write.csv(mcc_sep2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_sep2022.csv", row.names=F, quote=F)

mcc_oct2022 <- mcc_tab %>%
  filter(endYear >= 2022.747945 & endYear < 2022.832877) # October 2022
write.csv(mcc_oct2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_oct2022.csv", row.names=F, quote=F)

mcc_nov2022 <- mcc_tab %>%
  filter(endYear >= 2022.832877 & endYear < 2022.915068) # November 2022
write.csv(mcc_nov2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_nov2022.csv", row.names=F, quote=F)

mcc_dec2022 <- mcc_tab %>%
  filter(endYear >= 2022.915068 & endYear < 2023.000000) # December 2022
write.csv(mcc_dec2022, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_dec2022.csv", row.names=F, quote=F)

mcc_jan_jun2023 <- mcc_tab %>%
  filter(endYear >= 2023.00000 & endYear < 2023.495890) # First half of 2021
write.csv(mcc_jan_jun2023, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jan-jun2023.csv", row.names=F, quote=F)

mcc_jul_dec2023 <- mcc_tab %>%
  filter(endYear >= 2023.495890 & endYear <= 2023.99878) # Second half of 2023
write.csv(mcc_jul_dec2023, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_jul-dec2023.csv", row.names=F, quote=F)

mcc_2024 <- mcc_tab %>%
  filter(endYear >= 2024.00542 & endYear <= 2024.929) # should be 764
write.csv(mcc_2024, "./mcc_csvs/NAm_wild-aves_nov2021-2024_ha_MCC_2024.csv", row.names=F, quote=F)


# Plot each CSV in the folder and save each plot as a png to the output_plots directory (Chat GPT helped a lot with this)
mcc_files <- list.files("./mcc_csvs", pattern = "\\.csv$", full.names = TRUE)
if (!dir.exists("output_plots")) dir.create("output_plots")

for (file_path in mcc_files) 
{
  k <- read.csv(file_path)
  nberOfExtractionFiles = nberOfTreesToSample
  prob = 0.95; precision = 0.025
  startDatum = min(k[,"startYear"]) 
#  polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))
  
  
  # Step 5 - defining a color scale
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
  r_template <- rast(ext(north_america_vect), resolution = 0.3)
  template_raster <- rasterize(north_america_vect, r_template, field = 1)
  
  borders = gadm(country = "USA", level = 1, path = "/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/SERAPHIM/nov2021-2024/")
  
  # Define output PNG filename
  output_file <- paste0("plot_", tools::file_path_sans_ext(basename(file_path)), ".pdf")
  pdf(file = file.path("output_plots", output_file), width = 6, height = 6.3)
  par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
  plot(template_raster, col="grey", box=F, axes=F, colNA="lightblue", legend=F)
  
  for (i in 1:length(polygons))
  {
    plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
  }
  plot(borders, add=T, lwd=0.1, border="gray10")
  
  for (i in 1:dim(k)[1])
  {
    curvedarrow(cbind(k[i,"startLon"],k[i,"startLat"]), cbind(k[i,"endLon"],k[i,"endLat"]), arr.length=0,
                arr.width=0, lwd=0.2, lty=1, lcol="gray10", arr.col=NA, arr.pos=FALSE, curve=0.1, dr=NA, endhead=F)
  }
  # This loop plots the points, with size 0.15
  # According to CHAT GPT: color is based on endLon/endLat, not startLon/startLat, except for a one-time marker at the first iteration
  for (i in dim(k)[1]:1)
  {
#     if (i == 1)
#    {
#      points(k[i,"startLon"], k[i,"startLat"], pch=16, col=colour_scale[1], cex=0.3)
#      points(k[i,"startLon"], k[i,"startLat"], pch=1, col="gray10", cex=0.3)
#    }
#    points(k[i,"startLon"], k[i,"startLat"], pch=16, col="gray10", cex=0.13) # The "if" loop above colors the first starting point, but this ensure no starting points are colored
    points(k[i,"startLon"], k[i,"startLat"], pch=16, col=endYears_colours[i], cex=0.15) # Louise wanted the starting points colored too, so this line and the one below do that
    points(k[i,"startLon"], k[i,"startLat"], pch=1, col="gray10", cex=0.15)
    points(k[i,"endLon"], k[i,"endLat"], pch=16, col=endYears_colours[i], cex=0.15) # These next two lines color the ending points
    points(k[i,"endLon"], k[i,"endLat"], pch=1, col="gray10", cex=0.15)
  }
  
  rect(xmin(template_raster), ymin(template_raster), xmax(template_raster), ymax(template_raster), xpd=T, lwd=0.2)
  axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
  axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
#  rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(k[,"startYear"]); rast[2] = max(k[,"endYear"]) # This line ascribes the dates to the color scale
  rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(k[,"endYear"]); rast[2] = max(k[,"endYear"]) # I want the scale to only show endYear colors
  plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.15),
       legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
       axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
  
  dev.off()
}
