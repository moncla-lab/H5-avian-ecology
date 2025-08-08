library(seraphim)

# Set working directory
setwd("/Users/claramal/Desktop/Moncla_Lab/H5_avian_ecology/SERAPHIM/2022tree_test/")
getwd()

# Step 1 - extract spatio-temporal information from trees

localTreesDirectory = "Extracted_trees"
nberOfTreesToSample = 100 
mostRecentSamplingDatum = 2022.9945205479453
  
  # Use the "postTreeExtractions" function (advised)
  
trees = readAnnotatedNexus("NAm_wild-ave_2022tree_ha_contphyl_100combined.trees")
# This takes a bit of time
dir.create(localTreesDirectory, showWarnings=F)
for (i in 1:length(trees))
{
  tab = postTreeExtractions(post_tre=trees[[i]], mostRecentSamplingDatum)
  write.csv(tab, paste0(localTreesDirectory,"/TreeExtractions_",i,".csv"), row.names=F, quote=F)
}
# This also takes a while, and doesn't tell you while its going, but you can check the folder

# Step 2 - estimate dispersal statistics

nberOfExtractionFiles = 100
timeSlices = 100
onlyTipBranches = FALSE
showingPlots = FALSE
outputName = "NAm_wild-ave_2022tree_ha_contphyl"
nberOfCores = 1
slidingWindow = 1

spreadStatistics(localTreesDirectory, nberOfExtractionFiles, timeSlices, onlyTipBranches, 
                 showingPlots, outputName, nberOfCores, slidingWindow)
# 3 extraction files takes 10 minutes...