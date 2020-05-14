# Script to get data from slurm jobs and combine to one model
# Version 1.0
# Date:  14/05/2020
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Clear workspace
rm(list = ls()) # Not necessary but sometimes the files we load are quite large

# Libraries
library(brms)
library(rslurm)

# List of all parent folders
pathParentFolder <- c("U:/Projects/bayesianMLM/CBU_clusterGuide")

# This is the pattern that our result folder have
pattern <- '_rslurm_'

# /* 
# ----------------------------- Going through all folders ---------------------------
# */
for(i in 1:length(pathParentFolder)){
  # Get all rslurm folders in that parent folder
  allFiles      <- list.files(pathParentFolder[i])
  rslurmFolders <- allFiles[grepl(pattern, allFiles)]
  modelNames    <- gsub(paste0(pattern, '(.+)'), '\\1', rslurmFolders)
  
  # Go through all folders in that parent folder
  for(j in 1:length(rslurmFolders)){
    # Read rlsurm file
    tempList <- readRDS(paste0(pathParentFolder[i],
                               '/', 
                               rslurmFolders[j], 
                               '/results_0.RDS'), 
                        refhook = NULL)
    
    # Combine all 8 runs of the model
    tempModel <- combine_models(tempList[[1]]$model,
                                tempList[[2]]$model,
                                tempList[[3]]$model,
                                tempList[[4]]$model,
                                tempList[[5]]$model,
                                tempList[[6]]$model,
                                tempList[[7]]$model,
                                tempList[[8]]$model)
    
    # Assign to model name that is taken from folder name
    assign(modelNames[j], tempModel)
  }
}

# Remove temporary files
rm('tempList')
rm('tempModel')

# /* 
# ----------------------------- Save image ---------------------------
# */
save.image('combinedModels.RData')
