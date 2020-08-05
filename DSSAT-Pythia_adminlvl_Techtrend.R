##### Sensitivity analysis with admin level 2 +  Graphs with Slider #####
#### Meng Zhang ####

### This R routine will control which sub script will be run based on given 
### config JSON file. The trigger is the existence of each output path.
### For example,
### 1, if output_folder_name_1 is provided, then script 1 will be executed.
### 2, if both output_folder_name_1 and output_folder_name_2 are provided,
###    then both script 2 and 3 will be executed.
### 3, if only output_folder_name_2 is provided, then it will on run script 2.
### 4, only run script 1 and 3 is not supported, to run 3, you have to run
###    script 2 or provide existing result from previous runs of 2.
###    Similar condition for 2 as well.

### clean all data from global environment
rm(list=ls())
library(stringr)

### get the location of source to load utility file
if (Sys.getenv("RSTUDIO") == "1") {
  sourceDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  sourceDir <- dirname(regmatches(cmd.args, m))
}
source(file.path(sourceDir, "util", "util.R"))
configObj <- parseCmd(sourceDir, "techtrend")
orgWorkDir <- getwd()
funList <- ls()

### run sub script based on given output path
### run techtrend 1
if (!is.null(configObj$output_folder_name_1)) {
  source(file.path(sourceDir, "techtrend", "DSSAT-Pythia_adminlvl_Techtrend_1.R"))
  setwd(orgWorkDir)
}
### run techtrend 2
if (!is.null(configObj$output_folder_name_2)) {
  source(file.path(sourceDir, "techtrend", "DSSAT-Pythia_adminlvl_Techtrend_2.R"))
  setwd(orgWorkDir)
}
### run techtrend 3
if (!is.null(configObj$output_folder_name_3)) {
  if (!is.null(configObj$output_folder_name_1) && is.null(configObj$output_folder_name_2)) {
    ### Only run script 1 & 3 is not supported
    stop ("Only run script 1 & 3 is not supported")
  } else {
    source(file.path(sourceDir, "techtrend", "DSSAT-Pythia_adminlvl_Techtrend_3.R"))
    setwd(orgWorkDir)
  }
}
