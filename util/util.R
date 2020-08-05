### Creating SPAM rasters
### Meng Zhang

library(stringr)
library(rjson)



### Adjust path to adopt multiple OS platform
adjPath <- function(pathStr){
  return (str_replace_all(pathStr, "[\\\\/]", .Platform$file.sep))
}

### Parsing the command line arguments to read config files
parseCmd <- function(sourceDir=getwd(), scriptName=NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  ### load default setting
  configFile <- file.path(sourceDir, "config.json")
  pythiaConfigFile <- file.path(sourceDir, "pythia_config.json")
  if (length(args) > 1) {
    ### read config JSON file
    pythiaConfigFile <- args[1]
    if (length(args) > 2) {
      configFile <- args[2]
    }
  }
  
  ### read config file
  if (!file.exists(configFile)) stop (paste0("Cannot find file [", configFile, "]"))
  configObj <- fromJSON(file = configFile)
  ### read Pythia config file
  if (scriptName != "SPAMraster") {
    if (!file.exists(pythiaConfigFile)) stop (paste0("Cannot find file [", pythiaConfigFile, "]"))
    pythiaConfigObj <- fromJSON(file = pythiaConfigFile)
  } else {
    pythiaConfigObj <- c()
  }
  ### generate return object
  if (is.null(scriptName)) {
    if (is.null(configObj$pythia_config)) {
      configObj$pythia_config = pythiaConfigObj
    }
    return (configObj)
  } else {
    if (is.null(configObj[[scriptName]]$pythia_config)) {
      configObj[[scriptName]]$pythia_config = pythiaConfigObj
    }
    return (configObj[[scriptName]])
  }
}

### read config and create factor groups for aggregation
getSAFactors <- function(configObj) {
  factors <- list({})
  # factors <- data.frame()
  for (i in 1 : length(configObj$pythia_config$plugins)) {
    if (configObj$pythia_config$plugins[[i]]$plugin == "sensitivity_plugin") {
      varNames <- names(configObj$pythia_config$plugins[[i]]$params)
      for (j in 1 : length(varNames)) {
        varName <- varNames[[j]]
        values <- configObj$pythia_config$plugins[[i]]$params[[varName]]$values
        # factors[[varName]] <- c()
        orgSize <- length(factors)
        factors <- rep(factors, length(values))
        for (m in 1 : length(values)) {
          for (n in 1 : orgSize) {
            idx = (m - 1) * orgSize + n
            if (typeof(values) != "character") {
              factorStr <- paste0("_", varName, "_", values[[m]])
            } else {
              factorStr <- paste0("_", values[[m]])
            }
            factors[[idx]] <- c(factors[[idx]], factorStr)
          }
        }
      }
    }
  }
  return (factors)
}

getTechTrendResultDir <- function(configObj, scriptNum = 1) {
  if (scriptNum == 1 && !is.null(configObj$output_folder_name_1)) {
    return (file.path(configObj$output_base_dir, configObj$output_folder_name_1))
  } else if (scriptNum == 2 && !is.null(configObj$output_folder_name_2)) {
    return (file.path(configObj$output_base_dir, configObj$output_folder_name_2))
  } else {
    return (configObj$work_dir)
  }
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

split_path <- function(x) {
  if (dirname(x)==x) x 
  else c(basename(x),split_path(dirname(x)))
}
funList <- c()
funList <- ls()
