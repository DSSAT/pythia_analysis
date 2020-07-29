library(stringr)
library(rjson)

adjPath <- function(pathStr){
  return (str_replace_all(pathStr, "[\\\\/]", .Platform$file.sep))
}

parseCmd <- function(scriptName=NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  # load default setting
  configFile <- "config.json"
  pythiaConfigFile <- "pythia_config.json"
  if (length(args) > 1) {
    # read config JSON file
    pythiaConfigFile <- args[1]
    if (length(args) > 2) {
      configFile <- args[2]
    }
  }
  
  if (!file.exists(configFile)) stop (paste0("Cannot find file [", configFile, "]"))
  if (!file.exists(pythiaConfigFile)) stop (paste0("Cannot find file [", pythiaConfigFile, "]"))
  configObj <- fromJSON(file = configFile)
  pythiaConfigObj <- fromJSON(file = pythiaConfigFile)
  if (is.null(scriptName)) {
    configObj$pythia_config = pythiaConfigObj
    return (configObj)
  } else {
    configObj[[scriptName]]$pythia_config = pythiaConfigObj
    return (configObj[[scriptName]])
  }
}

getSAFactors <- function(configObj) {
  for (i in 1 : length(configObj$plugins)) {
    if (configObj$plugins[[i]]$plugin == "sensitivity_plugin") {
      for (j in 1 : length(configObj$plugins[[i]]$params)) {
        
      }
    }
  }

}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
