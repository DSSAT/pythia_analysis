library(stringr)
library(rjson)

adjPath <- function(pathStr){
  return (str_replace_all(pathStr, "[\\\\/]", .Platform$file.sep))
}

parseCmd <- function(scriptName=NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    # load default setting
    configFile <- "config.json"
  } else {
    # read config JSON file
    configFile <- args[1]
  }
  if (!file.exists(configFile)) stop (paste0("Cannot find file [", configFile, "]"))
  configObj <- fromJSON(file = configFile)
  if (is.null(scriptName)) {
    return (configObj)
  } else {
    return (get(scriptName, configObj))
  }
}
