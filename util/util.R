library(stringr)

adjPath <- function(pathStr){
  return (str_replace_all(pathStr, "[\\\\/]", .Platform$file.sep))
}
