#### Creating SPAM rasters
##### Nebi Yesilekin

rm(list=ls())
library(rgdal)
library(raster)
library(rasterVis)
library(countrycode)

#Set current work directory to the location of source
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  setwd(dirname(regmatches(cmd.args, m)))
}
source(file.path("util", "util.R"))


Workdir <-adjPath("../data/spam2010v1r1_global_harv_area.csv")
setwd(Workdir)

### check the crop full name in list below;
####"Full_name"=c("wheat","rice","maize","barley","pearl millet","small millet","sorghum","other cereals",
####"potato","sweet potato","yams","cassava","other roots","bean","chickpea","cowpea","pigeonpea","lentil",
####"other pulses","soybean","groundnut","coconut","oilpalm","sunflower","rapeseed","sesameseed",
####"other oil crops","sugarcane","sugarbeet","cotton","other fibre crops ofib","arabica coffee",
####"robusta coffee","cocoa","tea","tobacco","banana","plantain","tropical fruit","temperate fruit",
####"vegetables","rest of crops")

###define Country and Crop name below #####
countryname <- "Ghana"
cropname <- "groundnut"

### Define extension of country in degree 
### add or substract 0.5 degree from each side
### minimum and maximum longitude extension of country
longmin <- -4.5  ## Ghana example
longmax <-  2     ## Ghana example
#### minimum and maximim latitude extension of country 
latmin <- 4    ### Ghana example
latmax <- 12  ## Ghana example

##
filepath <- file.path("..", countryname, "SPAM")
Outdir <- dir.create(filepath, recursive = T)
### to find ISO3 name for countries
#country <- "GHA"  ###ISO3 name for country
country <- countrycode(countryname, origin ='country.name', destination ='iso3c')

##Crop name in SPAM data source
SPAMcrpname <- data.frame("Full_name"=c("wheat","rice","maize","barley","pearl millet","small millet","sorghum","other cereals","potato","sweet potato","yams","cassava","other roots","bean","chickpea","cowpea","pigeonpea","lentil","other pulses","soybean","groundnut","coconut","oilpalm","sunflower","rapeseed","sesameseed","other oil crops","sugarcane","sugarbeet","cotton","other fibre crops ofib","arabica coffee","robusta coffee","cocoa","tea","tobacco","banana","plantain","tropical fruit","temperate fruit","vegetables","rest of crops"),
                           "SPAMcrcode"= c("whea","rice","maiz","barl","pmil","smil","sorg","ocer","pota","swpo","yams","cass","orts","bean","chic","cowp","pige","lent","opul","soyb","grou","cnut","oilp","sunf","rape","sesa","ooil","sugc","sugb","cott","ofib","acof","rcof","coco","teas","toba","bana","plnt","trof","temf","vege","rest"))

crname <- SPAMcrpname[grep(cropname, SPAMcrpname[,"Full_name"]), "SPAMcrcode"] 

list <- dir(pattern = ".csv")
### loop for obtaining harvested area values ###
for (k in 1:length(list)){
  
  ##find csv path and read csv
  cd <- list[k]
  cd
  content <- read.csv(cd, header = T, sep = ',', quote = "", row.names = NULL)
  if(grepl("row.names", colnames(content)[1])==TRUE){
    colnames(content) <-c(colnames(content)[-1], NULL)
  }else{
    
  }
  
  ###grep country line based on iso3 name## 
  rowsofcnt <- grep(country, content[,"iso3"])
  content <- content[][rowsofcnt,]
  ## create record type for SPAM
  rectype <- tolower(gsub(".*(.)", "\\1",gsub(".csv","", list[k])))
  ### choose interested columns 
  result <- data.frame(content[,1:9], content[, paste0(crname,"_",rectype)])
  names(result)[length(names(result))]<- paste0(crname,"_",rectype)  
  ##rasterize ##
  ### Create frame for raster ####
  ### define min and max raster frame ( country extent )
  fr <- raster(xmn=longmin, xmx=longmax, ymn=latmin, ymx=latmax, res=0.083333333, crs="+proj=longlat +datum=WGS84")  ###Ghana All
  #fr <- raster(xmn=-1, xmx=2, ymn=5, ymx=12, res=0.083333333, crs="+proj=longlat +datum=WGS84") 
  us_fire <- rasterize(result[, c('x', 'y')], fr, result[, paste0(crname,"_",rectype)], fun='last')
  plot(us_fire)
  ##write raster with unique name for each managements ### 
  rastername <- file.path(filepath,
                          paste("spam2010V1r0", tolower(country), "harvested-area", crname, rectype, sep="_"))
  writeRaster(us_fire, filename = rastername, format = "GTiff", options=c('XML=YES'), NAflag= -99)
  
}


              