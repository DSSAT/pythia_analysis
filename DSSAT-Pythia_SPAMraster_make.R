### Creating SPAM rasters
### Nebi Yesilekin
### Meng Zhang

rm(list=ls())
orgWorkDir <- getwd()

### get the location of source to load utility file
if (Sys.getenv("RSTUDIO") == "1") {
  sourceDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  sourceDir <- dirname(regmatches(cmd.args, m))
}
source(file.path(sourceDir, "util", "util.R"))
configObj <- parseCmd(sourceDir, "SPAMraster")

library(rgdal)
library(raster)
library(rasterVis)
library(countrycode)

Workdir <- adjPath(configObj$SPAM_local_dir)
if (dir.exists(Workdir)) {
  setwd(Workdir)
} else {
  dir.create(path=Workdir, recursive = T)
  setwd(Workdir)
  ### download the SPAM data with given URL
  SPAMUrl = configObj$SPAM_url
  if (is.null(SPAMUrl) || SPAMUrl == ""){
    ### using the default SPAM URL to download data
    SPAMUrl = "https://s3.amazonaws.com/mapspam/2010/v1.0/csv/spam2010V1r0_global_harv_area.csv.zip"
  }
  download.file(url=SPAMUrl, destfile="tmp.zip")
  unzip(zipfile="tmp.zip",
        files=grep(unzip(zipfile="tmp.zip", list=TRUE)$Name, pattern="\\.csv$", value=TRUE, ignore.case = TRUE))
  file.remove("tmp.zip")
}

### check the crop full name in list below;
### "Full_name"=c("wheat","rice","maize","barley","pearl millet","small millet","sorghum","other cereals",
### "potato","sweet potato","yams","cassava","other roots","bean","chickpea","cowpea","pigeonpea","lentil",
### "other pulses","soybean","groundnut","coconut","oilpalm","sunflower","rapeseed","sesameseed",
### "other oil crops","sugarcane","sugarbeet","cotton","other fibre crops ofib","arabica coffee",
### "robusta coffee","cocoa","tea","tobacco","banana","plantain","tropical fruit","temperate fruit",
### "vegetables","rest of crops")

### define Country and Crop name below #####
countryname <- configObj$country_name
cropname <- configObj$crop

### Define extension of country in degree 
### add or substract 0.5 degree from each side
### minimum and maximum longitude extension of country
longmin <- configObj$longitude_min  ## Ghana example
longmax <- configObj$longitude_max     ## Ghana example
### minimum and maximim latitude extension of country 
latmin <- configObj$latitude_min    ### Ghana example
latmax <- configObj$latitude_max  ## Ghana example

##
filepath <- file.path(configObj$output_base_dir, countryname, "SPAM")
if (dir.exists(filepath)) {
  unlink(Outdir, recursive=TRUE)
}
Outdir <- dir.create(filepath, recursive = T, suppressWarnings(dirname))
### to find ISO3 name for countries
### ISO3 name for country, for example, "GHA"
country <- countrycode(countryname, origin ='country.name', destination ='iso3c')

### Crop name in SPAM data source
SPAMcrpname <- data.frame("Full_name"=c("wheat","rice","maize","barley","pearl millet","small millet","sorghum","other cereals","potato","sweet potato","yams","cassava","other roots","bean","chickpea","cowpea","pigeonpea","lentil","other pulses","soybean","groundnut","coconut","oilpalm","sunflower","rapeseed","sesameseed","other oil crops","sugarcane","sugarbeet","cotton","other fibre crops ofib","arabica coffee","robusta coffee","cocoa","tea","tobacco","banana","plantain","tropical fruit","temperate fruit","vegetables","rest of crops"),
                           "SPAMcrcode"= c("whea","rice","maiz","barl","pmil","smil","sorg","ocer","pota","swpo","yams","cass","orts","bean","chic","cowp","pige","lent","opul","soyb","grou","cnut","oilp","sunf","rape","sesa","ooil","sugc","sugb","cott","ofib","acof","rcof","coco","teas","toba","bana","plnt","trof","temf","vege","rest"))

crname <- SPAMcrpname[grep(cropname, SPAMcrpname[,"Full_name"]), "SPAMcrcode"] 

list <- dir(pattern = ".csv")
### loop for obtaining harvested area values ###
for (k in 1:length(list)){
  
  ### find csv path and read csv
  cd <- list[k]
  cd
  prefix <- str_split(cd, pattern = "_", simplify=TRUE)[1]
  print(paste0("processing ", cd))
  content <- read.csv(cd, header = T, sep = ',', quote = "", row.names = NULL)
  if(grepl("row.names", colnames(content)[1])==TRUE){
    colnames(content) <-c(colnames(content)[-1], NULL)
  }else{
    
  }
  
  ### grep country line based on iso3 name## 
  rowsofcnt <- grep(country, content[,"iso3"])
  content <- content[][rowsofcnt,]
  ### create record type for SPAM
  rectype <- tolower(gsub(".*(.)", "\\1",gsub(".csv","", list[k])))
  ### choose interested columns 
  result <- data.frame(content[,1:9], content[, paste0(crname,"_",rectype)])
  names(result)[length(names(result))]<- paste0(crname,"_",rectype)  
  ### rasterize ##
  ### Create frame for raster ####
  ### define min and max raster frame ( country extent )
  fr <- raster(xmn=longmin, xmx=longmax, ymn=latmin, ymx=latmax, res=0.083333333, crs="+proj=longlat +datum=WGS84")  ###Ghana All
  #fr <- raster(xmn=-1, xmx=2, ymn=5, ymx=12, res=0.083333333, crs="+proj=longlat +datum=WGS84") 
  us_fire <- rasterize(result[, c('x', 'y')], fr, result[, paste0(crname,"_",rectype)], fun='last')
  plot(us_fire)
  ### write raster with unique name for each managements ### 
  rastername <- file.path(filepath,
                          paste(prefix, tolower(country), "harvested-area", crname, rectype, sep="_"))
  writeRaster(us_fire, filename = rastername, format = "GTiff", options=c('XML=YES'), NAflag= -99, overwrite=TRUE)
  
}

setwd(orgWorkDir)
              