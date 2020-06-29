#### Creating SPAM rasters
##### Nebi Yesilekin

library(raster)
library(rasterVis)
library(countrycode)

rm(list=ls())
Workdir <-"D:\\Workdata\\Ethiopia\\SPAM\\World_SPAM\\spam2010V1r0_global_harv_area.csv"
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
filepath <- file.path("D:\\Workdata",countryname, "SPAM", fsep = "\\")
Outdir <- dir.create(filepath, recursive = T)
### to find ISO3 name for countries
#country <- "GHA"  ###ISO3 name for country
country <- countrycode(countryname, origin ='country.name', destination ='iso3c')

##Crop name in SPAM data source
SPAMcrpname <- data.frame("Full_name"=c("wheat","rice","maize","barley","pearl millet","small millet","sorghum","other cereals","potato","sweet potato","yams","cassava","other roots","bean","chickpea","cowpea","pigeonpea","lentil","other pulses","soybean","groundnut","coconut","oilpalm","sunflower","rapeseed","sesameseed","other oil crops","sugarcane","sugarbeet","cotton","other fibre crops ofib","arabica coffee","robusta coffee","cocoa","tea","tobacco","banana","plantain","tropical fruit","temperate fruit","vegetables","rest of crops"),
                           "SPAMcrcode"= c("whea","rice","maiz","barl","pmil","smil","sorg","ocer","pota","swpo","yams","cass","orts","bean","chic","cowp","pige","lent","opul","soyb","grou","cnut","oilp","sunf","rape","sesa","ooil","sugc","sugb","cott","ofib","acof","rcof","coco","teas","toba","bana","plnt","trof","temf","vege","rest"))

crname <- SPAMcrpname[grep(cropname, SPAMcrpname[,"Full_name"]), "SPAMcrcode"] 


list <- dir(Workdir, pattern = ".csv")
### loop for obtaining harvested area values ###
for (k in 1:length(list)){
  
  ##find csv path and read csv
  cd <-paste0(Workdir, '\\', list[k])
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
  rastername <- paste0(filepath,"\\", "spam2010V1r0_", 
                       tolower(country), "_harvested-area_", paste0(crname,"_",rectype))
  writeRaster(us_fire, filename = rastername, format = "GTiff", options=c('XML=YES'), NAflag= -99)
  
}


              