##### Sensitivity analysis with admin level 2 +  Graphs with Slider #####
#### Nebi Yesilekin ####
#### Meng Zhang ####

### This R routine helps us to conconate admin level results for each region plust country level.
### as output, we will have csv files for each sub-region and one for country level 
### for example  
### allyearAshantiMain.csv ---->  year , region name , application name 
### allyearGHAMain.csv ----> year, "GHA" (iso3 name), application name 
## For this example we have used Ghana pythia results and "GHA" iso3 name as defined in the script

### clean all data from global environment except config
rm(list=setdiff(ls(), funList))
library(plotly)
library(gapminder)
library(stringr)
library(sf)
library(raster)
library(maps)
library(data.table)
library(rgdal)
library(ggplot2)
library(rnaturalearth)
library(gtools)
library(GADMTools)
setup_packages(
  c(
    "plotly",
    "gapminder",
    "stringr",
    "sf",
    "maps",
    "data.table",
    "rgdal",
    "ggplot2",
    "rnaturalearth",
    "raster",
    "gtools",
    "GADMTools"
  )
)

### set working directory here
Workdir <- adjPath(configObj$work_dir)
setwd(Workdir)

### give output folder name here
outputfname <- configObj$output_folder_name_2
Outdir <- file.path(configObj$output_base_dir, outputfname)
if (file.exists(Outdir)) {
  unlink(Outdir, recursive=TRUE)
}
Outdir1 <- dir.create(Outdir, suppressWarnings(dirname))
outputPrefix <- configObj$output_file_prefix
### Adjust work directory to the place where store the result from Techtrend 1 script
Workdir1 <-adjPath(getTechTrendResultDir(configObj, 1))

### setup the factor groups
# seasonname <- "Belg"
factors <- getSAFactors(configObj)

### obtain zone name list
parentfolder <- dir(Workdir1, pattern = ".csv")
zonename <- str_split(parentfolder, pattern = "_", simplify = T)[,2]
zonename <- unique(zonename)

for (i in 1 : length(factors)) {
  
  factorStr <- paste0(factors[[i]], collapse = "")
  
  for(a in 1:length(zonename) ){
    print(zonename[a])
    zonefiles <- grep(paste0("*",zonename[a],factorStr), parentfolder)
    zonefiles
    zonesall <- c()
    for (e in zonefiles){
      csvpath <- file.path(Workdir1, parentfolder[e])
      print(parentfolder[e])
      ###appended 4 sensitivity runs fen_tot offset 0, 15, 30, 60
      content <- read.csv(csvpath, header = T, sep = ',', row.names = NULL)
      if(grepl("row.names", colnames(content)[1])==TRUE){
        colnames(content) <-c(colnames(content)[-1], NULL)
      }else{
        
      }
      zonesall <-rbind(content, zonesall)
      assign(gsub(" ","", paste("allyear_", zonename[a])), zonesall)
    }
  }
  
  dframes <- ls()
  allstates <- grep("allyear", dframes)
    
  for( o in 1:length(allstates)){
    
    k <- data.frame(lapply(dframes[allstates[o]], function(w) get(w)))
    if(nrow(k)==0){
      next
    }
    
    waggregation <- data.frame(as.numeric( k[,"YEARS"]),
                             k[,"LONGITUDE"],
                             k[,"LATITUDE"],
                             k[,"HARVEST_AREA"],
                             as.numeric(gsub("^.{4}", "", k[,"PDAT"])) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(gsub("^.{4}", "", k[,"HDAT"])) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"HWAM"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"TMAXA"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"TMINA"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"PRCP"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(gsub("^.{4}", "", k[,"MDAT"])) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"CWAM"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"HWAH"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"GNAM"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"CNAM"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(gsub("^.{4}", "", k[,"EDAT"])) * as.numeric(k[, "HARVEST_AREA"]), 
                             as.numeric(gsub("^.{4}", "", k[,"ADAT"])) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"NICM"]) * as.numeric(k[, "HARVEST_AREA"]),
                             as.numeric(k[,"NUCM"]) * as.numeric(k[, "HARVEST_AREA"])
                             # as.numeric(k[,"NMINC"]) * as.numeric(k[, "HARVEST_AREA"])
    )
    names(waggregation) <- c("YEARS", "LONGITUDE", "LATITUDE", "HARVEST_AREA", "PDAT", 
                           "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT", "CWAM", "HWAH",
                           "GNAM", "CNAM", "EDAT", "ADAT", "NICM", "NUCM"
                           # , "NMINC"
                           )
  
    deneme2 <- data.frame(aggregate(as.numeric(waggregation[,"YEARS"]),by= list(waggregation$YEARS),mean,na.rm=TRUE)[-c(1)],
                          #aggregate(as.numeric(waggregation[,"LONGITUDE"]),by= list(waggregation$YEARS),mean,na.rm=TRUE)[-c(1)],
                          #aggregate(as.numeric(waggregation[,"LONGITUDE"]),by= list(waggregation$YEARS),mean,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"HARVEST_AREA"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          
                          aggregate(as.numeric(waggregation[,"PDAT"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"HDAT"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"HWAM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"TMAXA"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"TMINA"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"PRCP"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"MDAT"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"CWAM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"HWAH"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"GNAM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"CNAM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"EDAT"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"ADAT"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"NICM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)],
                          aggregate(as.numeric(waggregation[,"NUCM"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)]
                          # aggregate(as.numeric(waggregation[,"NMINC"]),by= list(waggregation$YEARS),sum,na.rm=TRUE)[-c(1)]
    )
    names(deneme2) <- c( "YEARS",  "HARVEST_AREA", "PDAT", 
                        "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP",
                        "MDAT", "CWAM", "HWAH", "GNAM", "CNAM", "EDAT",
                        "ADAT", "NICM", "NUCM"
                        # , "NMINC"
                        )
  
    deneme4 <- data.frame(deneme2[, "YEARS"],
                          deneme2[,"HARVEST_AREA"],
                          deneme2[,"PDAT"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"HDAT"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"HWAM"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"TMAXA"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"TMINA"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"PRCP"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"MDAT"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"CWAM"]/deneme2[,"HARVEST_AREA"],
                          deneme2[, "HWAH"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"GNAM"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"CNAM"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"EDAT"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"ADAT"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"NICM"]/deneme2[,"HARVEST_AREA"],
                          deneme2[,"NUCM"]/deneme2[,"HARVEST_AREA"]
                          # deneme2[,"NMINC"]/deneme2[,"HARVEST_AREA"]
                          
    )
  
    names(deneme4) <- c("YEARS",  "HARVEST_AREA", "PDAT",
                        "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP",
                        "MDAT", "CWAM", "HWAH", "GNAM", "CNAM", "EDAT", 
                        "ADAT", "NICM", "NUCM"
                        # , "NMINC"
                        )
  
  
  
  
    ### Getting weighted average #####
    ### converting integer for decimal date average ###
    deneme4$PDAT <- as.integer(deneme4$PDAT)
    deneme4$HDAT <- as.integer(deneme4$HDAT)
    deneme4$MDAT <- as.integer(deneme4$MDAT)
    deneme4$EDAT <- as.integer(deneme4$EDAT)
    deneme4$ADAT <- as.integer(deneme4$ADAT)
    deneme4["TOT_PROD"] <- (deneme4$HARVEST_AREA * deneme4$HWAM)/1000
    deneme4["TOT_NICM"] <- deneme4$HARVEST_AREA * deneme4$NICM
    deneme4["TOT_NUCM"] <- deneme4$HARVEST_AREA * deneme4$NUCM
    # deneme4["TOT_NMINC"] <- deneme4$HARVEST_AREA * deneme4$NMICM
    deneme4$YEARS <- deneme4[, "YEARS"]
    deneme4["VWAM"] <- deneme4$CWAM-deneme4$HWAM   ### 
    deneme4["VNAM"] <- deneme4$CNAM-deneme4$GNAM 
    
    
    resultsfiles2 <- file.path(Outdir, paste0(dframes[allstates[o]], factorStr, ".csv"))
    write.csv(deneme4, resultsfiles2)
    
  }
}
  