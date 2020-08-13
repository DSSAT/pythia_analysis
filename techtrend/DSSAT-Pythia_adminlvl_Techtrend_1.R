##### Sensitivity analysis with admin level 2 +  Graphs with Slider #####
#### Nebi Yesilekin ####
#### Meng Zhang ####

### This R routine helps us to create admin level results for each year.
### as output, we will have csv files for each sub-region and one for country level 
### for example  
### 2018_Ashanti_Main.csv ---->  year , region name , application name 
### 2018_GHA_Main.csv ----> year, "GHA" ISO3 name), application name 
### The above example used Ghana Pythia results and "GHA" ISO3 name as defined in the config file

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
library(tidyverse)

### set working directory here
Workdir <- adjPath(configObj$work_dir)
setwd(Workdir)

### give output folder name here
outputfname <- configObj$output_folder_name_1
Outdir <- file.path(configObj$output_base_dir, outputfname)
if (file.exists(Outdir)) {
  unlink(Outdir, recursive=TRUE)
}
Outdir1 <- dir.create(Outdir, suppressWarnings(dirname))
outputPrefix <- configObj$output_file_prefix


### GADM parameters
### choosing admin level for subsetting data (based on the GADM database)
adminlvl <- configObj$GADM$admin_level
### ISO3 country name 
cntryname <- as.character(configObj$GADM$country_code)

### filtering years, choosing year range 
frstyear <- configObj$first_year
lstyear <- configObj$last_year
nyears <- lstyear - frstyear + 1
range <- as.character(seq(frstyear, lstyear, 1))
skipYears <- configObj$skip_years

### setup the factor groups
factors <- getSAFactors(configObj)

### getting aggregated average for each sell
parent <- basename(Workdir)

parentfolder <- dir()
resultssens <- c()
for (i in 1:length(factors)) {
  ### collect related sub result folders by grouped factors
  mainfolder <- parentfolder
  for (j in 1:length(factors[[i]])) {
    mainfolder <- grep(paste0("(", factors[[i]][j], "$|", factors[[i]][j], "_)"), mainfolder, value = TRUE)
  }
  
  number3 <- length(mainfolder)
  if (number3 == 0) {
    next
  }
  factorStr <- paste0(factors[[i]], collapse = "")


  results3 <- c()
  result<-c()
  resultsw <- c()
  allmng <- c()
  for (k in 1:number3) {
    klm <- c()
    ### matrix for reading csv columns
    cd <- mainfolder[k]
    cd
    filename <- dir(cd, pattern = ".csv")
    filename
    csvpath <- file.path(cd, filename)
    print(csvpath)
    content <- read.csv(csvpath, header = T, sep = ',', row.names = NULL)
    if(grepl("row.names", colnames(content)[1])==TRUE){
      colnames(content) <-c(colnames(content)[-1], NULL)
    }
    ### applying temporal queery in data
    if(!length(grep(paste(range,collapse="|"), gsub(".{3}$", "", content[,"HDAT"])))==0){
      rowsofyears <- grep(paste(range,collapse="|"), gsub(".{3}$", "", content[,"HDAT"]))
      content <- content[][rowsofyears,]
    }
    
    #years <- seq(frstyear, frstyear+max(content[,"RUNNO"])-1,1)
    years <- range
    
    
    content[,"HWAM"][content[,"HWAM"]== -99] <- NA
  
    print(nrow(content))
    #content[content== -99] <- NA
    allmng <- rbind(content, allmng)
    
  }
 
  noyears <- length(years)
          
  for( r in 1:noyears){
    
    rowsofsameyears <- grep(years[r], gsub(".{3}$", "", allmng[,"HDAT"]))
    newallmng <- allmng[][rowsofsameyears,]
  
    waggregation <- data.frame(YEARS= as.numeric(gsub(".{3}$", "", newallmng[,"HDAT"])),
                              LATITUDE= as.numeric(newallmng[, "LATITUDE"]),
                              LONGITUDE=as.numeric(newallmng[, "LONGITUDE"]),
                              HARVEST_AREA=newallmng[,"HARVEST_AREA"],
  
                              PDAT=as.numeric(gsub("^.{4}", "", newallmng[,"PDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              HDAT=as.numeric(gsub("^.{4}", "", newallmng[,"HDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              HWAM=as.numeric(newallmng[,"HWAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              TMAX =as.numeric(newallmng[,"TMAXA"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              TMINA=as.numeric(newallmng[,"TMINA"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              PRPC=as.numeric(newallmng[,"PRCP"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              MDAT=as.numeric(gsub("^.{4}", "", newallmng[,"MDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              CWAM=as.numeric(newallmng[,"CWAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              HWAH=as.numeric(newallmng[,"HWAH"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              GNAM=as.numeric(newallmng[,"GNAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              CNAM=as.numeric(newallmng[,"CNAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              EDAT=as.numeric(gsub("^.{4}", "", newallmng[,"EDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]), 
                              ADAT=as.numeric(gsub("^.{4}", "", newallmng[,"ADAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              NICM=as.numeric(gsub("^.{4}", "", newallmng[,"NICM"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                              NUCM=as.numeric(gsub("^.{4}", "", newallmng[,"NUCM"])) * as.numeric(newallmng[, "HARVEST_AREA"])
                              # NMINC=as.numeric(gsub("^.{4}", "", newallmng[,"NMINC"])) * as.numeric(newallmng[, "HARVEST_AREA"])
      
      )
  
  
    names(waggregation) <- c("YEARS", "LATITUDE", "LONGITUDE", "HARVEST_AREA", "PDAT", 
                   "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT", "CWAM", "HWAH", 
                   "GNAM", "CNAM", "EDAT", "ADAT", "NICM", "NUCM"
                   # , "NMINC"
                   )
    if (nrow(waggregation) == 0) {
      next
    }
  
    ### Create values for harvested area
    deneme2 <- data.frame(aggregate(as.numeric(waggregation[,"YEARS"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),mean,na.rm=TRUE),
                          #aggregate(as.numeric(waggregation[,"LATITUDE"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),mean,na.rm=TRUE),
                          #aggregate(as.numeric(waggregation[,"LONGITUDE"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),mean,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"HARVEST_AREA"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          
                          aggregate(as.numeric(waggregation[,"PDAT"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"HDAT"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"HWAM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"TMAXA"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"TMINA"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"PRCP"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"MDAT"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"CWAM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"HWAH"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"GNAM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"CNAM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"EDAT"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"ADAT"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"NICM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                          aggregate(as.numeric(waggregation[,"NUCM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)]
                          # aggregate(as.numeric(waggregation[,"NMINC"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)]
                    
  
    )
    names(deneme2) <- c("LATITUDE", "LONGITUDE", "YEARS",  "HARVEST_AREA", "PDAT", 
                        "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT", "CWAM", 
                        "HWAH", "GNAM", "CNAM", "EDAT", "ADAT", "NICM", "NUCM"
                  # ,"NMINC"
                  )
  
    deneme4 <- data.frame(deneme2[, "LATITUDE"],
                      deneme2[, "LONGITUDE"],
                      deneme2[, "YEARS"],
                      deneme2[,"HARVEST_AREA"],
                      deneme2[,"PDAT"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"HDAT"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"HWAM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"TMAXA"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"TMINA"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"PRCP"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"MDAT"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"CWAM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"HWAH"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"GNAM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"CNAM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"EDAT"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"ADAT"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"NICM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"NUCM"]/deneme2[,"HARVEST_AREA"]
                      # deneme2[,"NMINC"]/deneme2[,"HARVEST_AREA"]
    
    )
  
    names(deneme4) <- c("LATITUDE", "LONGITUDE", "YEARS",  "HARVEST_AREA", "PDAT", 
                    "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT",
                    "CWAM", "HWAH", "GNAM", "CNAM", "EDAT", "ADAT",
                    "NICM","NUCM"
                    # ,"NMINC"
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
    # deneme4["TOT_NMINC"] <- deneme4$HARVEST_AREA * deneme4$NMINC
    
    deneme4["VWAM"] <- deneme4$CWAM-deneme4$HWAM   ### 
    deneme4["VNAM"] <- deneme4$CNAM-deneme4$GNAM 
  
  
  
    resultssens <- rbind(resultssens, deneme4)
    
    ###Giving parent folder name to csv files #####
    resultsfiles2 <- file.path(Outdir, paste0(years[r],cntryname, ".csv"))
    #write.csv(deneme4, resultsfiles2)
    
    assign(gsub(" ","", paste("name", years[r])), deneme4)
  }
}

### zone analysis ###
setwd(Outdir)
zonesaggyears <- c()
for (e in 1:length(years)){
  
  ### appended 4 sensitivity runs fen_tot offset 0, 15, 30, 60
  k <- data.frame(lapply(gsub(" ","", paste("name", years[e])), function(w) get(w)))
  ### conver all columns from factor to numeric
  k[] <- lapply(k, function(x) as.numeric(as.character(x)))

  ### getting country border ### 
  ### creating geo sf
  gha_pp_sf <- st_as_sf(k, coords =c("LONGITUDE", "LATITUDE"), crs = 4326)

  gha_pp_sf <- gha_pp_sf %>%
    mutate( LONGITUDE= unlist(map(gha_pp_sf$geometry,1)),
            LATITUDE = unlist(map(gha_pp_sf$geometry,2)))

  gha_pp_sf<- gha_pp_sf %>%
    select(LATITUDE, LONGITUDE, everything())

  ctry_shps <- getData("GADM", download = TRUE, country=cntryname, level=adminlvl)
  allstatenumber <- length(ctry_shps$GID_1)
  cntrycsvname <- paste0(paste(years[e],cntryname, sep = "_"), factorStr, ".csv")
  write.csv(gha_pp_sf, file = cntrycsvname)
  for(z in 1:allstatenumber){
    # if(!z==2){
    ### defining different subset region ### 
    print(z)
        print(years[e])
        zonename <- as.data.frame(ctry_shps)
        zonename <- zonename[z,paste0("NAME_",adminlvl)]
        zonename
  
    zone1 <- ctry_shps[z,1]
    plot(zone1)
  
    ### applying special queery here 
    gha_pp_sfzones <- st_intersection(gha_pp_sf, st_set_crs(st_as_sf(as(zone1, "SpatialPolygons")), st_crs(gha_pp_sf)))
    if(!nrow(gha_pp_sfzones)==0){
      gha_pp_sfzones <- gha_pp_sfzones %>%
        mutate( LONGITUDE= unlist(map(gha_pp_sfzones$geometry,1)),
                LATITUDE = unlist(map(gha_pp_sfzones$geometry,2)))
    
      gha_pp_sfzones<- gha_pp_sfzones %>%
        select(LATITUDE, LONGITUDE, everything())
      
      
      gha_pp_sfzones <- st_set_geometry(gha_pp_sfzones,NULL)
    
    
      #zonesaggyears <- rbind(gha_pp_sfzones, zonesaggyears)
      #zonesaggyears <- aggregate(.,by= zonesaggyears$YEARS,sum,na.rm=TRUE)[-c(1)]
      #years <- as.numeric(years)
      zonecsvname <- paste0(paste(years[e], zonename, sep = "_"), factorStr, ".csv")
      write.csv(gha_pp_sfzones, file = zonecsvname)
    }
  }
}
