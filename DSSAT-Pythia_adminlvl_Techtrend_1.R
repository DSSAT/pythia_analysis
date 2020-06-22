##### Sensitivity analysis with admin level 2 +  Graphs with Slider #####
#### Nebi Yesilekin #####

### Before starting folder structure should be like below ### 
###D:\\Workdata\\Ethiopia\\ETH_MZ_fen_tot\\ETH_MZ_belg_fen_tot                
###¦--ETH_MZ_belg_fen_tot_0                          
###¦   ¦--Maize_irrig_belg_S_season_fen_tot_0        
###¦   ¦--Maize_rf_0N_belg_S_season_fen_tot_0        
###¦   ¦--Maize_rf_highN_belg_S_season_fen_tot_0     
###¦   °--Maize_rf_lowN_belg_S_season_fen_tot_0      
###¦--ETH_MZ_belg_fen_tot_100                        
###¦   ¦--Maize_irrig_belg_S_season_fen_tot_100      
###¦   ¦--Maize_rf_0N_belg_S_season_fen_tot_100      
###¦   ¦--Maize_rf_highN_belg_S_season_fen_tot_100   
###¦   °--Maize_rf_lowN_belg_S_season_fen_tot_100    
###¦--ETH_MZ_belg_fen_tot_25                         
###¦   ¦--Maize_irrig_belg_S_season_fen_tot_25       
###¦   ¦--Maize_rf_0N_belg_S_season_fen_tot_25       
###¦   ¦--Maize_rf_highN_belg_S_season_fen_tot_25    
###¦   °--Maize_rf_lowN_belg_S_season_fen_tot_25     
###°--ETH_MZ_belg_fen_tot_50                         
###    ¦--Maize_irrig_belg_S_season_fen_tot_50       
###    ¦--Maize_rf_0N_belg_S_season_fen_tot_50       
###    ¦--Maize_rf_highN_belg_S_season_fen_tot_50    
###    °--Maize_rf_lowN_belg_S_season_fen_tot_50     

### clean all data from global environment
rm(list=ls())

### This R routine helps us to create admin level results for each year.
### as output, we will have csv files for each sub-region and one for country level 
### for example  
### 2018AshantiMain.csv ---->  year , region name , application name 
### 2018GHAMain.csv ----> year, "GHA" (iso3 name), application name 
## For this example we have used Ghana pythia results and "GHA" iso3 name as defined in the script



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



### set working directory here
Workdir <-"D:\\Workdata\\Ghana\\Maize_base_run_6_8_abo\\Maize_base_run_abo_fentot150\\GHA_MZ_Main_base"
setwd(Workdir)
### give output folder name here
outputfname <- "GHA_MZ_Main_Base_Analysis_Abo_adminlvl2test"
Outdir1 <- dir.create(file.path(dirname(Workdir), outputfname), suppressWarnings(dirname))
Outdir <- file.path(dirname(Workdir), outputfname)



###Choosing mainfolder Management types only rainfed or only irrigated
#### 1: irrigated 2: rainfed 0N  3: rainfed highN  4: rainfed lowN c(1,2,3,4)

### Change it on line 108
###Choosing mainfolder Management types only rainfed or only irrigated
#### 1: irrigated 2: rainfed 0N  3: rainfed highN  4: rainfed lowN c(1,2,3,4)
###mainfolder <- mainfolder[][c(1,2,3,4)]
### choosing admin level for subsetting data (based on the GADM database)

adminlvl <- 1
###ISO3 country name 
cntryname <- as.character("GHA")
## inputs ### 
nyears <- 34 #####number of years in seasonal analysis


## filtering years, choosing year range 
frstyear <- 2018
lstyear <- 2018
range <- as.character(seq(frstyear, lstyear,1))



####getting aggregated average for each sell
parent <- basename(Workdir)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
parentname <- firstup(unlist(strsplit(parent, "_", fixed = TRUE))[3]) ### 3 season name
parentfolder <- dir()
#####Choosing Parent Folders ####
#parentfolder[][c(1,2,3,4,5,6,7,8)]
numpar <- length(parentfolder)
resultssens <- c()
for (j in 1:numpar){
    #Locations each cells
    kc <- paste0(Workdir, "\\", parentfolder[j])
    kc
    list.dirs(kc)
    main <- list.dirs(kc)[1]
    split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
    main <- split_path(main)[1]
    main
    # counting number of first folders
    mainfolder<-dir(kc)
    mainfolder
    ###Choosing mainfolder Management types only rainfed or only irrigated
    #### 1: irrigated 2: rainfed 0N  3: rainfed highN  4: rainfed lowN c(1,2,3,4)
    mainfolder <- mainfolder[][c(1,2,3)]
    mainfolder
    mainfoldername <- unlist(strsplit(mainfolder, "_", fixed = TRUE))[1] ###irrig name 
    #mainfolder <- mainfolder [sapply(mainfolder, function(x) length(list.files(x))>0)]
    #mainfolder
    number3 <- length(mainfolder)


    results3 <- c()
    result<-c()
    resultsw <- c()
    allmng <- c()
        for (k in 1:number3) {
          klm <- c()
          #matrix for reading csv colums
          cd <-paste0(kc, '\\', mainfolder[k])
          cd
          filename <- dir(cd)
          filename
          csvpath <- paste0(cd,'\\', filename)
          print(csvpath)
          content <- read.csv(csvpath, header = T, sep = ',', row.names = NULL)
          if(grepl("row.names", colnames(content)[1])==TRUE){
            colnames(content) <-c(colnames(content)[-1], NULL)
          }else{
            
          }
          #### applying temporal queery in data
          if(!length(grep(paste(range,collapse="|"), gsub(".{3}$", "", content[,"HDAT"])))==0){
            rowsofyears <- grep(paste(range,collapse="|"), gsub(".{3}$", "", content[,"HDAT"]))
            content <- content[][rowsofyears,]
          }else{
            
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


      waggregation <- data.frame(as.numeric(gsub(".{3}$", "", newallmng[,"SDAT"])),
                                as.numeric(newallmng[, "LATITUDE"]),
                                as.numeric(newallmng[, "LONGITUDE"]),
                                newallmng[,"HARVEST_AREA"],

                                as.numeric(gsub("^.{4}", "", newallmng[,"PDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"HDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"HWAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"TMAXA"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"TMINA"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"PRCP"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"MDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"CWAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"HWAH"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"GNAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(newallmng[,"CNAM"]) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"EDAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]), 
                                as.numeric(gsub("^.{4}", "", newallmng[,"ADAT"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"NICM"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"NUCM"])) * as.numeric(newallmng[, "HARVEST_AREA"]),
                                as.numeric(gsub("^.{4}", "", newallmng[,"NMINC"])) * as.numeric(newallmng[, "HARVEST_AREA"])
        )
 
 
          names(waggregation) <- c("YEARS", "LATITUDE", "LONGITUDE", "HARVEST_AREA", "PDAT", 
                         "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT", "CWAM", "HWAH", 
                         "GNAM", "CNAM", "EDAT", "ADAT", "NICM", "NUCM", "NMINC")


# Create values for harvested area


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
                      aggregate(as.numeric(waggregation[,"NUCM"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)],
                      aggregate(as.numeric(waggregation[,"NMINC"]),by= list(waggregation$LATITUDE, waggregation$LONGITUDE),sum,na.rm=TRUE)[-c(1,2)]
                      
  
)
names(deneme2) <- c("LATITUDE", "LONGITUDE", "YEARS",  "HARVEST_AREA", "PDAT", 
                         "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT", "CWAM", 
                    "HWAH", "GNAM", "CNAM", "EDAT", "ADAT", "NICM", "NUCM", "NMINC")

  
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
                      deneme2[,"NUCM"]/deneme2[,"HARVEST_AREA"],
                      deneme2[,"NMINC"]/deneme2[,"HARVEST_AREA"]
  
)

names(deneme4) <- c("LATITUDE", "LONGITUDE", "YEARS",  "HARVEST_AREA", "PDAT", 
                    "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP", "MDAT",
                    "CWAM", "HWAH", "GNAM", "CNAM", "EDAT", "ADAT",
                    "NICM","NUCM","NMINC")


###Getting weighted average #####
### converting integer for decimal date average ###
deneme4$PDAT <- as.integer(deneme4$PDAT)
deneme4$HDAT <- as.integer(deneme4$HDAT)
deneme4$MDAT <- as.integer(deneme4$MDAT)
deneme4$EDAT <- as.integer(deneme4$EDAT)
deneme4$ADAT <- as.integer(deneme4$ADAT)
deneme4["TOT_PROD"] <- (deneme4$HARVEST_AREA * deneme4$HWAM)/1000
deneme4["TOT_NICM"] <- deneme4$HARVEST_AREA * deneme4$NICM
deneme4["TOT_NUCM"] <- deneme4$HARVEST_AREA * deneme4$NUCM
deneme4["TOT_NMINC"] <- deneme4$HARVEST_AREA * deneme4$NMINC

deneme4["VWAM"] <- deneme4$CWAM-deneme4$HWAM   ### 
deneme4["VNAM"] <- deneme4$CNAM-deneme4$GNAM 



resultssens <- rbind(resultssens, deneme4)

###Giving parent folder name to csv files #####
resultsfiles2 <- paste0(Outdir, "\\", years[r],cntryname, ".csv")
#write.csv(deneme4, resultsfiles2)

assign(gsub(" ","", paste("name", years[r])), deneme4)
}
}


zonesaggyears <- c()
for (e in 1:length(years)){
  
###appended 4 sensitivity runs fen_tot offset 0, 15, 30, 60
k <- data.frame(lapply(gsub(" ","", paste("name", years[e])), function(w) get(w)))
##conver all columns from factor to numeric
k[] <- lapply(k, function(x) as.numeric(as.character(x)))

###getting ETH border ### 
### creatinf geo sf
gha <- ne_states(country = "Ghana", returnclass = "sf")
class(gha)
gha_pp_sf <- st_as_sf(k, coords =c("LONGITUDE", "LATITUDE"), crs = 4326)

gha_pp_sf <- gha_pp_sf %>%
  mutate( LONGITUDE= unlist(map(gha_pp_sf$geometry,1)),
          LATITUDE = unlist(map(gha_pp_sf$geometry,2)))

gha_pp_sf<- gha_pp_sf %>%
  select(LATITUDE, LONGITUDE, everything())



### zone analysis #### 
setwd(Outdir)

#ctry_shps = do.call("bind", lapply(c(0,1), 
 #                                  function(x) getData('GADM', country="gha", level=x)))
ctry_shps <- getData("GADM", download = TRUE, country="GHA", level=adminlvl)
allstatenumber <- length(ctry_shps$GID_1)
cntrycsvname <- paste0(Outdir, "\\", years[e],cntryname, parentname, ".csv")
write.csv(gha_pp_sf, file = cntrycsvname)
#ctry_shps$NAME_1[1] <- "GHA"
for(z in 1:allstatenumber){
 # if(!z==2){
### definin different subsetted region ### 
print(z)
    print(years[e])
    zonename <- as.data.frame(ctry_shps)
    zonename <- zonename[z,paste0("NAME_",adminlvl)]
    zonename

zone1 <- ctry_shps[z,1]
plot(zone1)

### applying special queery here 
gha_pp_sfzones <- st_intersection(gha_pp_sf, st_set_crs(st_as_sf(as(zone1, "SpatialPolygons")), st_crs(gha_pp_sf)))
gha_pp_sfzones <- gha_pp_sfzones %>%
  mutate( LONGITUDE= unlist(map(gha_pp_sfzones$geometry,1)),
          LATITUDE = unlist(map(gha_pp_sfzones$geometry,2)))

gha_pp_sfzones<- gha_pp_sfzones %>%
  select(LATITUDE, LONGITUDE, everything())


gha_pp_sfzones <- st_set_geometry(gha_pp_sfzones,NULL)


#zonesaggyears <- rbind(gha_pp_sfzones, zonesaggyears)
#zonesaggyears <- aggregate(.,by= zonesaggyears$YEARS,sum,na.rm=TRUE)[-c(1)]
#years <- as.numeric(years)
zonecsvname <- paste0(Outdir, "\\", years[e], zonename, parentname, ".csv")
write.csv(gha_pp_sfzones, file = zonecsvname)


  
}
}


