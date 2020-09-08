##### Sensitivity analysis with admin level 2 +  Graphs with Slider #####
#### Nebi Yesilekin ####
#### Meng Zhang ####

#### read observed values ### 
rm(list=setdiff(ls(), funList))
setup_packages(
  c(
    "plyr",
    "stringr"
  )
)

### set working directory here
Workdir <- adjPath(configObj$work_dir)
setwd(Workdir)

### give output folder name here
outputfname <- configObj$output_folder_name_3
Outdir <- file.path(configObj$output_base_dir, outputfname)
if (file.exists(Outdir)) {
  unlink(Outdir, recursive=TRUE)
}
Outdir1 <- dir.create(Outdir, suppressWarnings(dirname))
outputPrefix <- configObj$output_file_prefix
### Adjust work directory to the place where store the result from Techtrend 1 script
Workdir2 <-adjPath(getTechTrendResultDir(configObj, 2))

### choose year range for technology trend analysis 
frstyear <- configObj$first_year_3
lastyear <- configObj$last_year_3
range <- as.character(seq(frstyear, lastyear, 1))
skipYears <- configObj$skip_years

### setup the factor groups
# seasonname <- "Belg"
factors <- getSAFactors(configObj)

parentname <- paste0(factors[[1]], collapse = "")
# parent <- basename(Workdir)
# parentname <- firstup(unlist(strsplit(parent, "_", fixed = TRUE))[3]) ### 3 season name

observedpath <- configObj$observed_path
files <- dir(observedpath, pattern = "_OBS.csv")
files
csvpatho <- file.path(observedpath, files[2])
csvpatho  
contento <- read.csv(csvpatho, header = T, sep = ',', row.names = NULL)
#contento <- na.omit(contento)
#contento[,] <- as.numeric(as.character(contento[,]))
cropname <- as.character(names(contento[1])) 

sfiles <- dir(Workdir2, pattern = "all*")

for(i in 1:length(sfiles)){
  csvpaths <- file.path(Workdir2, sfiles[i])
  contents <- read.csv(csvpaths)
  contentsy <- contents[grep(frstyear, contents[, "YEARS"]):grep(lastyear, contents[, "YEARS"]), "HWAM"]
  assign(gsub("allyear_", "", paste0(sfiles[i])), contentsy)
}
 
### obtain zone name list
parentfolder <- dir(Workdir2, pattern = ".csv")
zonenames <- str_split(parentfolder, pattern = "_", simplify = T)[,2]
zonenames <- unique(zonenames)
zonenames <- gsub(",", ".", zonenames)
zonenames <- gsub("-", ".", zonenames)
years <- range

for( e in 1:length(zonenames)){
  allpred <- c()
  devs <- c()
  yearsl <- c()
   
  print(zonenames[e])
  fit1b <- lm(contento[,paste0(zonenames[e],"_Yield")] ~ contento[,cropname], 1)
  fit1b
  pol1 <- function(a) fit1b$coefficients[1]+ (fit1b$coefficients[2] * (a))
  
  means <- mean(unlist(lapply(ls()[grep(zonenames[e], ls())], function(w) get(w))))
  devs <-  data.frame(unlist(lapply(ls()[grep(paste0(zonenames[e],parentname, ".csv"), ls())], function(w) get(w))))
  devs[,] <- ((devs-means)/means)
  ### + number of years added 
  devs[(nrow(devs)+1):(nrow(devs)+1),] <- 0
  yearsl <- seq(min(contento[, cropname]), (min(contento[, cropname])+(nrow(devs)-1)), by=1)
  for (c in yearsl){
    
    k <- pol1(c)
    allpred <- rbind(allpred, k)
    #assign(paste0(c, "pol2",zonenames[1]),k )
  }
 
  allpredlast <- allpred[,1] * (1+devs[,1])
  #yearsl <- seq(min(contento[, "Maize"]), (min(contento[, "Maize"])+(nrow(devs)-1)), by=1)
 
  combined <- rbind.fill(obs= data.frame(contento[, paste0(zonenames[e],"_Yield")])
                         , data.frame(unlist(lapply(ls()[grep(paste0(zonenames[e],parentname, ".csv"), ls())], function(w) get(w))))
                         ,data.frame(allpredlast[])
                         )
  minyd <- round(min(combined, na.rm = TRUE)-1000, digits = -2)
  maxyd <- round(max(combined, na.rm = TRUE)+4000, digits = -2)
  zonepngname <- file.path(Outdir, paste0(zonenames[e], ".png"))
  png(zonepngname, width=600, height=350)
  par(xpd=FALSE)
  # xlim= c(min(contento[, "Maize"]),(min(contento[, "Maize"])+(nrow(devs)-1)))
  plot(contento[,cropname], contento[, paste0(zonenames[e],"_Yield")], 
       pch = 19,ylim= c(minyd, maxyd), xlim= c(frstyear-1,lastyear+2),
       ylab = "Yield(kg/ha)", xlab= "Years",
       main= paste0(zonenames[e]," Techtrend Application for ", substring(parentname, 2)," ",cropname, " Yield")
       ) ##Observed ### type = "b"
  #lines(contento[, "Maize"],fitted(fit2b), col= "red", type = "l" , lty = 6, lwd = 4)
  lines(contento[, cropname],fitted(fit1b), col= "pink", type = "l" , lty = 6, lwd = 4)  ### Trend line
  lines(contents[grep(frstyear, contents[, "YEARS"]):grep(lastyear, contents[, "YEARS"]), "YEARS"],unlist(lapply(ls()[grep(paste0(zonenames[e],parentname, ".csv"), ls())], function(w) get(w))), col= "red", type = "l" , lty = 6, lwd = 4)  ### Simulated 
  lines(yearsl,allpredlast[], col= "blue", type = "l" , lty = 6, lwd = 4) ### Linear Equation Simulated 
  #abline(lm(contento[,paste0(zonenames[1],"_Yield")] ~ contento[,"Maize"]))
  #zonepngname <- file.path(Outdir, paste0(zonenames[1], ".png"))
  grid(nx = NULL, ny =  NULL, col = "lightgray", lty = "dotted")
  
  legend("topright",c("Observed", "Trendline", "Simulated", "Corrected Simulated"), 
         pch = c(19,NA,NA,NA), lty = c(NA,6,6,6), col = c("black", "pink","red", "blue"))
  
 ##legend("bottomleft", inset=c(-1,0), legend= c("Observed", "Trendline", "Simulated", "Corrected Simulated"), 
 ##                                           pch = c(19,NA,NA,19), lty = c(NA,6,6,NA),
 ##       mar(c(7,7,7,7)),col = c("black", "pink","red", "blue"))
  dev.off(dev.cur())
  #ggsave(zonepngname)
}


