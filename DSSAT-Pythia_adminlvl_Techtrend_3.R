#### read observed values ### 

#source("D:\\Workdata\\R_code\\R_code_2\\sensitivity_GitHUB3\\DSSAT-Pythia_adminlvl_Techtrend_2.R")

rm(list = ls())
Workdir <-"D:\\Workdata\\Ethiopia\\Sensitivity_Runs_NewPdatzones\\ETH_ALL_MZ\\ETH_MZ_base\\ETH_MZ_Belg_Base_Analysis_adminlvl2test"
setwd(Workdir)

outputfname <- "ETH_MZ_Belg_Base_Analysis_adminlvl3test"
Outdir1 <- dir.create(file.path(dirname(Workdir), outputfname), suppressWarnings(dirname))
Outdir <- file.path(dirname(Workdir), outputfname)


### choose year range for technology trend analysis 
frstyear <- 2012
lastyear <- 2018
range <- seq(frstyear, lastyear, 1)


parent <- basename(Workdir)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
parentname <- firstup(unlist(strsplit(parent, "_", fixed = TRUE))[3]) ### 3 season name

#unlist(strsplit(Workdir, "_", fixed = TRUE))[3]

observedpath <- "D:\\Workdata\\Ethiopia\\Ixchel\\ETH_OBS_csv"
files <- dir(observedpath, pattern = "_OBS.csv")
files
csvpatho <- paste0(observedpath,'\\', files[2])
csvpatho  
#csvpath <- "C:\\Users\\nebiyesilekin\\Desktop\\Hudo\\vakhtang\\allyearAfarMeher.csv"
contento <- read.csv(csvpatho, header = T, sep = ',', row.names = NULL)
#contento <- na.omit(contento)
#contento[,] <- as.numeric(as.character(contento[,]))
cropname <- as.character(names(contento[1])) 

sfiles <- dir(pattern = "all*")
#sfiles[grep("ETH", sfiles)] <- "allyearETH_CSAMeher.csv"

for(i in 1:length(sfiles)){
  csvpaths <- paste0(Workdir,"\\", sfiles[i])
  contents <- read.csv(csvpaths)
  contentsy <- contents[grep(frstyear, contents[, "YEARS"]):grep(lastyear, contents[, "YEARS"]), "HWAM"]
  assign(gsub("allyear", "", paste0(sfiles[i])), contentsy)
}

 

zonenames<- gsub("-", ".",gsub(paste0("allyear|",parentname,".csv"), "", sfiles))
zonenames <- gsub(",", ".", zonenames)
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
  #devs <- 
  devs[,] <- ((devs-means)/means)
  #### + number of years added 
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
  zonepngname <- paste0(Outdir, "\\", zonenames[e], ".png")  
  png(zonepngname, width=600, height=350)
  par(xpd=FALSE)
  # xlim= c(min(contento[, "Maize"]),(min(contento[, "Maize"])+(nrow(devs)-1)))
  plot(contento[,cropname], contento[, paste0(zonenames[e],"_Yield")], 
       pch = 19,ylim= c(minyd, maxyd), xlim= c(frstyear-1,lastyear+2),
       ylab = "Yield(kg/ha)", xlab= "Years",
       main= paste0(zonenames[e]," Techtrend Application for ", parentname," ",cropname, " Yield")
       ) ##Observed ### type = "b"
  #lines(contento[, "Maize"],fitted(fit2b), col= "red", type = "l" , lty = 6, lwd = 4)
  lines(contento[, cropname],fitted(fit1b), col= "pink", type = "l" , lty = 6, lwd = 4)  ### Trend line
  lines(contents[grep(frstyear, contents[, "YEARS"]):grep(lastyear, contents[, "YEARS"]), "YEARS"],unlist(lapply(ls()[grep(paste0(zonenames[e],parentname, ".csv"), ls())], function(w) get(w))), col= "red", type = "l" , lty = 6, lwd = 4)  ### Simulated 
  lines(yearsl,allpredlast[], col= "blue", type = "l" , lty = 6, lwd = 4) ### Linear Equation Simulated 
  #abline(lm(contento[,paste0(zonenames[1],"_Yield")] ~ contento[,"Maize"]))
  #zonepngname <- paste0(Outdir, "\\", zonenames[1], ".png")
  grid(nx = NULL, ny =  NULL, col = "lightgray", lty = "dotted")
  
  legend("topright",c("Observed", "Trendline", "Simulated", "Corrected Simulated"), 
         pch = c(19,NA,NA,NA), lty = c(NA,6,6,6), col = c("black", "pink","red", "blue"))
  
 ##legend("bottomleft", inset=c(-1,0), legend= c("Observed", "Trendline", "Simulated", "Corrected Simulated"), 
 ##                                           pch = c(19,NA,NA,19), lty = c(NA,6,6,NA),
 ##       mar(c(7,7,7,7)),col = c("black", "pink","red", "blue"))
  dev.off(dev.cur())
  #ggsave(zonepngname)
}


