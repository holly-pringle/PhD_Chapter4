###LOAD PACKAGES

library(unmarked)
library(here)
library(dplyr)
library(data.table)
library(chron)
library(rlist)
library(ggplot2)
library(tibble)
library(jagsUI)
library(wiqid)
library(psych)
library(tidyr)
library(arsenal)
library(abind)
library(unmarked)
library(lubridate)
library(ggcorrplot)

data<-read.csv("Kenya_ML_2018_min3_threshold09_md09_hyenas.csv")
length(unique(data$FilePath_correct)) #131443- manual   #515173- ML
head(data)
unique(data$label)
allcams2018<-read.csv("all_sites_effort_occupancy_v2_2018.csv")

data <- rename(data, Site = CT_site) #to match function below
#data$Species<-NULL
data <- rename(data, Species = label) #change to match function
data <- rename(data, DateTime = DateTime_corrected) #change to match function
data$DateTime <- as.Date(data$DateTime)

unique(sort(data$Species))
str(data)
nrow(data)
## already in this format data$DateTime_corrected <- as.Date(data$DateTime_corrected,"%Y-%m-%d")
head(allcams2018)
str(allcams2018)

allcams2018$Date <- as.Date(allcams2018$Date, "%Y.%m.%d") # making sure date is Date
sort(unique(allcams2018$Date))
startDate <- as.Date("2018/10/09","%Y/%m/%d") #remove first four days of survey
allcams2018 <- allcams2018[-c(1:4), ]
endDate <- as.Date("2018/11/25","%Y/%m/%d") #remove last four days of survey
allcams2018 <- allcams2018[-c(49:52), ]
sort(unique(allcams2018$Date))

nrow(allcams2018) #48
str(startDate)
str(endDate)
str(allcams2018)
all_cams<-allcams2018 #so matches function

str(data)


calcOcc <-
  function(species, # species name - in dataframe - that the function is to be run for
           d = d, # dataframe with species, site, and each date it was seen at that site - must have a columns called Species, Site and DateTime
           all_cams = all_cams, # matrix with all the survey dates, 1s for dates when a camera was working/deployed and NAs for when it wasn't
           startDate = startDate,#start date in date format
           endDate = endDate) {
    # Make a vector of breaks
    brks <-seq(startDate, endDate, by = "day")   #makes a sequence of all the days from start to end
    
    # # Create an empty matrix of dim sites x time periods
    #occ <-matrix(0, ncol = length(unique(d$Site)), nrow = length(brks))
    occ <-matrix(0, ncol = ncol(all_cams)-1, nrow = length(brks)) 
    colnames(occ) <- sort(unique(names(all_cams[-1])))
    rownames(occ) <- strftime(brks, format = "%Y-%m-%d")
    
    for (s in unique(d$Site)) {
      #this loops through each site and inserts 1's on days which there were captures
      seen <- NA
      captures <-na.omit(d$DateTime[d$Species == species & d$Site == s])
      # Were animals seen at the site
      seen <- which(brks %in% as.Date(captures))
      # If the species was seen, occ = 1
      col_i <- which(colnames(occ) == s)
      occ[seen, col_i] <- 1
    }
    
    occ <- occ[, colnames(all_cams[,-1])]
    occ <- occ * all_cams[, 2:ncol(all_cams)]
    print(paste0(species, " done!"))
    species_name <- gsub(" ", "", species)
    row.names(occ) <- brks
    write.csv(occ, paste0("1d_matrix_", species_name, ".csv"))
    return(occ)
    
    
  }


lapply(
  X = unique(data$Species),
  FUN = calcOcc,
  d = data,
  all_cams=all_cams,
  startDate = startDate,
  endDate = endDate) # this will save CSVs of spp matrices in the working directory
