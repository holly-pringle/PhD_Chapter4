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

data<-read.csv("Nepal2019_allTagsPB_221209_systematic.csv")
length(unique(data$img_filepath)) #63670 #57668
head(data)
unique(data$Species)
allcams2019<-read.csv("SurveyEffort_Mar-Apr_CTs_2019.csv")

data <- rename(data, Site = station) #to match function below
#data$Species<-NULL
data <- rename(data, DateTime = datetime_correctedPB) #change to match function
data$DateTime <- as.Date(data$DateTime, "%d/%m/%Y")

allcams2019<- rename(allcams2019, Date = X) #to match function below

unique(sort(data$Species))
str(data)
nrow(data)
## already in this format data$DateTime_corrected <- as.Date(data$DateTime_corrected,"%Y-%m-%d")
head(allcams2019)
str(allcams2019)

allcams2019$Date <- as.Date(allcams2019$Date, "%d/%m/%Y") # making sure date is Date
sort(unique(allcams2019$Date))
startDate <- as.Date("2019/03/15","%Y/%m/%d") #remove first four days of survey
endDate <- as.Date("2019/04/15","%Y/%m/%d") #remove last four days of survey

nrow(allcams2019) #48
str(startDate)
str(endDate)
str(allcams2019)
all_cams<-allcams2019 #so matches function

table(data$Species)
data <- data %>%  mutate(Species= ifelse(Species == "Samll Indian Civet", "Small Indian Civet", Species))
data <- data %>%  mutate(Species= ifelse(Species == "Palm Civet", "Masked Palm Civet", Species))


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

# applying function to each species (label)
lapply(
  X = unique(data$Species),
  FUN = calcOcc,
  d = data,
  all_cams=all_cams,
  startDate = startDate,
  endDate = endDate) # this will save CSVs of spp matrices in the working directory

str(data$DateTime)
str(all_cams$Date)
