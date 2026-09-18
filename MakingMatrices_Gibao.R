####LOAD PACKAGES####

library(dplyr)
library(ggplot2)
library(tidyr)
library(arsenal)
library(abind)
library(rlist)
library(unmarked)
library(here)
library(data.table)
library(chron)
library(rlist)
library(tibble)
library(jagsUI)
library(wiqid)
library(psych)
library(lubridate)
library(ggcorrplot)

gibao<-read.csv("~/Chapter_4/Cerrado/Legacy_2012-2017/Gibao_OC_2017_Data+Metadata_vChecked_HP.csv")
names(gibao)
length(unique(gibao$File.name))
unique(gibao$Species)

allcams2017<-read.csv("effort_matrix_gibao_2017.csv")

## for survey effort x wildlife images

all_cams_totals_2017<-allcams2017 %>%summarize_if(is.numeric, sum, na.rm=TRUE) #total number of days per site
all_cams_totals_2017<-as.data.frame(t(all_cams_totals_2017)) #transpose
setDT(all_cams_totals_2017, keep.rownames = TRUE[]) #renaming columns
colnames(all_cams_totals_2017) <- c("CT_site","N_Days") #renaming columns
head(all_cams_totals_2017)
nrow(allcams2017) #58

#####PREPARE MATRICES####
data<-gibao
data <- rename(data, Site = Sampling_site) #to match function below
data <- rename(data, DateTime = Date) #change to match function


unique(sort(data$Species))
str(data)
data$Species <- sub(pattern="Bos sp", replace="Bos",data$Species)
data$Species <- sub(pattern="Bos ", replace="Bos",data$Species)
data$Species <- sub(pattern="Leopardus tigrinus ", replace="Leopardus tigrinus",data$Species)

head(allcams2017)
str(allcams2017)

allcams2017<- rename(allcams2017, Date = X) #change to match function

allcams2017$Date <- as.Date(allcams2017$Date, "%d/%m/%Y") # making sure date is Date
sort(unique(allcams2017$Date))
startDate <- as.Date("2017-06-28","%Y-%m-%d") #remove first four days of survey
allcams2017<- allcams2017[-c(1:5), ]
endDate <- as.Date("2017-08-14","%Y-%m-%d") #remove last four days of survey
allcams2017 <- allcams2017[-c(49:53), ]
sort(unique(allcams2018$Date))

str(allcams2017)
view(allcams2017)
allcams2017 <- allcams2017 %>% select(order(colnames(allcams2017)))
tail(allcams2017)
str(allcams2017)

nrow(allcams2017) #48
str(startDate)
str(endDate)
str(allcams2017)
all_cams<-allcams2017 #so matches function
has_rownames(all_cams)
str(all_cams)

str(data)

# 1) A robust parser that accepts several common input formats
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  as.Date(x, tryFormats = c(
    "%Y-%m-%d",   # 2017-06-28
    "%d/%m/%Y",   # 28/06/2017
    "%d-%m-%Y",   # 28-06-2017
    "%d %b %Y"    # 28 Jun 2017
  ))
}

# 2) Apply consistently across your objects
all_cams$Date   <- to_Date(all_cams$Date)         # already prints like "2017-07-01"
data$DateTime       <- to_Date(data$DateTime)         # coerce and (optionally) keep one date col
startDate       <- to_Date(startDate)
endDate         <- to_Date(endDate)

# (optional) drop/rename to keep things tidy

# 4) Verify consistent ordering/coverage
sort(unique(all_cams$Date))
sort(unique(data$DateTime))
seq(startDate, endDate, by = "day")
startDate
data$DateTime <- as.Date(data$DateTime, "%Y-%m-%d")


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
