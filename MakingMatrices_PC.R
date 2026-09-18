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

PC<-read.csv("~/Chapter_4/Cerrado/Legacy_2012-2017/RPPNPC_OC_2015_Data+Metadata_vChecked.csv")
names(PC)
length(unique(PC$File.name)) #5884
unique(PC$Species)

allcams2015<-read.csv("effort_matrix_PC.csv")

## for survey effort x wildlife images

all_cams_totals_2015<-allcams2015 %>%summarize_if(is.numeric, sum, na.rm=TRUE) #total number of days per site
all_cams_totals_2015<-as.data.frame(t(all_cams_totals_2015)) #transpose
setDT(all_cams_totals_2015, keep.rownames = TRUE[]) #renaming columns
colnames(all_cams_totals_2015) <- c("CT_site","N_Days") #renaming columns
head(all_cams_totals_2015)
nrow(allcams2015) #67

#####PREPARE MATRICES####
data<-PC
data <- rename(data, Site = Sampling.site.name) #to match function below
data <- rename(data, DateTime = Date) #change to match function

unique(sort(data$Species))
data <- data %>%
  mutate(Species = recode(Species,
                          "cavia aperea" = "Cavia aperea"
  ))
str(data)
head(allcams2015)
str(allcams2015)

allcams2015<- rename(allcams2015, Date = X) #change to match function

allcams2015$Date <- as.Date(allcams2015$Date, "%d/%m/%Y") # making sure date is Date
sort(unique(allcams2015$Date))
startDate <- as.Date("2015-08-08","%Y-%m-%d") #remove first four days of survey
allcams2015<- allcams2015[-c(1:4), ]
endDate <- as.Date("2015-10-05","%Y-%m-%d") #remove last four days of survey
allcams2015 <- allcams2015[-c(60:64), ]
sort(unique(allcams2015$Date))

str(allcams2015)
allcams2015 <- allcams2015 %>% select(order(colnames(allcams2015)))
tail(allcams2015)
str(allcams2015)

nrow(allcams2015) #59
str(startDate)
str(endDate)
str(allcams2015)
all_cams<-allcams2015 #so matches function
has_rownames(all_cams)
str(all_cams)

str(data)


# 4) Verify consistent ordering/coverage
sort(unique(all_cams$Date))
sort(unique(data$DateTime))
seq(startDate, endDate, by = "day")
data$DateTime <- as.Date(data$DateTime, format = "%d/%m/%Y")
sort(unique(data$DateTime))
startDate



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
