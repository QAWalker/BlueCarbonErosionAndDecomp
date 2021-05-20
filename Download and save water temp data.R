#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

## This script is optional to run ##

# This script uses NOAA's API to download and save water temperature data from every active NOAA CO-OPS station
# these files are available to download using a browser at 
# https://github.com/QAWalker/BlueCarbonErosionAndDecomp/tree/main/Water%20Temp%20Data

library(tidyverse)

#### Download and save water temperature data from around the CONUS ####
# read in function that calls the coops api to download the data
source(paste0(getwd(), "/NOAA.WT.R"))

# read a csv of the active stations. This will tell us which stations to download
idlist <- read.csv(file.path(getwd(), "coops-activewatertempstations.csv"), stringsAsFactors = F) %>% 
  mutate(ObjName = gsub(" ", "",.$Name, fixed =T)) %>% 
  mutate(ObjName = gsub(",", "",.$ObjName, fixed =T),
         ObjName = paste(ObjName, State, sep = ".")) %>% 
  filter(Region %in% c("Atlantic", "Gulf", "Pacific"))

# create a list to store all the data in 
WTlist <- list()
# create a list to store all the data in 
stationmetadata <- data.frame(id = NA, name = NA, lat = NA, lon = NA)
# start downloading data from Jan. 1 2010 
begin_date = (paste0("2010", "0101"))
# stop downloading data after Dec. 31 2020
end_date = (paste0("2020", "0901"))

time <- Sys.time()

# for each station id in the station list, download the data from COOPs
for (i in 1:length(idlist$ObjName)){#1:length(idlist$StationID)){
  # Prints a little message letting you know the progress of the data download
  cat(paste0("   \nStation ", idlist$StationID[[i]], ": ",idlist$Name[[i]], ", ", idlist$State[[i]], " ....."))
  # stores the temperature data in a temporary list
  temptemplist <- NOAA.WT(begin_date = begin_date, end_date = end_date, stationid = idlist$StationID[[i]])
  
  #checks that there was actually data downloaded
  if(!is.na(temptemplist)){
    #ensures that each reading is stored once and stores it
    WTlist[[i]] <- distinct(temptemplist$data)
    #names the data stored by the name of the station
    names(WTlist)[[i]] <- idlist$ObjName[[i]]
    # first checks if a station metadata object has been created and if it hasn't creates one with the data
    # if it has been created, it stores the station metadata
    if(!exists("stationmetadata")){
      stationmetadata <- as.data.frame(temptemplist$metadata)
    }else{
      stationmetadata[i,] <- temptemplist$metadata
    }
    
  }
}
Sys.time() - time

# ensure the metadata are in the right format
stationmetadata <- stationmetadata %>% 
  mutate(id = as.numeric(id), 
         lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  left_join(idlist, by = c("id" = "StationID", "name" = "Name"))

## save each station as its own csv file ####
# create a subdirectory to save the water temp data 
dir.create(file.path(getwd(), "Water Temp Data")) #will throw a warning if directory already exists, but no error

lapply(names(WTlist[which(!is.na(WTlist))]), function(nm) {
  write.csv(WTlist[[nm]],
            paste0(getwd(),"/Water Temp Data/", nm, ".csv"),
            row.names = F)
})

# save the metadata 
write.csv(stationmetadata, paste0(getwd(), "/stations metadata.csv"))
