historicstations <- read.csv(paste0(getwd(), "/active and historic stations.csv")) %>% 
  select(-starts_with("X")) %>% 
  mutate(Start = mdy(Start), 
         End = mdy(End))

historicstations <- filter(historicstations, 
                           !(ID %in% historicstations$StationID), 
                           End >= ymd("2011 - 01 - 01"), 
                           End != ymd("2020-09-01"), 
                           End - Start >= 365/2) %>% #want stations that were operating for at least 6 months
  mutate(ObjName = gsub(" ", "",.$Name, fixed =T)) %>% 
  mutate(ObjName = gsub(",", "",.$ObjName, fixed =T),
         ObjName = paste(ObjName, State, sep = ".")) %>% 
  filter(Region %in% c("Atlantic", "Gulf", "Pacific"))

# create a list to store all the data in 
WTlist.hist <- list()
# create a list to store all the data in 
# stationmetadata.hist <- data.frame(id = historicstations$ID, name = NA, lat = NA, lon = NA)
# start downloading data from Jan. 1 2010 
begin_date = (paste0("2010", "0101"))
# stop downloading data after Dec. 31 2020
end_date = (paste0("2020", "0901"))

time <- Sys.time()

# for each station id in the station list, download the data from COOPs
for (i in 1:length(historicstations$ObjName)){#1:length(historicstations$StationID)){}
  # Prints a little message letting you know the progress of the data download
  cat(paste0("   \nStation ", historicstations$ID[[i]], ": ",historicstations$Name[[i]], ", ", historicstations$State[[i]], " ....."))
  # stores the temperature data in a temporary list
  begin_date = ifelse(
    historicstations$Start[i] >= ymd("2010-01-01"),
    paste0(
      year(historicstations$Start[i]),
      ifelse(month(historicstations$Start[i])<=9, paste0("0", month(historicstations$Start[i])), month(historicstations$Start[i])),
      ifelse(day(historicstations$Start[i])<=9, paste0("0", day(historicstations$Start[i])), day(historicstations$Start[i]))
    ),
    "20100101"
  )
  
  end_date = paste0(
    year(historicstations$End[i]),
    ifelse(month(historicstations$End[i])<=9, paste0("0", month(historicstations$End[i])), month(historicstations$End[i])),
    ifelse(day(historicstations$End[i])<=9, paste0("0", day(historicstations$End[i])), day(historicstations$End[i]))
  )
  
  temptemplist <- NOAA.WT(begin_date = begin_date, end_date = end_date, stationid = historicstations$ID[[i]])
  
  #checks that there was actually data downloaded
  if(!is.na(temptemplist)){
    #ensures that each reading is stored once and stores it
    nm <- historicstations$ObjName[[i]]
    WTlist.hist[[nm]] <- distinct(temptemplist$data)
    #names the data stored by the name of the station
    # names(WTlist.hist)[[i]] <- historicstations$ObjName[[i]]
    # first checks if a station metadata object has been created and if it hasn't creates one with the data
    # if it has been created, it stores the station metadata
    if(!exists("stationmetadata.hist")){
      stationmetadata.hist <- as.data.frame(temptemplist$metadata)
    }else{
      stationmetadata.hist <- bind_rows(stationmetadata.hist, temptemplist$metadata)
    }
    
  }
}
Sys.time() - time

# WTlist.hist <- WTlist[which(!(names(WTlist.hist)%in%c("", "NULL")))]
# historicstations <- filter(historicstations, ObjName %in% names(WTlist.hist))

histStationswithData <- with(historicstations, ObjName[which(ObjName %in% names(WTlist.hist))])

# ensure the metadata are in the right format
stationmetadata.hist <- stationmetadata.hist %>% 
  filter(!is.na(id)) %>% 
  mutate(id = as.numeric(id), 
         lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  left_join(historicstations, by = c("id" = "ID", "name" = "Name"))

#figure out how many 6 minute readings should have happened in each year (changes based on leap years)
daysinayear <- data.frame(day = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by="1 day")) %>% 
  mutate(year = year(day)) %>% 
  {as.data.frame(table(.[, c('year')]))} %>% 
  mutate(hours = Freq*24, 
         minutes = hours*60, 
         sixminute = minutes/6)

# selects all the years at each site that have greater coverage than 75% of all readings
monthyearslist.hist <- data.frame(ObjName = NULL, Month= NULL, Year= NULL, Readings= NULL, readings.pct= NULL)
# this loop calculates the number of readings that were collected in each 12 month period and stores the number in a data.frame called monthyearslist
for(i in 1:length(WTlist.hist)){
  nm <- names(WTlist.hist[i])
  # prints to the console so that you know that something is happening behind the scenes
  cat(paste(nm, ""))
  if(!is.null(WTlist.hist[[i]])){
    if(!is.na(WTlist.hist[[i]])){
      monthyearslist.hist <- bind_rows(
        monthyearslist.hist,
        data.frame(ObjName=nm, date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by="1 month")) %>% 
          mutate(Month = month(date), Year = year(date), 
                 # calculate recall the # of potential 6min readings that happened in the 12 months (any month that starts after feb. you have to look at the total the next year)
                 potentialreadings = unlist(lapply(Year, function(yr) daysinayear$sixminute[daysinayear$Var1==yr])), 
                 # count the number of readings in the 12 months following each month 
                 numreadings = unlist(lapply(date, function(month){length(filter(WTlist.hist[[i]], t >= month, t < month + years(1))$v)})),
                 # determine the pct of potential readings that were actually recorded
                 readings.pct = numreadings/potentialreadings))
    }
  }
}

# find the best (most temporally complete) year at each station  
bestyears.hist <- lapply(unique(monthyearslist.hist$ObjName), function(nm){
  maxreading = max(monthyearslist.hist[monthyearslist.hist$ObjName==nm, "readings.pct"], na.rm = T)
  monthyearslist.hist %>% 
    arrange(desc(Year), desc(Month)) %>% 
    filter(., ObjName==nm, readings.pct==maxreading) %>% 
    .[1,]
})

# turn the output list from above into a df
bestyears.hist <- as.data.frame(do.call(rbind, bestyears.hist)) 
# remove the unneeded columns
bestyears.hist <- select(bestyears.hist, -c("date", "potentialreadings"))
#join the best year data with the station metadata
stationmetadata.hist <- stationmetadata.hist %>% 
  left_join(., bestyears.hist, by = c("ObjName"))
# create a list to store the full water temp data for the most complete years
WTlist.bestyears.hist <- list()
# create a df to store the count of six minute readings at each temperature
tempcount.hist <- data.frame(temp = -5:50)

# loop through each station and store the data into the WTlist.bestyears list and count the readings at each temperature
for (i in 1:length(WTlist.hist)) {
  nm <- names(WTlist.hist[i])
  m <- stationmetadata.hist$Month[stationmetadata.hist$ObjName==nm&!is.na(stationmetadata.hist$ObjName)]
  yr <- stationmetadata.hist$Year[stationmetadata.hist$ObjName==nm&!is.na(stationmetadata.hist$ObjName)]
  if(!is_empty(yr)&!is.na(nm)){
    WTlist.bestyears.hist[[nm]] <- filter(WTlist.hist[[i]], t>=mdy(paste(m, 1, yr))&t<mdy(paste(m, 1, yr+1)))
    # create a new variable to store the temperature rounded to the nearest interger
    WTlist.bestyears.hist[[nm]]$v.rounded <- round(WTlist.bestyears.hist[[nm]]$v,0)
    # create a temporary df 'x' that stores the count data
    x <- data.frame(table(WTlist.bestyears.hist[[nm]][, "v.rounded"]))
    for(j in unique(as.numeric(as.character(x$Var1)))){
      #store the count data into the tempcount df
      tempcount.hist[tempcount.hist$temp == j, paste0(nm)] <- x$Freq[x$Var1==j]
    }
  }
}

# set all the NAs in the tempcount df to 0s
for (nm in names(tempcount.hist)) {
  tempcount.hist[is.na(tempcount.hist[,nm]), nm] <- 0
}

#### calculate the decomposition #####
# this function is from https://www.r-bloggers.com/easy-error-propagation-in-r/
# it allows error to be propagated through calculations 
# error for each var is denoted by a 'd' preceding the variable name i.e. error for 'mean' is 'dmean'
mutate_with_error = function(.data, f) {
  exprs = list(
    # expression to compute new variable values
    deparse(f[[3]]),
    
    # expression to compute new variable errors
    sapply(all.vars(f[[3]]), function(v) {
      dfdp = deparse(D(f[[3]], v))
      sprintf('(d%s*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('d%s', deparse(f[[2]]))
  )
  
  .data %>%
    # the standard evaluation alternative of mutate()
    mutate_(.dots=exprs)
}

## constants calculated from experiments
R <- .00831446 #kJ mol^-1 K^-1 (universal gas constant)
K2 <- c(840.5/1000000, 1018/1000000) #mean mol C mol C^-1 d^-1 deep, shallow sediment respectively
dK2 <- c(82.4/1000000, 166/1000000) #sd mol C mol C^-1 d^-1 deep, shallow sediment respectively
K1 <- c(379.4/1000000, 518/1000000) #mean mol C mol C^-1 d^-1 deep, shallow sediment respectively
dK1 <- c(92.3/1000000, 32.4/1000000) #sd mol C mol C^-1 d^-1 deep, shallow sediment respectively
T2 <- 30 + 273.15 #Kelvin
T1 <- 20 + 273.15 #Kelvin

# Create a data frame to feed to the error propagation function. 
# Each object, even the constants needs to have an error, I used 0 for the constants
Kvals.hist <- data.frame(temp = rep(-5:50, each = 2)+273.15, dtemp = 0, # no error for temp
                    temp.C = rep(-5:50, each = 2), dtemp.C = 0, # no error for temp
                    depth = c("deep", "shallow"),
                    K1, dK1, 
                    K2, dK2, 
                    R, dR = 0, # no error for gas constant
                    T1, dT1 = 0, # no error for temp
                    T2, dT2 = 0) %>%  # no error for temp
  # calc activation energy and propagate the error
  mutate_with_error(Ea ~ (log(K2)-log(K1))*R/((1/T2)-(1/T1))*(-1)) %>% # kJ mol^-1
  # calc the K value at each temperature and propagate the error
  mutate_with_error(Ktemp ~ (-1*(Ea/R)*((1/temp)-(1/T1)))) %>% 
  mutate_with_error(Ktemp ~ exp(Ktemp+log(K1))) #not sure why but have to seperate these two steps

# sort the kvals df into a df that is interpretable 
Arrmodel.hist <- data.frame(filter(Kvals.hist, depth=="deep")[,c("temp", "temp.C")], 
                       filter(Kvals.hist, depth=="deep")[,c("Ktemp", "dKtemp")], 
                       filter(Kvals.hist, depth=="shallow")[,c("Ktemp", "dKtemp")])
# rename
names(Arrmodel.hist) <- c("temp", "temp.C",
                     "Ktemp.deep", "dKtemp.deep",
                     "Ktemp.shallow", "dKtemp.shallow")

## This is how I was calculating the K vals without error propagation
#
# Ea <-  (log(K2)-log(K1))*R/((1/T2)-(1/T1))*(-1) # kJ mol^-1
# ## new df to store the arrhenius kenetics model equations and calculations
# Arrmodel <-  data.frame(Temp = c(-5:50), K2.deep = (0), K2.shallow = (0))
# ## loop through temperatures -5 to 50 calculating the K for both sediment depths at each temp
# for(i in -5:50){
#   Arrmodel[Arrmodel$Temp==i, "K2.deep"] <- exp(-1*(Ea[[1]]/R)*((1/(i+273))-(1/T1))+log(K1[[1]]))
#   Arrmodel[Arrmodel$Temp==i, "K2.shallow"] <- exp(-1*(Ea[[2]]/R)*((1/(i+273))-(1/T1))+log(K1[[2]]))
# }
# store the portion of C respired at each temperature for a year
i = 2

decomp.list.hist <- list()
decomp.list.hist <- lapply(1:length(bestyears.hist$ObjName), function(i){
  # nm <- bestyears$ObjName[i]
  # if()
  Arrmodel.hist %>% 
    mutate(count = tempcount.hist[,i],
           days = count *  6/60/24, 
           ddays = 0) %>% 
    mutate_with_error(decomp.deep ~ days * Ktemp.deep*1000) %>% # mmol C mol^-1 C 
    mutate_with_error(decomp.shallow ~ days * Ktemp.shallow*1000) %>% 
    select(starts_with("temp"), "days", starts_with("decomp"), starts_with("ddecomp"))
})
# rename
names(decomp.list.hist) <- bestyears.hist$ObjName 

# find the portion of C respired from all temperatures for the year
totalDecomp.hist <- list()
totalDecomp.hist <- lapply(names(decomp.list.hist), function(nm){
  data.frame(ObjName = nm, 
             totalDecomp.deep =  sum(decomp.list.hist[[nm]]$decomp.deep), # sum the portion of C respired at each temperature
             sdDecomp.deep = sqrt(sum((decomp.list.hist[[nm]]$ddecomp.deep)^2)), # manually propagate the error
             totalDecomp.shallow =  sum(decomp.list.hist[[nm]]$decomp.shallow),
             sdDecomp.shallow = sqrt(sum((decomp.list.hist[[nm]]$ddecomp.shallow)^2))) # manually propagate the error
})
totalDecomp.hist <- as.data.frame(do.call(rbind, totalDecomp.hist))
#join the total decomp to the metadata list
stationmetadata.hist <- left_join(stationmetadata.hist, totalDecomp.hist)
stationmetadata.all <- bind_rows(stationmetadata, stationmetadata.hist)
#Plot
plotdata <- filter(stationmetadata.all, 
                   # Region == "Pacific",
                   readings.pct >= 0.9)

ggplot(data = world) +
  geom_sf(color = "transparent")+
  # geom_sf(data = states1, fill = "transparent", color = "gray70")+
  # geom_sf(data = states2, aes(fill = meanDecomp.deep/10), color = "white", size = 0.0025)+
  # geom_sf(data = coast, color = "gray50")+
  geom_point(data = plotdata,
             aes(x = as.numeric(lon), y = as.numeric(lat), fill = totalDecomp.deep/10), 
             size = 3, color = "black", shape = 21)+
  coord_sf(xlim = c(min(as.numeric(plotdata$lon), na.rm = T), max(as.numeric(plotdata$lon), na.rm = T)), 
           ylim = c(min(as.numeric(plotdata$lat), na.rm = T), max(as.numeric(plotdata$lat), na.rm = T)), expand = T)+
  # scale_color_continuous(limits = c(0.85, 1.005))+
  scale_fill_continuous(type = "viridis", option = "C", limits = c(0, 40))+
  labs(x = NULL, y = NULL, fill = "Annual Decomposition (%C)")+
  theme_bw()
