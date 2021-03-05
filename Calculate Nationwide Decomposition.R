##################
# the start of this script is designed to be run without downloading the data
require(tidyverse)
require(lubridate)
# make sure your working directory is the place where the data is stored
getwd()
# set the working directory to where you have the stations csv file saved 
# setwd("C:/Users/Quentin.Walker/R/Projects/MarshSedDecomp")

# the start of this script is designed to be run without downloading the data
# Load the station list
idlist <- read.csv(paste0(getwd(), "/coops-activewatertempstations.csv"), stringsAsFactors = F) %>% 
  mutate(ObjName = gsub(" ", "",.$Name, fixed =T)) %>% 
  mutate(ObjName = gsub(",", "",.$ObjName, fixed =T),
         ObjName = paste(ObjName, State, sep = ".")) %>% 
  filter(Region %in% c("Atlantic", "Gulf", "Pacific"))

# Load the metadata
stationmetadata <- read.csv(paste0(getwd(), "/stations metadata.csv")) %>% 
  filter(!is.na(State))

# load the previously downloaded water temperature data
WTlist <- lapply(
  idlist$ObjName,
  function(on){
    tryCatch({
      read.csv(paste0(getwd(), "/Water Temp Data/", on,".csv"), stringsAsFactors = F) %>% 
        as_tibble() %>% 
        mutate(t = ymd_hms(t))
    }, error = function(e){
      message(e)
      cat(paste0(" ", on, "   \n"))
      WTlist[[on]] <- NA
    })
    }
  )
names(WTlist) <- idlist$ObjName
#### here's where the analysis starts ####
## Find the most complete years including starting on the first of each month in that dataset

#figure out how many 6 minute readings should have happened in each year (changes based on leap years)
daysinayear <- data.frame(day = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by="1 day")) %>% 
  mutate(year = year(day)) %>% 
  {as.data.frame(table(.[, c('year')]))} %>% 
  mutate(hours = Freq*24, 
         minutes = hours*60, 
         sixminute = minutes/6)

monthyearslist <- data.frame(ObjName = NULL, Month= NULL, Year= NULL, Readings= NULL, readings.pct= NULL)
# this loop calculates the number of readings that were collected in each 12 month period and stores the number in a data.frame called monthyearslist
for(i in 1:length(WTlist)){
  nm <- names(WTlist[i])
  # prints to the console so that you know that something is happening behind the scenes
  cat(paste(nm, ""))
  if(!is.null(WTlist[[i]])){
    if(!is.na(WTlist[[i]])){
    monthyearslist <- bind_rows(
      monthyearslist,
      data.frame(ObjName=nm, date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by="1 month")) %>% 
        mutate(Month = month(date), Year = year(date), 
               # calculate recall the # of potential 6min readings that happened in the 12 months 
               # (any year that starts after feb. you have to look at the total the next year)
               potentialreadings = ifelse(Month<=2, 
                                          unlist(lapply(Year, function(yr) daysinayear$sixminute[daysinayear$Var1==yr])),
                                          unlist(lapply(Year, function(yr) daysinayear$sixminute[daysinayear$Var1==yr+1]))),
               # count the number of readings in the 12 months following each month 
               numreadings = unlist(lapply(date, function(month){length(filter(WTlist[[i]], t >= month, t < month + years(1))$v)})),
               # determine the pct of potential readings that were actually recorded
               readings.pct = numreadings/potentialreadings))
    }
  }
}

# find the best (most temporally complete) year at each station  
bestyears <- lapply(unique(monthyearslist$ObjName), function(nm){
  maxreading = max(monthyearslist[monthyearslist$ObjName==nm, "readings.pct"], na.rm = T)
  monthyearslist %>% 
    arrange(desc(Year), desc(Month)) %>% 
    filter(., ObjName==nm, readings.pct==maxreading) %>% 
    .[1,]
})
# turn the output list from above into a df
bestyears <- as.data.frame(do.call(rbind, bestyears)) 
# remove the unneeded columns
bestyears <- select(bestyears, -c("date"))
#join the best year data with the station metadata
stationmetadata <- stationmetadata %>% 
  left_join(., bestyears)
# create a list to store the full water temp data for the most complete years
WTlist.bestyears <- list()
# create a df to store the count of six minute readings at each temperature
tempcount <- data.frame(temp = -5:50)

# loop through each station and store the data into the WTlist.bestyears list and count the readings at each temperature
for (i in 1:length(WTlist)) {
  nm <- names(WTlist[i])
  m <- stationmetadata$Month[stationmetadata$ObjName==nm&!is.na(stationmetadata$ObjName)]
  yr <- stationmetadata$Year[stationmetadata$ObjName==nm&!is.na(stationmetadata$ObjName)]
  if(!is_empty(yr)&!is.na(nm)){
    WTlist.bestyears[[nm]] <- filter(WTlist[[i]], t>=mdy(paste(m, 1, yr))&t<mdy(paste(m, 1, yr+1)))
    # create a new variable to store the temperature rounded to the nearest interger
    WTlist.bestyears[[nm]]$v.rounded <- round(WTlist.bestyears[[nm]]$v,0)
    # create a temporary df 'x' that stores the count data
    x <- data.frame(table(WTlist.bestyears[[nm]][, "v.rounded"]))
    for(j in unique(as.numeric(as.character(x$Var1)))){
      #store the count data into the tempcount df
      tempcount[tempcount$temp == j, paste0(nm)] <- x$Freq[x$Var1==j]
    }
  }
}
rm(x)
# set all the NAs in the tempcount df to 0s
for (nm in names(tempcount)) {
  tempcount[is.na(tempcount[,nm]), nm] <- 0
}

#### determine the size of the gaps in the best years ####
gapdf <- 
  lapply(
    names(WTlist.bestyears),
    function(nm){
      t <- mutate(WTlist.bestyears[[nm]], d = as.difftime(t - lag(t, ), units = "mins"))$d %>% 
        table() %>% as.data.frame() %>% 
        t() %>% as.data.frame()
      
      names(t) <- t[1,]
      
      t <- t[2,] %>% 
        mutate_all(as.numeric) %>% 
        mutate(Name = nm) %>% 
        select("Name", everything())
      
      return(t)
    }
  )

# take the output list and put it into a dataframe
gapdf <- as.data.frame(do.call(bind_rows, gapdf)) 
# order the columns from low to high, left to right
gapdf <- gapdf[,c(1,names(gapdf)[names(gapdf)!="Name"] %>% 
                    as.numeric() %>% 
                    order()+1)]
# create a melted df to store the gaps, which will be easier to use to retrieve the info
gapdf.melt <- reshape2::melt(gapdf, id.vars = "Name", na.rm = T, value.name = "count", variable.name = "gap") %>% 
  mutate(gap = as.numeric(as.character(gap)))
# Summary df to add to the main station metadata df
gapdf.summary <- gapdf.melt %>% 
  group_by(Name) %>% 
  summarize(largestGap = max(gap, na.rm = T), 
            largestGapCount = count[which(gap==max(gap))], 
            sixMinGaps = count[which(gap == 6)], 
            sixMinGaps.pct = sixMinGaps/sum(count)) %>% 
  ungroup()
# add to the main station metdata df
stationmetadata <- left_join(stationmetadata, gapdf.summary, by = c("ObjName" = "Name"))

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
Kvals <- data.frame(temp = rep(-5:50, each = 2)+273.15, dtemp = 0, # no error for temp
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
  mutate_with_error(Ktemp ~ exp(Ktemp+log(K1))) #not sure why but have to separate these two steps

# sort the kvals df into a df that is interpretable 
Arrmodel <- data.frame(filter(Kvals, depth=="deep")[,c("temp", "temp.C")], 
                       filter(Kvals, depth=="deep")[,c("Ktemp", "dKtemp")], 
                       filter(Kvals, depth=="shallow")[,c("Ktemp", "dKtemp")])
# rename
names(Arrmodel) <- c("temp", "temp.C",
                     "Ktemp.deep", "dKtemp.deep",
                     "Ktemp.shallow", "dKtemp.shallow")

# store the portion of C respired at each temperature for a year
decomp.list <- list()
decomp.list <- lapply(2:length(names(tempcount)), function(i){
  Arrmodel %>% 
    mutate(name.tc = names(tempcount)[i],
           count = tempcount[,i],
           days = count *  6/60/24, 
           ddays = 0) %>% 
    mutate_with_error(decomp.deep ~ days * Ktemp.deep) %>% # mol C mol^-1 C 
    mutate_with_error(decomp.shallow ~ days * Ktemp.shallow) %>% 
    select(starts_with("temp"), "days", starts_with("decomp"), starts_with("ddecomp"))
})
# rename
names(decomp.list) <- names(tempcount)[2:length(names(tempcount))]

# find the portion of C respired from all temperatures for the year
totalDecomp <- list()
totalDecomp <- lapply(names(decomp.list), function(nm){
  data.frame(ObjName = nm, 
             totalDecomp.deep =  sum(decomp.list[[nm]]$decomp.deep), # sum the portion of C respired at each temperature
             sdDecomp.deep = sqrt(sum((decomp.list[[nm]]$ddecomp.deep)^2)), # manually propagate the error
             totalDecomp.shallow =  sum(decomp.list[[nm]]$decomp.shallow),
             sdDecomp.shallow = sqrt(sum((decomp.list[[nm]]$ddecomp.shallow)^2))) # manually propagate the error
})
totalDecomp <- as.data.frame(do.call(rbind, totalDecomp))
#join the total decomp to the metadata list
stationmetadata <- left_join(stationmetadata, totalDecomp) 

#### normalize the total decomp to a year ####
# divide the total decomp in the selected year by the pct of the readings recorded
stationmetadata <- stationmetadata %>% 
  mutate(across(contains("Decomp"), 
                function(x, pct, potentialreadings) ifelse(potentialreadings == 87840, (x/pct)*365/366, x/pct),
                pct = readings.pct, potentialreadings = potentialreadings))

######## Calculate sediment loss from erosion and the carbon decomposed because of that #######
##### read in the Gittman 2015 marsh shoreline length data ####
shorelinelengths <- read.csv(paste0(getwd(), "/Gittman 2015.csv")) %>% 
  select('Region', "State" = 'Abb', "MarshShore" = "Marsh.shore..km.") 

shorelinelengths$MarshShore[shorelinelengths$State=="LA"] = 66459
shorelinelengths <- distinct(shorelinelengths)

#estimate for bank height
BankHt <- c(0.0003, 0.0001) # km large and small estimate
#estimate for bank erosion rate
ErosionRate <- c(0.0003, 0.0001) # km/yr, large and small estimate
#marsh carbon density
Cdensity <- 27 # kg C / m^3 
dCdensity <- 13 # kg C / m^3; (n = 8280)

# Create a summary 
station.decomp.summary <-
  lapply(
    unique(
      paste(stationmetadata$State, stationmetadata$Region)
      ),
  function(x) {
    x <- unlist(strsplit(x, split = " "))
    df <-
      filter(stationmetadata, State == x[1], Region == x[2], readings.pct >=
               0.95)
    y <- data.frame(
      State = x[1],
      Region = x[2],
      n = length(df[, 1]),
      meanDecomp.deep = mean(df$totalDecomp.deep, na.rm = T),
      meanDecomp.shallow = mean(df$totalDecomp.shallow, na.rm = T)
    ) %>%
      mutate(
        dDecomp.deep = sqrt(sum(df$sdDecomp.deep^2, na.rm = T))/length(df$totalDecomp.deep),
        dDecomp.shallow =sqrt(sum(df$sdDecomp.shallow^2, na.rm = T))/length(df$totalDecomp.shallow)
      )
      # mutate(
      #   dDecomp.deep = abs(meanDecomp.deep) * sqrt(sum(df$sdDecomp.deep^2, na.rm = T)) / sum(df$totalDecomp.deep, na.rm = T),
      #   dDecomp.shallow = abs(meanDecomp.shallow) * sqrt(sum(df$sdDecomp.shallow^2, na.rm = T)) / sum(df$totalDecomp.shallow, na.rm = T)
      # )
      # mutate(
      #   dDecomp.deep = abs(meanDecomp.deep) * sqrt(sum((df$sdDecomp.deep / df$totalDecomp.deep) ^
      #                                                    2, na.rm = T
      #   )),
      #   dDecomp.shallow = abs(meanDecomp.shallow) * sqrt(sum((df$sdDecomp.shallow /
      #                                                           df$totalDecomp.shallow) ^ 2, na.rm = T
      #   ))
      # )
    return(y)
  })

station.decomp.summary <- as.data.frame(do.call(rbind, station.decomp.summary)) %>% 
  select("State", "Region", "n", contains("deep"), contains("shallow"))

# apply shallow rate to shallow part (0 - 10 cm) and deep to rest (11 - 30 cm)
statedecomp <- shorelinelengths %>% 
  left_join(station.decomp.summary) %>% 
  rename(Decomp.deep = meanDecomp.deep, Decomp.shallow = meanDecomp.shallow) %>% 
  #marsh area eroded by large and small erosion rate
  mutate(erodedSedArea.Large = (ErosionRate[1] * 1000) * (MarshShore * 1000) , # m^2/yr
         erodedSedArea.Small = (ErosionRate[2] * 1000) * (MarshShore * 1000)) %>% # m^2/yr
  #marsh sed volume of the deep and shallow portions
  mutate(erodedSedVolume.LargeShallow = erodedSedArea.Large * BankHt[2]*1000, # m^3/yr
         erodedSedVolume.SmallShallow = erodedSedArea.Small * BankHt[2]*1000, # m^3/yr
         erodedSedVolume.LargeDeep = erodedSedArea.Large * (BankHt[1]*1000 - BankHt[2]*1000), # m^3/yr
         erodedSedVolume.SmallDeep = erodedSedArea.Small * (BankHt[1]*1000 - BankHt[2]*1000)) %>%  # m^3/yr
  #total marsh sed volume of the tall and short bank heights
  mutate(erodedSedVolume.LargeTall = erodedSedVolume.LargeShallow + erodedSedVolume.LargeDeep,
         erodedSedVolume.SmallTall = erodedSedVolume.SmallShallow + erodedSedVolume.SmallDeep,
         erodedSedVolume.LargeShort = erodedSedVolume.LargeShallow,
         erodedSedVolume.SmallShort = erodedSedVolume.SmallShallow) %>% 
  #carbon in the shallow and deep portions
  mutate(erodedSedC.LargeShallow = erodedSedVolume.LargeShallow * Cdensity, # kg C/yr
         derodedSedC.LargeShallow = 0, # kg C/yr
         erodedSedC.SmallShallow = erodedSedVolume.SmallShallow * Cdensity, # kg C/yr
         derodedSedC.SmallShallow = 0, # kg C/yr
         erodedSedC.LargeDeep = erodedSedVolume.LargeDeep * Cdensity, # kg C/yr
         derodedSedC.LargeDeep = 0, # kg C/yr
         erodedSedC.SmallDeep = erodedSedVolume.SmallDeep * Cdensity, # kg C/yr
         derodedSedC.SmallDeep = 0) %>% # kg C/yr
  # total carbon of the tall and short bank heights
  mutate(erodedSedC.LargeTall = erodedSedC.LargeShallow + erodedSedC.LargeDeep,
         erodedSedC.SmallTall = erodedSedC.SmallShallow + erodedSedC.SmallDeep,
         erodedSedC.LargeShort = erodedSedC.LargeShallow,
         erodedSedC.SmallShort = erodedSedC.SmallShallow) %>%
  
  mutate_with_error(Decomp.LargeShallow ~ erodedSedC.LargeShallow * Decomp.shallow) %>% 
  mutate_with_error(Decomp.SmallShallow ~ erodedSedC.SmallShallow * Decomp.shallow) %>% 
  mutate_with_error(Decomp.LargeDeep ~ erodedSedC.LargeDeep * Decomp.deep) %>% 
  mutate_with_error(Decomp.SmallDeep ~ erodedSedC.SmallDeep * Decomp.deep) %>% 
  
  mutate(Decomp.LargeShort = Decomp.LargeShallow,
         dDecomp.LargeShort = dDecomp.LargeShallow,
         Decomp.LargeTall = Decomp.LargeShallow + Decomp.LargeDeep,
         dDecomp.LargeTall = sqrt((dDecomp.LargeShallow)^2 + (dDecomp.LargeDeep)^2),
         Decomp.SmallShort = Decomp.SmallShallow,
         dDecomp.SmallShort = dDecomp.SmallShallow,
         Decomp.SmallTall = Decomp.SmallShallow + Decomp.SmallDeep,
         dDecomp.SmallTall = sqrt((dDecomp.LargeShallow)^2 + (dDecomp.LargeDeep)^2))

regiondecomp <- statedecomp %>% 
  #filter(Region == "Gulf", MarshShore != 1551) %>% 
  #mutate(Region = paste0(Region, "1986")) %>%
  #bind_rows(filter(statedecomp, MarshShore != 73745)) %>%
  group_by(Region) %>% 
  summarize(MarshShore = sum(MarshShore),
            n = sum(n, na.rm = T),
            #volume for the Short and Tall banks
            erodedSedVolume.LargeShort = sum(erodedSedVolume.LargeShort, na.rm = T), 
            erodedSedVolume.SmallShort = sum(erodedSedVolume.SmallShort, na.rm = T),
            erodedSedVolume.LargeTall = sum(erodedSedVolume.LargeTall, na.rm = T),
            erodedSedVolume.SmallTall = sum(erodedSedVolume.SmallTall, na.rm = T),
            # carbon for the Short and Tall banks
            erodedSedC.LargeShort = sum(erodedSedC.LargeShort, na.rm = T), 
            erodedSedC.SmallShort = sum(erodedSedC.SmallShort, na.rm = T),
            erodedSedC.LargeTall = sum(erodedSedC.LargeTall, na.rm = T),
            erodedSedC.SmallTall = sum(erodedSedC.SmallTall, na.rm = T),
            
            Decomp.LargeShort = sum(Decomp.LargeShort, na.rm = T), # kg C/yr
            dDecomp.LargeShort = sqrt(sum(dDecomp.LargeShort^2, na.rm = T)), # kg C/yr
            
            Decomp.LargeTall = sum(Decomp.LargeTall, na.rm = T), # kg C/yr
            dDecomp.LargeTall = sqrt(sum(dDecomp.LargeTall^2, na.rm = T)), # kg C/yr
            
            Decomp.SmallShort = sum(Decomp.SmallShort, na.rm = T), # kg C/yr
            dDecomp.SmallShort = sqrt(sum(dDecomp.SmallShort^2, na.rm = T)), # kg C/yr
            
            Decomp.SmallTall = sum(Decomp.SmallTall, na.rm = T), # kg C/yr
            dDecomp.SmallTall = sqrt(sum(dDecomp.SmallTall^2, na.rm = T))) %>% # kg C/yr
  ungroup()

nationaldecomp <-
  statedecomp %>% 
  #filter(MarshShore != 1551) %>% 
  #mutate(Region = paste0(Region, "1986"), national = "1986") %>%
  #bind_rows(filter(statedecomp, MarshShore != 73745) %>% mutate(national = "2015")) %>%
  #group_by(national) %>% 
  summarize(MarshShore = sum(MarshShore),
            n = sum(n, na.rm = T),
            
            erodedSedVolume.LargeShort = sum(erodedSedVolume.LargeShort, na.rm = T), 
            erodedSedVolume.SmallShort = sum(erodedSedVolume.SmallShort, na.rm = T),
            erodedSedVolume.LargeTall = sum(erodedSedVolume.LargeTall, na.rm = T),
            erodedSedVolume.SmallTall = sum(erodedSedVolume.SmallTall, na.rm = T),
            
            erodedSedC.LargeShort = sum(erodedSedC.LargeShort, na.rm = T), 
            erodedSedC.SmallShort = sum(erodedSedC.SmallShort, na.rm = T),
            erodedSedC.LargeTall = sum(erodedSedC.LargeTall, na.rm = T),
            erodedSedC.SmallTall = sum(erodedSedC.SmallTall, na.rm = T),
            
            Decomp.LargeShort = sum(Decomp.LargeShort, na.rm = T), # kg C/yr
            dDecomp.LargeShort = sqrt(sum(dDecomp.LargeShort^2, na.rm = T)), # kg C/yr
            
            Decomp.LargeTall = sum(Decomp.LargeTall, na.rm = T), # kg C/yr
            dDecomp.LargeTall = sqrt(sum(dDecomp.LargeTall^2, na.rm = T)), # kg C/yr
            
            Decomp.SmallShort = sum(Decomp.SmallShort, na.rm = T), # kg C/yr
            dDecomp.SmallShort = sqrt(sum(dDecomp.SmallShort^2, na.rm = T)), # kg C/yr
            
            Decomp.SmallTall = sum(Decomp.SmallTall, na.rm = T), # kg C/yr
            dDecomp.SmallTall = sqrt(sum(dDecomp.SmallTall^2, na.rm = T))) %>% # kg C/yr
  ungroup()# %>% 
  # rename("LAestYear" = "national")
#### create a df for an erosion and decomp summary ####
statedecomp.melt <- reshape2::melt(statedecomp, 
                                   id.vars = c("Region", "State", "MarshShore", "n"), 
                                   measure.vars = names(statedecomp)[grepl(pattern = "Tall|Short", names(statedecomp))]) %>% 
  mutate(BankHt = grepl(x = variable, pattern = "Short"),
         ErosionRate = grepl(x = variable, pattern = "Large"),
         sedVol = grepl(x = variable, pattern = "Volume"),
         sedC = grepl(x = variable, pattern = "SedC"),
         SD = grepl(x = variable, patter = "dD"),
         BankHt = ifelse(BankHt, "Shallow", "Deep"),
         ErosionRate = ifelse(ErosionRate, "Large", "Small"))

statedecomp.melt <- left_join(
  filter(statedecomp.melt, !SD, !sedVol, !sedC) %>% 
    select(-c("SD", "variable"), "Decomp" = "value"),
  filter(statedecomp.melt, SD, !sedVol, !sedC) %>% 
    select(-c("n", "variable", "SD"), "SD" = "value"),
  by = c("Region", "State", "MarshShore", "BankHt", "ErosionRate")) %>% 
  left_join(
    filter(statedecomp.melt, !SD, sedVol, !sedC) %>% 
      select(-c("n", "variable", "SD"), "erodedSedVolume" = "value"),
    by = c("Region", "State", "MarshShore", "BankHt", "ErosionRate")) %>% 
  left_join(
    filter(statedecomp.melt, !SD, !sedVol, sedC) %>% 
      select(-c("n", "variable", "SD"), "erodedSedCarbon" = "value"),
    by = c("Region", "State", "MarshShore", "BankHt", "ErosionRate")) %>% 
  select("Region", "State", "MarshShore", "n", "BankHt", "ErosionRate", "erodedSedVolume", "erodedSedCarbon", "Decomp", "SD")

regiondecomp.melt <- reshape2::melt(regiondecomp, 
                                   id.vars = c("Region", "MarshShore", "n")) %>% 
  mutate(BankHt = grepl(x = variable, pattern = "Short"),
         ErosionRate = grepl(x = variable, pattern = "Large"),
         sedVol = grepl(x = variable, pattern = "Volume"),
         sedC = grepl(x = variable, pattern = "SedC"),
         SD = grepl(x = variable, patter = "dD"),
         BankHt = ifelse(BankHt, "Shallow", "Deep"),
         ErosionRate = ifelse(ErosionRate, "Large", "Small"))

regiondecomp.melt <- 
  left_join(
    filter(regiondecomp.melt, !SD, !sedVol, !sedC) %>% 
      select(-c("SD", "variable", "sedVol", "sedC"), "Decomp" = "value"),
    filter(regiondecomp.melt, SD, !sedVol, !sedC) %>% 
      select(-c("n", "variable", "SD", "sedVol", "sedC"), "SD" = "value"),
    by = c("Region", "MarshShore", "BankHt", "ErosionRate")) %>% 
  left_join(
    filter(regiondecomp.melt, !SD, sedVol, !sedC) %>% 
      select(-c("n", "variable", "SD", "sedVol", "sedC"), "erodedSedVolume" = "value"),
    by = c("Region", "MarshShore", "BankHt", "ErosionRate")) %>% 
  left_join(
    filter(regiondecomp.melt, !SD, !sedVol, sedC) %>% 
      select(-c("n", "variable", "SD"), "erodedSedCarbon" = "value"),
    by = c("Region", "MarshShore", "BankHt", "ErosionRate")) %>% 
  select("Region", "MarshShore", "n", "BankHt", "ErosionRate", "erodedSedVolume", "erodedSedCarbon", "Decomp", "SD")


nationaldecomp.melt <- reshape2::melt(nationaldecomp, id.vars = c("MarshShore", "n")) %>% 
  mutate(BankHt = grepl(x = variable, pattern = "Short"),
         ErosionRate = grepl(x = variable, pattern = "Large"),
         sedVol = grepl(x = variable, pattern = "Volume"),
         sedC = grepl(x = variable, pattern = "SedC"),
         SD = grepl(x = variable, patter = "dD"),
         BankHt = ifelse(BankHt, "Shallow", "Deep"),
         ErosionRate = ifelse(ErosionRate, "Large", "Small"))

nationaldecomp.melt <-
  left_join(
    filter(nationaldecomp.melt, !SD, !sedVol, !sedC) %>%
      select(-c("SD", "variable", "sedVol", "sedC"), "Decomp" = "value"),
    filter(nationaldecomp.melt, SD, !sedVol, !sedC) %>% 
      select(-c("n", "variable", "SD", "sedVol", "sedC"), "SD" = "value"),
    by = c("MarshShore", "BankHt", "ErosionRate")) %>%
  left_join(
    filter(nationaldecomp.melt, !SD, sedVol, !sedC) %>%
      select(-c("n", "variable", "SD", "sedVol", "sedC"), "erodedSedVolume" = "value"),
    by = c("MarshShore", "BankHt", "ErosionRate")) %>%
  left_join(
    filter(nationaldecomp.melt, !SD, !sedVol, sedC) %>%
      select(-c("n", "variable", "SD"), "erodedSedCarbon" = "value"),
    by = c("MarshShore", "BankHt", "ErosionRate")) %>%
  mutate(Region = "National") %>%
  select("Region", "MarshShore", "n", "BankHt", "ErosionRate", "erodedSedVolume", "erodedSedCarbon", "Decomp", "SD")


ErosionAndDecompSummary <- bind_rows(statedecomp.melt, regiondecomp.melt, nationaldecomp.melt)
needtosave = F
if(needtosave)  {
  #### Save results #### 
  stationmetadata %>%
    # give the results names that are human readable with units
    select(everything(),
           "Deep Sed C Annual Decomp (mol C/mol C)" = "totalDecomp.deep", "Deep Sed C Annual Decomp SD (mol C/mol C)" = "sdDecomp.deep",
           "Shallow Sed C Annual Decomp (mol C/mol C)" = "totalDecomp.shallow", "Shallow Sed C Annual Decomp SD (mol C/mol C)" = "sdDecomp.shallow",
           ) %>% 
    write.csv(paste0(getwd(), "/results/station metadata and decomp.csv"), row.names = F, na = "")
  
  write.csv(tempcount, paste0(getwd(), "/results/Temp Freq table.csv"), row.names = F, na = "")
  
  station.decomp.summary %>% 
    # give the results names that are human readable with units
    select("Region", "State", "n stations" = "n", 
           "Mean Deep Sed Annual Decomp (mol C/mol C)" = "meanDecomp.deep", "SD Deep Sed Annual Decomp (mol C/mol C)" = "dDecomp.deep",
           "Mean Shallow Sed Annual Decomp (mol C/mol C)" = "meanDecomp.shallow", "SD Shallow Sed Annual Decomp SD (mol C/mol C)" = "dDecomp.shallow",) %>% 
    write.csv(paste0(getwd(), "/results/state decomp summary.csv"), row.names = F, na = "")
  
  statedecomp %>% 
    # give the results names that are human readable with units
    select("Region", "State", "n stations" = "n", "Marsh Shoreline (km)" = "MarshShore",
           "Deep Sed Annual Decomp (mol C/mol C)" = "Decomp.deep", "Deep Sed Annual Decomp SD (mol C/mol C)" = "dDecomp.deep",
           "Shallow Sed Annual Decomp (mol C/mol C)" = "Decomp.shallow", "Shallow Sed Annual Decomp SD (mol C/mol C)" = "dDecomp.shallow",
           "Eroded Marsh Sediment Volume Deep Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeTall",
           "Eroded Marsh Sediment Volume Deep Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallTall", 
           "Eroded Marsh Sediment Volume Shallow Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeShort",
           "Eroded Marsh Sediment Volume Shllow Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallShort",
           "Eroded Marsh Sediment Carbon Deep Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeTall",
           "Eroded Marsh Sediment Carbon Deep Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallTall", 
           "Eroded Marsh Sediment Carbon Shallow Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeShort",
           "Eroded Marsh Sediment Carbon Shllow Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallShort",
           "Eroded Carbon Decomposed Shallow Bank & Large Erosion (kg/yr)" = "Decomp.LargeShort", "Eroded Carbon Decomposed Shallow Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeShort",
           "Eroded Carbon Decomposed Deep Bank & Large Erosion (kg/yr)" = "Decomp.LargeTall", "Eroded Carbon Decomposed Deep Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeTall",
           "Eroded Carbon Decomposed Shallow Bank & Small Erosion (kg/yr)" = "Decomp.SmallShort", "Eroded Carbon Decomposed Shallow Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallShort",
           "Eroded Carbon Decomposed Deep Bank & Small Erosion (kg/yr)" = "Decomp.SmallTall", "Eroded Carbon Decomposed Deep Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallTall") %>% 
    write.csv(paste0(getwd(), "/results/state decomp and erosion summary.csv"), row.names = F, na = "")
  
  regiondecomp %>% 
    # give the results names that are human readable with units
    select("Region", "n stations" = "n", "Marsh Shoreline (km)" = "MarshShore",
           "Eroded Marsh Sediment Volume Deep Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeTall",
           "Eroded Marsh Sediment Volume Deep Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallTall", 
           "Eroded Marsh Sediment Volume Shallow Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeShort",
           "Eroded Marsh Sediment Volume Shllow Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallShort",
           "Eroded Marsh Sediment Carbon Deep Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeTall",
           "Eroded Marsh Sediment Carbon Deep Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallTall", 
           "Eroded Marsh Sediment Carbon Shallow Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeShort",
           "Eroded Marsh Sediment Carbon Shllow Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallShort",
           "Eroded Carbon Decomposed Shallow Bank & Large Erosion (kg/yr)" = "Decomp.LargeShort", "Eroded Carbon Decomposed Shallow Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeShort",
           "Eroded Carbon Decomposed Deep Bank & Large Erosion (kg/yr)" = "Decomp.LargeTall", "Eroded Carbon Decomposed Deep Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeTall",
           "Eroded Carbon Decomposed Shallow Bank & Small Erosion (kg/yr)" = "Decomp.SmallShort", "Eroded Carbon Decomposed Shallow Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallShort",
           "Eroded Carbon Decomposed Deep Bank & Small Erosion (kg/yr)" = "Decomp.SmallTall", "Eroded Carbon Decomposed Deep Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallTall") %>% 
    write.csv(paste0(getwd(), "/results/regional decomp and erosion summary.csv"), row.names = F, na = "")
  
  nationaldecomp %>% 
    # give the results names that are human readable with units
    select(#"Year of Louisiana Shoreline Estimate" = "LAestYear", 
      "n stations" = "n", "Marsh Shoreline (km)" = "MarshShore",
           "Eroded Marsh Sediment Volume Deep Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeTall",
           "Eroded Marsh Sediment Volume Deep Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallTall", 
           "Eroded Marsh Sediment Volume Shallow Bank & Large Erosion (m^3/yr)" = "erodedSedVolume.LargeShort",
           "Eroded Marsh Sediment Volume Shllow Bank & Small Erosion (m^3/yr)" = "erodedSedVolume.SmallShort",
           "Eroded Marsh Sediment Carbon Deep Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeTall",
           "Eroded Marsh Sediment Carbon Deep Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallTall", 
           "Eroded Marsh Sediment Carbon Shallow Bank & Large Erosion (kg/yr)" = "erodedSedC.LargeShort",
           "Eroded Marsh Sediment Carbon Shllow Bank & Small Erosion (kg/yr)" = "erodedSedC.SmallShort",
           "Eroded Carbon Decomposed Shallow Bank & Large Erosion (kg/yr)" = "Decomp.LargeShort", "Eroded Carbon Decomposed Shallow Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeShort",
           "Eroded Carbon Decomposed Deep Bank & Large Erosion (kg/yr)" = "Decomp.LargeTall", "Eroded Carbon Decomposed Deep Bank & Large Erosion SD (kg/yr)" = "dDecomp.LargeTall",
           "Eroded Carbon Decomposed Shallow Bank & Small Erosion (kg/yr)" = "Decomp.SmallShort", "Eroded Carbon Decomposed Shallow Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallShort",
           "Eroded Carbon Decomposed Deep Bank & Small Erosion (kg/yr)" = "Decomp.SmallTall", "Eroded Carbon Decomposed Deep Bank & Small Erosion SD (kg/yr)" = "dDecomp.SmallTall") %>% 
    write.csv(paste0(getwd(), "/results/national decomp and erosion summary.csv"), row.names = F, na = "")
  
  ErosionAndDecompSummary %>% 
    # mutate(State = ifelse(State=="LA"& MarshShore >= 70000, "LA 1986", State)) %>% 
    write.csv(paste0(getwd(), "/results/decomp and erosion summary.csv"), row.names = F, na = "")
}
