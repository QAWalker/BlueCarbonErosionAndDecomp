#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

##### Run this script third #####

# this script reads in raw temperature data from a YSI near the coring location, ##### 
# and calculates the decomposition that would occur if sediment were incubated at those temperatures for a year

library(tidyverse)
library(lubridate)

# load in function to propagate error
source(file.path(getwd(),"mutate_with_error.R"))

## read in data
tempData <- read.csv(file.path(getwd(), 'data', "MHBTEMP2008-2016.csv")) %>% 
  rename(time = start_datetime, temp = result_value) %>% # rename cols
  mutate(time = mdy_hm(time, tz = "est"), ## Change time from a character to a POSIXct
         temp = as.numeric(temp), ## Change temp from a character to a numerical
         temp = round(temp, 0)) %>% ## Round all temp data to the ones place
  filter(!is.na(temp))

## Create a df to store each month of the sampling period and how many readings were made in that month
readingCounts <- data.frame(table(month(tempData$time), year(tempData$time)))
names(readingCounts) <- c("month", "year", "freq")

## sum the num of readings of next 11 months for each month giving us an idea of which series of 12 months had the most complete sampling regiments
for(i in 1:length(readingCounts$year)){
  readingCounts$yearly[i] <- sum(readingCounts$freq[i:(i+11)], na.rm = T)
}

## store the names of the month which begins the top ten most complete yearly sampling periods
topYears <- paste(month.name[readingCounts[order(-readingCounts$yearly), ][1:10, "month"]], readingCounts[order(-readingCounts$yearly), ][1:10, "year"], sep = "")

## create a list to store the data of the most complete sampling periods
goodYears = list()

## loop through each of the top 10 yearly sampling periods 
for(i in 1:length(topYears)){
  # create a POSIXct var storing the first day of each yearly period
  year <- myd(paste(topYears[i], "01", sep = ""), tz = "est") 
  # store the data from the raw data that fall between the first day and the first day plus one year
  # I had to declare each entry into the list specifically as a df to get it to store both the time and temp data
  goodYears[[i]] <- data.frame(time = tempData$time[(tempData$time>=year)&(tempData$time<(year+years(1)))],
                               temp = tempData$temp[(tempData$time>=year)&(tempData$time<(year+years(1)))])
}

names(goodYears) <- topYears # give the list a proper name

## create a df for storing the count of six minute periods at every temp from 0 to 40 degrees C for each of the top years
tempcountMHB <- data.frame(temp = -5:50, matrix(0, ncol = length(goodYears), nrow = length(-5:50)))
names(tempcountMHB) <- c("temp", names(goodYears)) # give the df a proper name

## loop through each of the top years to count the temperature periods
for(i in names(goodYears)){
  # use table to count how many readings at each temperature were recorded
  # I created a temporary df (x) each time through storing the table of individual counts
  x <- data.frame(table(data.frame(goodYears[i])[, paste(i, "temp", sep = ".")])) 
  for(j in min(as.numeric(as.character(x$Var1))):max(as.numeric(as.character(x$Var1)))){
    tempcountMHB[tempcountMHB$temp == j, i] <- x$Freq[x$Var1==j]
  }
}

#### activation energy and total decomposition ####
finalRates <- filter(decompRate.summary, day == max(decompRate.summary$day)) %>% 
  arrange(Depth, Treatment)

# constants calculated from experiments
R <- .00831446 #kJ mol^-1 K^-1
K2 <- c(finalRates$slope[finalRates$Treatment == "IB 30"]) #mol C mol C^-1 d^-1 #changes based on deep, shallow sediment
K2.sd <- c(finalRates$sd[finalRates$Treatment == "IB 30"])
K1 <- c(finalRates$slope[finalRates$Treatment == "IB 20"]) #mol C mol C^-1 d^-1 #changes based on deep, shallow sediment
K1.sd <- c(finalRates$sd[finalRates$Treatment == "IB 20"])
T2 <- 30 + 273.15 #Kelvin
T1 <- 20 + 273.15 #Kelvin

#Create a data frame to feed to the error propagation function. 
#Each object, even the constants needs to have an error, I used 0 for the constants
Kvals <- data.frame(temp = rep(-5:50, each = 2)+273.15, dtemp = 0, 
                    temp.C = rep(-5:50, each = 2), dtemp = 0, 
                    depth = c("deep", "shallow"),
                    K1, dK1 = K1.sd, 
                    K2, dK2 = K2.sd, 
                    R, dR = 0, 
                    T1, dT1 = 0, 
                    T2, dT2 = 0)

## calc activation energy and propagate the error
Kvals <- Kvals %>% mutate_with_error(Ea ~ (log(K2)-log(K1))*R/((1/T2)-(1/T1))*(-1))

# calc the K value at each temperature and propagate the error
Kvals <- Kvals %>% mutate_with_error(Ktemp ~ (-1*(Ea/R)*((1/temp)-(1/T1)))) %>% 
  mutate_with_error(Ktemp ~ exp(Ktemp+log(K1))) #not sure why but have to seperate these two steps

#create a dataframe from the arrhenius model
Amod <- data.frame(filter(Kvals, depth=="deep")[,c("temp", "temp.C")], 
                   filter(Kvals, depth=="deep")[,c("Ktemp", "dKtemp")], 
                   filter(Kvals, depth=="shallow")[,c("Ktemp", "dKtemp")])

names(Amod) <- c("temp", "temp.C",
                 "Ktemp.deep", "dKtemp.deep",
                 "Ktemp.shallow", "dKtemp.shallow")

## Plot K2 for each temperature
ggplot(Amod, aes(x = temp.C))+
  geom_point(aes(y = Ktemp.deep, color = "deep"))+
  geom_line(aes(y = Ktemp.deep, color = "deep"))+
  geom_point(aes(y = Ktemp.shallow, color = "shallow"))+
  geom_line(aes(y = Ktemp.shallow, color = "shallow"))+
  coord_cartesian(xlim = c(-5, 50), expand = F)+
  theme_bw()+
  labs(x = "Temp (°C)", #degree = Alt + 0176
       y = "mol CO2 · mol C-1 · d^-1", # dot operator = Alt + 0183
       color = "Soil Horizon", 
       title = "K at each temperature and propagated error")

Ea.summary <- Kvals[1:2,c("depth", "K1", "dK1", "K2", "dK2", "Ea", "dEa")]

## New df to hold total decomp from each year at each sed depth 
totalDecompMHB <- data.frame(matrix(0, ncol = length(topYears), nrow = 2), row.names = c("deep", "shallow"))
names(totalDecompMHB) <- topYears #desriptive names

## loop through the top years and calculate the decomp at each temp in each year
for(i in topYears){
  ## generate new columns for each year that calculate the amount of decomp that happened because of each temp. and then put the unit in mmol per days
  tempcountMHB[, paste(i,"Decomp.deep", sep = "")] <- tempcountMHB[, i] * Ea.summary$K2[1] * 6/60/24 * 1000
  ## sum the total decomp from each temperature and add it to a new df
  totalDecompMHB[1, i] <- sum(tempcountMHB[, paste(i,"Decomp.deep", sep = "")])
  tempcountMHB[, paste(i,"Decomp.shallow", sep = "")] <- tempcountMHB[, i] * Ea.summary$K2[2] * 6/60/24 * 1000
  ## sum the total decomp from each temperature and add it to a new df
  totalDecompMHB[2, i] <- sum(tempcountMHB[, paste(i,"Decomp.shallow", sep = "")])
}

decomp.list <- list()
for(i in topYears){
  ## generate new columns for each year that calculate the amount of decomp that happened because of each temp. and then put the unit in mmol per days
  decomp.list[[i]] <- Amod %>% 
    mutate(year = i, 
           count = tempcountMHB[,i],
           years = count *  6/60/24 * 1000, 
           dyears = 0) %>% 
    mutate_with_error(decomp.deep ~ years * Ktemp.deep) %>% 
    mutate_with_error(decomp.shallow ~ years * Ktemp.shallow)
}

# create a data frame to store the total decomposition in each of the top years
decomp.yearly <- data.frame()
for (i in names(decomp.list)) {
  decomp.yearly <- bind_rows(decomp.yearly, 
                             data.frame(year = i, 
                                        total.deep =  sum(decomp.list[[i]]$decomp.deep),
                                        sd.deep = sqrt(sum((decomp.list[[i]]$ddecomp.deep)^2)),
                                        total.shallow =  sum(decomp.list[[i]]$decomp.shallow),
                                        sd.shallow = sqrt(sum((decomp.list[[i]]$ddecomp.shallow)^2))))
} 
