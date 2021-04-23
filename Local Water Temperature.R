#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

##### Run this sript third #####

# this script reads in raw temperature data from a YSI near the coring location, ##### 
# and calculates the decomposition that would occur if sediment were incubated at those temperatures for a year


library(tidyverse)
library(lubridate)

## read in data
tempData <- read.csv(file.path(getwd(), "/MHBTEMP2008-2016.csv"))

tempData <- tempData %>% 
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
topYears <- numeric()
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
range(tempData$temp)
## loop though each year and save a histogram of the data
for(i in names(goodYears)){
  ggplot(select(tempcountMHB, temp, count = i), aes(temp, count))+
    geom_col()+
    scale_y_continuous(limits = c(0,10000))+
    scale_x_continuous(limits = c(0, 35), breaks = seq(-10, 50, by = 10))+
    labs(title = paste("year starting in", i), y = "Count of Six Minute Measurements", 
         x = "Temperature (C)")+
    theme_bw()
   
  ggsave(paste0(getwd(), "/plots/", i, ".png"), width = 6, height = 4, units = "in", dpi = 330)
}

######
# from activation energy and total decomposition #
####
# this function is from https://www.r-bloggers.com/easy-error-propagation-in-r/
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

# This version is from https://gist.github.com/EricEdwardBryant/8d0fbabac28f3adb2cf9a24a17763533
# mutate_with_error = function(.data, f) {
#   
#   exprs = list(
#     # expression to compute new variable values
#     deparse(f[[3]]),
#     
#     # expression to compute new variable errors
#     all.vars(f[[3]]) %>%
#       purrr::map_chr(~ sprintf('(d%s*(%s))^2', ., deparse(D(f[[3]], .)))) %>%
#       stringr::str_c(collapse = '+') %>%
#       sprintf('sqrt(%s)', .)
#   )
#   
#   names(exprs) = c(deparse(f[[2]]), sprintf('d%s', deparse(f[[2]])))
#   
#   dplyr::mutate_(.data, .dots = exprs)
# }
## constants calculated from experiments
R <- .00831446 #kJ mol^-1 K^-1
K2 <- c(840.5/1000000, 1018/1000000) #mol C mol C^-1 d^-1 #changes based on deep, shallow sediment
K2.sd <- c(82.4/1000000, 166/1000000)
K1 <- c(379.4/1000000, 518/1000000) #mol C mol C^-1 d^-1 #changes based on deep, shallow sediment
K1.sd <- c(92.3/1000000, 32.4/1000000)
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

# Kvals %>% mutate_with_error(Ktemp ~ log(K1)) %>% 
#   mutate_with_error(Ktemp ~ (-1*(Ea/R)*((1/temp)-(1/T1)))+Ktemp) %>% 
#   #mutate_with_error(Ktemp ~ exp(Ktemp)) %>% 
#   head()
# #%>% 
#   mutate_with_error(Ktemp ~ exp(Ktemp+log(K1))) #not sure why but have to seperate these two steps

# Same as above but with the error at 30 degrees
# ## calc activation energy and propagate the error
# Kvals <- Kvals %>% mutate_with_error(Ea ~ (log(K1)-log(K2))*R/((1/T1)-(1/T2))*(-1))
# 
# # calc the K value at each temperature and propagate the error
# Kvals <- Kvals %>% mutate_with_error(Ktemp ~ (-1*(Ea/R)*((1/temp)-(1/T2)))) %>% 
#   mutate_with_error(Ktemp ~ exp(Ktemp+log(K2))) #not sure why but have to seperate these two steps

# Kvals <- Kvals %>% mutate_with_error(Ktemp ~ (-1*(Ea/R)*((1/temp)-(1/T1)))) %>% 
#   mutate_with_error(Ktemp1 ~ exp((Ea/R)*(+log(K1)))) #not sure why but have to seperate these two steps

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

# Ea <-  (log(K2)-log(K1))*R/((1/T2)-(1/T1))*(-1) # kJ mol^-1
# ## new df to store the arrhenius kenetics model equations and calculations
# ArrmodelMHB <-  data.frame(Temp = c(-5:50), K2.deep = (0), K2.shallow = (0))
# ## loop through temperatures 0 to 40 calculating the k2 for both sediment depths at each temp
# for(i in -5:50){
#   ArrmodelMHB[ArrmodelMHB$Temp==i, "K2.deep"] <- exp(-1*(Ea[[1]]/R)*((1/(i+273))-(1/T1))+log(K1[[1]]))
#   ArrmodelMHB[ArrmodelMHB$Temp==i, "K2.shallow"] <- exp(-1*(Ea[[2]]/R)*((1/(i+273))-(1/T1))+log(K1[[2]]))
# }
# write.csv(x= ArrmodelMHB, file = "R:/DCERP/Carbon Inventory/Decomp Expts/Creek Temp/K2 calc.csv")
# 
# ## plot the K2s
# ggplot(ArrmodelMHB, aes(x = Temp))+
#   geom_line(aes(y = K2.deep))+
#   geom_point(aes(y = K2.deep))+
#   geom_line(aes(y = K2.shallow))+
#   geom_point(aes(y = K2.shallow))+
#   labs(y = expression(paste("mol C mol ", C^-1, d^-1)))

# 
# ################ Do I need this?
# tempcountMHB$K2.deep <- ArrmodelMHB$K2.deep
# tempcountMHB$K2.shallow <- ArrmodelMHB$K2.shallow
# tempcountMHB <- tempcountMHB[c(1, (length(names(tempcountMHB))-1):length(names(tempcountMHB)), 2:(length(names(tempcountMHB))-2))] #reorder


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
names(tempcountMHB)
for(i in names(tempcountMHB)[!(names(tempcountMHB) %in% c("temp", names(goodYears)))]){
  ## Generate a plot showing the decomposition that happened at each temp through out the year for each depth
  ggplot(select(tempcountMHB, temp, decomp = i), aes(temp, decomp))+
    geom_col()+
    # scale_y_continuous(limits = c(0,10000))+
    scale_x_continuous(limits = c(0, 35), breaks = seq(-10, 50, by = 10))+
    labs(title = paste("year starting in", i), y = "Count of Six Minute Measurements",
         x = "Temperature (C)")+
    theme_bw()

  # ggsave(paste0(getwd(), "/plots/", i, ".png"), width = 6, height = 4, units = "in", dpi = 330)
  # 
  jpeg(paste0(getwd(),'/', names(tempcountMHB[i]), 'plot.jpg'))
  plot(tempcountMHB$temp, tempcountMHB[, i], ylim = c(0, 41), type = "h", main = names(tempcountMHB[i]), lwd = 5, xlab = "Temperature (C)", ylab = "decomp")
  dev.off()
}
i = "March2015"
decomp.list <- list()
for(i in topYears){
  ## generate new columns for each year that calculate the amount of decomp that happened because of each temp. and then put the unit in mmol per days
  # tempcountMHB[, paste(i,"Decomp.deep", sep = "")] <- tempcountMHB[, i] * tempcountMHB$K2.deep * 6/60/24 * 1000
  
  decomp.list[[i]] <- Amod %>% 
    mutate(year = i, 
           count = tempcountMHB[,i],
           years = count *  6/60/24 * 1000, 
           dyears = 0) %>% 
    mutate_with_error(decomp.deep ~ years * Ktemp.deep) %>% 
    mutate_with_error(decomp.shallow ~ years * Ktemp.shallow)
  
  
  
  ## sum the total decomp from each temperature and add it to a new df
  # totalDecompMHB[1, i] <- sum(tempcountMHB[, paste(i,"Decomp.deep", sep = "")])
  # tempcountMHB[, paste(i,"Decomp.shallow", sep = "")] <- tempcountMHB[, i] * tempcountMHB$K2.shallow * 6/60/24 * 1000
  # ## sum the total decomp from each temperature and add it to a new df
  # totalDecompMHB[2, i] <- sum(tempcountMHB[, paste(i,"Decomp.shallow", sep = "")])
}
decomp.yearly <- data.frame()
for (i in names(decomp.list)) {
  decomp.yearly <- bind_rows(decomp.yearly, 
                             data.frame(year = i, 
                                        total.deep =  sum(decomp.list[[i]]$decomp.deep),
                                        sd.deep = sqrt(sum((decomp.list[[i]]$ddecomp.deep)^2)),
                                        total.shallow =  sum(decomp.list[[i]]$decomp.shallow),
                                        sd.shallow = sqrt(sum((decomp.list[[i]]$ddecomp.shallow)^2))))
} 

ggplot(dec, aes(temp.C, decomp.deep))+
  geom_errorbar(aes(ymin = decomp.deep/2, ymax = decomp.deep + ddecomp.deep))+
  geom_col()

data.frame(a = 20, da = 5, b = 30, db = 8) %>% 
  mutate_with_error(c ~ a + b)

data.frame(a = 20, da = 5, b = 30, db = 8) %>%
  mutate(c = a+b, 
         dc = sqrt((1^2) * (da^2) + (1^2) * (db^2)))

sum(dec$decomp.deep)
sqrt(sum((dec$ddecomp.deep)^2))

sum(dec$decomp.shallow)
sqrt(sum((dec$ddecomp.shallow)^2))

for(i in 14:33){
  ## Generate a plot showing the decomposition that happened at each temp through out the year for each depth
  jpeg(paste('R:/DCERP/DCERP2/Final Report 2017/Decomposition CW-4/', names(tempcountMHB[i]), 'plot.jpg'))
  plot(tempcountMHB$temp, tempcountMHB[, i], ylim = c(0, 41), type = "h", main = names(tempcountMHB[i]), lwd = 5, xlab = "Temperature (C)", ylab = "decomp")
  dev.off()
}

Amod

totalDecompMHB$Mean <- rowMeans(totalDecompMHB, na.rm = T)
totalDecompMHB$sd <- c(sd(totalDecompMHB[1,topYears], na.rm = T), sd(totalDecompMHB[2,topYears], na.rm = T))

# write.csv(x = totalDecompMHB, file = "totalDecompMHB.csv",row.names = T)
# write.csv(x = readingCounts, file = "readingCounts.csv" ,row.names = F)
# write.csv(x = tempcountMHB, file = "tempcountMHB.csv" ,row.names = F)
# write.csv(x = tempData, file = "tempData.csv" ,row.names = F)