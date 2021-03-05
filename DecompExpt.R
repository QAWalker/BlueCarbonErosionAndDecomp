#####
# Q10 vs portion of sediment respired
# This method of calculating Q10 uses the ratio of time to respire a given portion of sediment at different temperature
# rather than the ratio of rates at a the same time after experiment start. 
# This was inspired by Conant et al. 2008
###
rm(list = ls())

library(tidyverse)
# 
# decomp.data <- read.csv(paste0(getwd(), "/data_summary_submerged_corrected.csv")) %>% 
#   filter(Treatment != "SA") %>% 
#   as_tibble()
# 
# write_csv(decomp.data, file.path(getwd(), "GGA_data.csv"), na = "")

SampleCarbon <- read.csv(file.path(getwd(), 'Sample Carbon Content.csv'))

GGATechSpecs <- 
  data.frame(vol.HS = 0.615, #volume of the incubation bottle HeadSpace in liters
             vol.GGA = 0.35258189116719, #volume of air in the GGA
             vol.total = 0.967581891167191, #volume of the incubation bottle + GGA system
             temp.K = 300.85) #temperature 27.7 C in Kelvin

# GGATechSpecs$vol.total <- (GGATechSpecs$vol.HS + GGATechSpecs$vol.GGA)

decomp.data <- read.csv(file.path(getwd(), "GGA_data_revised.csv")) %>% 
  select("Date", "Time", "Time.Point", "day", "ElapsedDays",
         "Bottle", "Treatment", "Depth", "Rep",
         "CO2.sys.ppm.p", "CH4.sys.ppm.p", "n.mol.p",
         "CO2.air.ppm", "CH4.air.ppm", "CO2.sys.ppm", "CH4.sys.ppm") %>%
  filter(CO2.sys.ppm.p != "opened bottles to atmosphere") %>%
  mutate(CO2.sys.ppm.p = as.numeric(CO2.sys.ppm.p),
         CH4.sys.ppm.p = as.numeric(CH4.sys.ppm.p))

decomp.data <- decomp.data %>% 
  # filter(!(Treatment == "IB 30" & Depth == "S" & Rep == 1 & day == 1 & CO2.air == 428.76)) %>% 
  group_by(Treatment, Depth, Rep) %>% 
  mutate(CO2.bottle.ppm = (CO2.sys.ppm - ((GGATechSpecs$vol.GGA/GGATechSpecs$vol.total)*CO2.air.ppm))*(GGATechSpecs$vol.total/GGATechSpecs$vol.HS),
         CH4.bottle.ppm = (CH4.sys.ppm - ((GGATechSpecs$vol.GGA/GGATechSpecs$vol.total)*CH4.air.ppm))*(GGATechSpecs$vol.total/GGATechSpecs$vol.HS),
         n.mol.p = GGATechSpecs$vol.HS/(GGATechSpecs$temp.K * 0.08206),
         n.mol = GGATechSpecs$vol.HS/(GGATechSpecs$temp.K * 0.08206),
         CO2.bottle.umol = round(CO2.bottle.ppm * n.mol, 2),
         CH4.bottle.umol = round(CH4.bottle.ppm * n.mol, 4),
         CO2.production.umol = round(ifelse(day == 0, 0, CO2.bottle.umol - (CO2.sys.ppm.p * n.mol.p)), 2),
         CH4.production.umol = round(ifelse(day == 0, 0, CH4.bottle.umol - (CH4.sys.ppm.p * n.mol.p)), 4), 
         CO2.cumulative.umol = round(cumsum(CO2.production.umol), 2),
         CH4.cumulative.umol = round(cumsum(CH4.production.umol), 4)) %>% 
  ungroup() %>% 
  left_join(SampleCarbon) %>% 
  mutate(Cumulative_CO2.C = CO2.cumulative.umol/C.mol,
         Cumulative_CH4.C = CH4.cumulative.umol/C.mol, 
         TotalCarbonRespired_umolC = CO2.cumulative.umol + CH4.cumulative.umol, 
         TotalCarbonRespired_umolC.C = TotalCarbonRespired_umolC/C.mol,
         RemainingSedCarbon_molC = C.mol - (TotalCarbonRespired_umolC / 10000000),
         PercentCRespired = TotalCarbonRespired_umolC/(C.mol*1000000))

#get mean, sd, se, n of Percent C Respired for each day, depth and treatment group
decomp.summary <- decomp.data %>% 
  group_by(day, Depth, Treatment) %>% 
  summarize(n = n(),
            mean = mean(PercentCRespired, na.rm = T), 
            sd = sd(PercentCRespired, na.rm = T), 
            se = sd/sqrt(n)) %>% 
  ungroup() %>% 
  rename(PercentCRespired = mean)

##Create blank df to store the q10 data in
Q10q <- data.frame("PercentRespired" = seq(0.01,0.1, by = .01))
#last reading BEFORE hitting the portion threshold value
Q10q[,c("30MTimeB" ,"30MValueB","20MTimeB","20MValueB","30STimeB","30SValueB","20STimeB","20SValueB")] <- NA
#first reading AFTER hitting the portion threshold value
Q10q[,c("30MTimeA","30MValueA","20MTimeA","20MValueA","30STimeA","30SValueA","20STimeA","20SValueA")] <- NA

for (d in unique(decomp.summary$Depth)) {
  for (i in seq(0.01, 0.1, by = .01)) {
    #create a df storing all the values GREATER than the % step; one for 30 degree and one for 20 degree
    ag <- decomp.summary %>% 
      filter(Treatment == "IB 30", Depth == d, PercentCRespired > i)
    bg <- decomp.summary %>% 
      filter(Treatment == "IB 20", Depth == d, PercentCRespired > i)
    
    #Store the time and value for the first reading greater than the threshold for 30 degree
    Q10q[Q10q$PercentRespired==i, paste("30",d,"TimeA", sep = "")] <- ag$day[1]
    Q10q[Q10q$PercentRespired==i, paste("30",d,"ValueA", sep = "")] <- ag$PercentCRespired[1]
    #Store the time and value for the first reading greater than the threshold for 20 degree
    Q10q[Q10q$PercentRespired==i, paste("20",d,"TimeA", sep = "")] <- bg$day[1]
    Q10q[Q10q$PercentRespired==i, paste("20",d,"ValueA", sep = "")] <- bg$PercentCRespired[1]
    
    #create a df storing all the values LESS than the % step; one for 30 degree and one for 20 degree
    al <- decomp.summary[(decomp.summary$Depth==d)&
                           (decomp.summary$PercentCRespired<i)&
                           (decomp.summary$Treatment=="IB 30"), ]
    bl <- decomp.summary[(decomp.summary$Depth==d)&
                           (decomp.summary$PercentCRespired<i)&
                           (decomp.summary$Treatment=="IB 20"), ]
    #check to see if there are any readings greater than the current portion threshold, if not store NA, else store the value
    if (length(ag[,1])==0) {
      Q10q[Q10q$PercentRespired==i, paste("30",d,"TimeB", sep = "")] <- NA
      Q10q[Q10q$PercentRespired==i, paste("30",d,"ValueB", sep = "")] <- NA
    }else { 
      Q10q[Q10q$PercentRespired==i, paste("30", d, "TimeB", sep = "")] <- al$day[length(al$day)]
      Q10q[Q10q$PercentRespired==i, paste("30", d, "ValueB", sep = "")] <- al$PercentCRespired[length(al$day)]
    }
    #check to see if there are any readings greater than the current portion threshold, if not store NA, else store the value
    if (length(bg[, 1])==0) {
      Q10q[Q10q$PercentRespired==i, paste("20", d, "TimeB", sep = "")] <- NA
      Q10q[Q10q$PercentRespired==i, paste("20", d, "ValueB", sep = "")] <- NA
    }else { 
      Q10q[Q10q$PercentRespired==i, paste("20", d, "TimeB", sep = "")] <- bl$day[length(bl$day)]
      Q10q[Q10q$PercentRespired==i, paste("20", d, "ValueB", sep = "")] <- bl$PercentCRespired[length(bl$day)]
    }
  }
}

#Create a new dataframe to store the calculated value where each group of bottles would have crossed the threshold if they respired carbon linearly
Q10calc <- data.frame("PercentRespired" = Q10q$PercentRespired, 
                      "M30" = 0, "M20" = 0, "Q10M" = NA, "S30" = 0, "S20" = 0, "Q10S" = NA)

#New Function that calculates the time where we estimate each group of bottles crossed the threshold
TimeAtGivenPortion <- function(Portion, BeforeXY, AfterXY){
  m = (AfterXY[2]-BeforeXY[2])/(AfterXY[1]-BeforeXY[1]) #calculate slope (rise over run)
  b = AfterXY[2]-m*AfterXY[1] #calculate the intercept by substituing the calculated slope into y=mx+b
  Time = (Portion - b)/m #calculate the time the linear model intercepts the threshold value
  return(Time) #return Time
}


#loop through each portion threshold and run each combo of depth, temperature throught the TimeAtGivenPortion function and store ito the Q10calc df
for (portion in unique(Q10q$PercentRespired)) {
  for (depth in c("M", "S"))  {
    for (temp in c(20, 30))  {
      Q10calc[Q10q$PercentRespired == portion, paste(depth, temp, sep = "")] <-
        TimeAtGivenPortion(
          Portion = portion,
          BeforeXY = c(Q10q[Q10q$PercentRespired == portion, paste0(temp, depth, "TimeB")],
                       Q10q[Q10q$PercentRespired == portion, paste0(temp, depth, "ValueB")]),
          AfterXY = c(Q10q[Q10q$PercentRespired == portion, paste0(temp, depth, "TimeA")],
                      Q10q[Q10q$PercentRespired == portion, paste0(temp, depth, "ValueA")])
        )
    }
  }
}

#Calc Q10 by dividing the time to get through each portion at 20 degrees by the time to get through the same portion at 30 degrees
for (i in 1:10) {
  if (i == 1) {
    Q10calc$Q10M[i] <- (Q10calc$M20[i])/(Q10calc$M30[i])
    Q10calc$Q10S[i] <- (Q10calc$S20[i])/(Q10calc$S30[i])
  }else{
    Q10calc$Q10M[i] <- (Q10calc$M20[i]-Q10calc$M20[i-1])/(Q10calc$M30[i]-Q10calc$M30[i-1])
    Q10calc$Q10S[i] <- (Q10calc$S20[i]-Q10calc$S20[i-1])/(Q10calc$S30[i]-Q10calc$S30[i-1])
  }
}

#Reorganize the Q10 data into a new df that is more friendly to plotting
Q10 <- reshape2::melt(Q10calc, id.vars = "PercentRespired", measure.vars = c("Q10M", "Q10S"), value.name = "Q10") %>% 
  mutate(Depth = substr(variable, 4,4))


