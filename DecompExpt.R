#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

##### Run this script first #####


# this script reads in raw data from our decomposition/incubation experiment, ##### 
# converts it to moles of gas and moles of carbon,
# and calculates the mean amount decomposed for each treatment

library(tidyverse)

# Read in the initial Carbon content of the sediment pre-incubation
SampleCarbon <- read.csv(file.path(getwd(), 'Sample Carbon Content.csv'))

# read in the technical specifications of the LGR GGA
GGATechSpecs <- 
  data.frame(vol.HS = 0.615, #volume of the incubation bottle HeadSpace in liters
             vol.GGA = 0.35258189116719, #volume of air in the GGA
             vol.total = 0.967581891167191, #volume of the incubation bottle + GGA system
             temp.K = 300.85) #temperature 27.7 C in Kelvin

# Read in the gas concentration data from the incubation experimentand get all the data into the correct format
# for both CO2 and CH4:
# CXX.sys.ppm.p = the concentration of gas in the GGA + incubation bottle system at the PRIOR READING
# CXX.sys.ppm = the concentration of gas in the GGA + incubation bottle system 
# CXX.air.ppm = the concentration of gas in the GGA prior to connecting the incubation bottle  
decomp.data <- read.csv(file.path(getwd(), "GGA_data.csv")) %>% 
  select("Date", "Time", "Time.Point", "day", "ElapsedDays",
         "Bottle", "Treatment", "Depth", "Rep",
         "CO2.sys.ppm.p", "CH4.sys.ppm.p", 
         "CO2.air.ppm", "CH4.air.ppm", 
         "CO2.sys.ppm", "CH4.sys.ppm") %>%
  filter(CO2.sys.ppm.p != "opened bottles to atmosphere") %>% # filter out extraneous rows
  mutate(CO2.sys.ppm.p = as.numeric(CO2.sys.ppm.p),
         CH4.sys.ppm.p = as.numeric(CH4.sys.ppm.p))

# Convert the gas concentration data to moles of gas #####
decomp.data <- decomp.data %>% 
  group_by(Treatment, Depth, Rep) %>% 
  mutate(
    ## Back calculate the gas concentrations in the incubation 
    CO2.bottle.ppm = (CO2.sys.ppm - ((GGATechSpecs$vol.GGA/GGATechSpecs$vol.total)*CO2.air.ppm))*(GGATechSpecs$vol.total/GGATechSpecs$vol.HS),
    CH4.bottle.ppm = (CH4.sys.ppm - ((GGATechSpecs$vol.GGA/GGATechSpecs$vol.total)*CH4.air.ppm))*(GGATechSpecs$vol.total/GGATechSpecs$vol.HS),
    
    ## calculate the number of moles of gas in the incubation bottle prior to and after incubation 
    n.mol.p = GGATechSpecs$vol.HS/(GGATechSpecs$temp.K * 0.08206),
    n.mol = GGATechSpecs$vol.HS/(GGATechSpecs$temp.K * 0.08206),
    
    ## calculate the number of moles of each gas in the bottle
    CO2.bottle.umol = round(CO2.bottle.ppm * n.mol, 2),
    CH4.bottle.umol = round(CH4.bottle.ppm * n.mol, 4),
    
    ## calculate the number of moles produced since the last reading
    CO2.production.umol = round(ifelse(day == 0, 0, CO2.bottle.umol - (CO2.sys.ppm.p * n.mol.p)), 2),
    CH4.production.umol = round(ifelse(day == 0, 0, CH4.bottle.umol - (CH4.sys.ppm.p * n.mol.p)), 4), 
    
    ## add the number of moles produced since the last reading to the total number of mole produced during incubation
    CO2.cumulative.umol = round(cumsum(CO2.production.umol), 2),
    CH4.cumulative.umol = round(cumsum(CH4.production.umol), 4)) %>% 
  ungroup() %>% 
  left_join(SampleCarbon) %>% 
  mutate(
    # Calculate the number of umols of each gas produced per mol of initial sediment C
    Cumulative_CO2.C = CO2.cumulative.umol/C.mol,
    Cumulative_CH4.C = CH4.cumulative.umol/C.mol, 
    
    # calculate the number of umols of C respired as CO2 and CH4
    TotalCarbonRespired_umolC = CO2.cumulative.umol + CH4.cumulative.umol, 
    
    # calculate the number of umols of C respired as CO2 and CH4 per mol of initial sediment C
    TotalCarbonRespired_umolC.C = TotalCarbonRespired_umolC/C.mol,
    
    # calculate how much of the initial C is left un-respired
    RemainingSedCarbon_molC = C.mol - (TotalCarbonRespired_umolC / 10000000),
    
    #calculate the percent of initial sediment C that has been respired as CH4 and CO2
    PercentCRespired = TotalCarbonRespired_umolC/(C.mol*1000000))

#get mean, sd, se, n of Percent C Respired for each day, depth and treatment group
decomp.summary <- decomp.data %>% 
  group_by(day, Depth, Treatment) %>% 
  summarize(n = n(),
            mean = mean(PercentCRespired, na.rm = T), 
            sd = sd(PercentCRespired, na.rm = T), 
            se = sd/sqrt(n), 
            ci = se * qt(0.95/2 + .5, n-1)) %>% 
  ungroup() %>% 
  rename(PercentCRespired = mean)

# Calculate the rate of decomposition on each day of the experiment by looking at total C respired/days
# first the rate in each bottle
decompRate <- decomp.data %>% 
  group_by(day, Depth, Treatment, Rep) %>% 
  summarize(d = day, 
            slope = (PercentCRespired/day)) %>% 
  ungroup()

# the mean rate for each depth and treatment group
decompRate.summary <- decompRate %>% 
  group_by(day, Depth, Treatment) %>% 
  summarise(meanslope = mean(slope, na.rm = T), 
            sd = sd(slope, na.rm = T), 
            n = n(),
            se = sd/sqrt(n)) %>% 
  rename(slope = meanslope) %>% 
  ungroup()
