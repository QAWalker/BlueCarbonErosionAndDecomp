#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

##### Run this sript second #####
# this script assumes that output from DecompExpt.R is in the global environment

##### Calculate Temperature Sensitivity #####
# This script takes the summarized decomp expt data and calculates temp sensitivity 
# for the shallow and deep depth horizons
# temperature sensitivity is measured with traditional Q10 and Q10q

# load required packages
library(reshape2)

# load in function to propogate error
source(file.path(getwd(),"mutate_with_error.R"))

# Function that calculates the time where we estimate each group of bottles crossed the threshold
TimeAtGivenPortion <- function(Portion, BeforeXY, AfterXY){
  m = (AfterXY[2]-BeforeXY[2])/(AfterXY[1]-BeforeXY[1]) #calculate slope (rise over run)
  b = AfterXY[2]-m*AfterXY[1] #calculate the intercept by substituing the calculated slope into y=mx+b
  Time = (Portion - b)/m #calculate the time the linear model intercepts the threshold value
  return(Time) #return Time
}

## Traditional Q10 ####
decompRate <- data.frame(Depth = NA, Treatment = NA, day = NA)

for (d in unique(decomp.data$day)) {
  decompRate <- filter(decomp.data, day <= d) %>% 
    group_by(Depth, Treatment, Rep) %>% 
    summarize(d = d, 
              slope = summary(lm(PercentCRespired ~ day))$coefficients[2]) %>% 
    rename(day = d) %>% 
    ungroup() %>% 
    bind_rows(decompRate, .) %>% 
    filter(!is.na(Depth))
}

Q10 <- decompRate %>% 
  group_by(day, Depth, Treatment) %>% 
  summarise(slope = mean(slope, na.rm = T)) %>% 
  dcast(day + Depth ~ Treatment) %>% 
  left_join(.,
            decompRate %>% 
              group_by(day, Depth, Treatment) %>% 
              summarise(dSlope = sd(slope, na.rm = T)) %>% 
              dcast(day + Depth ~ Treatment), 
            suffix = c(".slope", ".sd"), by = c("day", "Depth")) %>% 
  rename("slope.20" = "IB 20.slope", 
         "dslope.20" = "IB 20.sd", 
         "slope.30" = "IB 30.slope",
         "dslope.30" = "IB 30.sd") %>% 
  mutate_with_error(Q10 ~ slope.30/slope.20) %>% 
  ungroup() %>% 
  mutate(seQ10 = dQ10/sqrt(6)) %>% 
  select(day, Depth, dplyr::contains("Q"))

# Plot Q10
ggplot(Q10, aes(day, Q10, color = Depth))+
  geom_line()+
  geom_errorbar(aes(min = Q10 - seQ10, max = Q10 + seQ10))+
  geom_point()+
  theme_bw()

#####
# Q10q
# Q10 at each nth percent of sediment C respired
# 
# This method of calculating Q10 uses the ratio of time to respire a given portion of sediment C at temperatures 10 degrees apart
# rather than the ratio of rates at a the same time after experiment start.
# See Conant et al. 2008
###

for (q in seq(0, round(max(decomp.data$PercentCRespired), 2), by=0.01)) {
  temp <- decomp.data %>% 
    filter(PercentCRespired <= q) %>% 
    group_by(Treatment, Depth, Bottle = Rep) %>% 
    summarize(PortionC = q, 
              TimeBefore = max(day, na.rm = T),
              ValueBefore = max(PercentCRespired, na.rm = T))
  
  temp <- decomp.data %>% 
    filter(PercentCRespired > q) %>% 
    group_by(Treatment, Depth, Bottle = Rep) %>% 
    summarize(PortionC = q, 
              TimeAfter = min(day, na.rm = T),
              ValueAfter = min(PercentCRespired, na.rm = T)) %>% 
    left_join(temp, ., by = c("Treatment", "Depth", "Bottle", "PortionC"))
  
  if(!exists("ElapsedTime")){
    ElapsedTime <- temp
  }else{
    ElapsedTime <- bind_rows(ElapsedTime, temp)
  }
}

for (i in 1:nrow(ElapsedTime)) {
  ElapsedTime[i, "CalcTime"] <- with(ElapsedTime[i, ], TimeAtGivenPortion(PortionC, c(TimeBefore, ValueBefore), c(TimeAfter, ValueAfter)))
}

ElapsedTime <- ElapsedTime %>% 
  group_by(Treatment, Depth, Bottle) %>% 
  arrange(Treatment, Depth, Bottle, PortionC) %>% 
  mutate(ElapsedTime = CalcTime - lag(CalcTime, default = 0))

ElapsedTime.summary <- ElapsedTime %>% 
  group_by(PortionC, Depth, Treatment) %>% 
  summarize(mean = mean(ElapsedTime, na.rm = T),
            sd = sd(ElapsedTime, na.rm = T),
            N = n(), 
            se = sd/sqrt(N)) %>% 
  rename(ElapsedTime = mean) %>% 
  filter(PortionC != 0)

Q10q <- left_join(filter(ElapsedTime.summary, Treatment == "IB 20")[,c("PortionC", "Depth", "N", "ElapsedTime", "sd", "se")], 
                   filter(ElapsedTime.summary, Treatment == "IB 30")[,c("PortionC", "Depth", "N", "ElapsedTime", "sd", "se")], 
                   by = c("Depth", "PortionC"), 
                   suffix = c(".20", ".30")) %>% 
  as_tibble() %>% 
  rename(dElapsedTime.30 = sd.30, dElapsedTime.20 = sd.20) %>% 
  mutate_with_error(Q10q ~ ElapsedTime.20/ElapsedTime.30) %>% 
  mutate(N = N.30 + N.20,
         se = dQ10q / sqrt(N)) %>% 
  select("PortionC", "Depth", "Q10q", "sd" = "dQ10q", "N", "se")
