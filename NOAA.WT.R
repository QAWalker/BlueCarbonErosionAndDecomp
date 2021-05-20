#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

NOAA.WT <- function(begin_date, end_date, stationid, time_zone="gmt", units="metric", throttle = F){
  require(rnoaa)
  require(lubridate)
  require(dplyr)
  
  dates <- seq.Date(from = ymd(begin_date), to = ymd(end_date), by = "30 days")
  if (max(dates, na.rm = T)!=ymd(end_date)) {dates <- as.Date(c(dates, ymd(end_date) + days(1))) }
  
  for (d in 1:(length(dates)-1)) {
    i = dates[[d]]
    begindate = paste0(
      year(i), 
      ifelse(month(i)<10, paste0("0", month(i)), month(i)),
      ifelse(day(i)<10, paste0("0", day(i)), day(i))
    )
    
    j = dates[[d+1]]
    enddate = paste0(
      year(j), 
      ifelse(month(j)<10, paste0("0", month(j)), month(j)),
      ifelse(day(j)<10, paste0("0", day(j)), day(j))
    )
    
    tryCatch(
      expr = {
        temp <- rnoaa::coops_search(
          begin_date = begindate,
          end_date = enddate,
          station_name = stationid,
          product = "water_temperature",
          time_zone = time_zone,
          units = units
        )
        if(!exists("wt")){
          wt <- temp
        } else{
          wt$data <- bind_rows(wt$data, temp$data)
        }
      }, error = function(e){
        message(paste0("From ", ymd(begindate), " to ", ymd(enddate), ", ", e))
      }
    )
    if (throttle) {
      Sys.sleep(3.6) 
      #the server can only handle 1000 requests an hour. This slows the code enought to keep it below the limit
      #only need this if you are using a time_zone other than GMT
    }
  }
  if(exists("wt")){
    wt$data <- filter(
      wt$data, 
      t >= ymd(begin_date) & t <= ymd(end_date), 
      !is.na(v))
    return(wt)
  } else{
    return(NA)
  }
}
