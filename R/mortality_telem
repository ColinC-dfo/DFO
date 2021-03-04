# Needs a dataframe with a column named "Distance" and arranged by datetime.

# The function starts at the most recent telemetry detections and works backwards
# looking for the last "movement" (detection on another receiver). The returned value 
# isn't the real deathtime, as the fish may still be alive, but it helps to clean 
# up the data. 

# For fish that are still alive, and moving the estimated deathTime will be close to the 
# most recent download time from the receivers. Otherwise it will extract the last time a 
# fish showed a "movement" based on the telemetry detections

deathDate = function(dataframe){
  test = NULL
  x = dataframe$Distance
  for(i in 1:length(x)){
    test[i] = mean(x[(length(x) - i):length(x)], na.rm = TRUE)
    if(test[i] > 0){
      i = i-1
      break
    } else if(all(test == 0)){
      i = length(x)-1
    }
  }
  deathTime = dataframe$Date[(length(x)-i)]
  return(deathTime)
}

# simulate a dataset, and set the distance moved to 0 for detection >= July 15, 2016
dat <- data.frame(Date = sample(seq(as.POSIXct('2017/06/01'), as.POSIXct('2017/07/31'), by = "15 mins"), 100),
           Distance = rnorm(100, 5, 1)) %>% 
  mutate(Distance = ifelse(Date >= as.POSIXct('2017/07/15'), 0, Distance)) %>% 
  arrange(Date)

# check out the data around where we set the distance to be zero
dat %>% 
  dplyr::filter(Date > as.POSIXct('2017/07/13') & Date <= as.POSIXct('2017/07/19'))

# The last "recorded movement" is designated as the deathtime
deathDate(dat)
