## Testing multiple variables to compare to D. Thoma's v2 spreadsheet model

rm(list = ls())

library(lubridate)
library(dplyr)

## Input and prep

setwd("C:/Users/msears/OneDrive - DOI/WB_crosscheck/")

frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

## Functions

# Jenning's coefficient is 2.1555781 per D.Thoma v2 model inputs for Frog Rock
low_thresh_temp = frog$Jenningcoeff-3 
high_thresh_temp = frog$Jenningcoeff+3

# Get freeze first - corresponds with 'F' in D.Thoma v2 model
get_freeze = function(low_thresh_temp, tmean, high_thresh_temp){
  freeze = ifelse(
    tmean <= low_thresh_temp, 0, 
    ifelse(tmean >= high_thresh_temp,
           1, (0.167*(tmean-low_thresh_temp))))
}

frog$freeze <- get_freeze(low_thresh_temp, frog$tmean, high_thresh_temp)

# Get rain - corrpesonds with 'RAIN' in D.Thoma v2model
get_rain = function(freeze, precip){
  rain = freeze*precip
  return(rain)
}

frog$rain <- get_rain(frog$freeze, frog$precip_mmday)

# Get snow - corresponds with 'SNOW' in D.Thoma v2 model
get_snow = function(freeze, precip){
  snow = (1-freeze)*precip
  return(snow)
}

frog$snow <- get_snow(frog$freeze, frog$precip_mmday)

# Get melt
#get_melt = function(tmean, low_thresh_temp, pack, hock, p.0=NULL){
#  p.i = ifelse(!is.null(p.0), p.0, 0)
#  melt = c()
#  for(i in 1:length(tmean)){
#    melt[i] = ifelse(tmean[i]<low_thresh_temp||p.i==0, 0, 
#                     ifelse((tmean[i]-low_thresh_temp)*hock[i]>p.i, 
#                            p.i, (tmean[i]-low_thresh_temp)*hock[i]))
#    p.i = pack[i]
#  }
#  return(melt)
#}

#frog$melt <- get_melt(frog$tmean, low_thresh_temp, frog$pack, frog$hock, p.0 = NULL)

# Get pack
#get_pack = function(snow, melt, p.0=NULL){
#  p.i = ifelse(!is.null(p.0), p.0, 0)
#  pack = c()
#  for (i in 1:length(snow)){
#    pack[i] = p.i+snow[i]-melt[i]
#    p.i=pack[i]
#  }
#  return(pack)
#}

#frog$pack <- get_pack(frog$snow, frog$melt, p.0=NULL)

#adding melt and pack loops together
get_pack_melt = function(tmean,low_thresh_temp, hock, snow, p.0=NULL){
    p.i = ifelse(!is.null(p.0), p.0, 0)
    melt=c()
    for(i in 1:length(tmean)){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||p.i==0, 0, 
                       ifelse((tmean[i]-low_thresh_temp)*hock[i]>p.i, 
                              p.i, (tmean[i]-low_thresh_temp)*hock[i]))
      pack=c()
      for (j in 1:length(snow)){
      pack[j] = p.i+snow[j]-melt[j]
      p.i=pack[j]
      melt_pack <- list(melt, pack)
      }
    }
    return(melt_pack)
}      
        
frog$pack <- get_pack_melt(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, p.0=NULL)
frog$melt <- get_pack_melt(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, p.0=NULL)

###########################
test <- frog

# THESE TWO LOOPS WORK
pack.init <- 0
for(i in 1:2){
    for (j in 1:2){
    test$melt[i] = ifelse(test$tmean[i]<low_thresh_temp||pack.init==0, 0, 
                     ifelse((test$tmean[i]-low_thresh_temp)*test$Hock[i]>pack.init, 
                            pack.init, (test$tmean[i]-low_thresh_temp)*test$Hock[i]))
      test$pack[j] = pack.init+test$snow[j]-test$melt[j]
    }
}

p.0=NULL
p.i = ifelse(!is.null(p.0), p.0, 0)

for(i in 1:length(test$tmean)){
  for (j in 1:(length(test$tmean))){
    test$melt[i] = ifelse(test$tmean[i]<low_thresh_temp||p.i==0, 0, 
                          ifelse(((test$tmean[i]-low_thresh_temp)*test$Hock[i])>p.i, 
                                 p.i, ((test$tmean[i]-low_thresh_temp)*test$Hock[i])))
    test$pack[j] = p.i+test$snow[j]-test$melt[j]
    p.i=test$pack[j]
  }
}
############################

test$pack <- get_pack_melt(test$tmean, low_thresh_temp, test$Hock, test$snow, p.0=NULL)
test$melt <- get_pack_melt(test$tmean, low_thresh_temp, test$Hock, test$snow, p.0=NULL)

## Write as csv to compare (rain, snow, pack, melt) to DT and MT before moving on
write.csv(frog, "C:/Users/msears/OneDrive - DOI/WB-cross check/SWE_output")

# W (water reaching soil as rain + melt) - rain + melt
get_W = function(rain, melt){
  W = (melt+rain)
  return(W)
}

frog$W <- get_W(frog$rain, frog$melt)

# PET using Oudin equation - first calculate rad Ra, Oudin location, then PET
latitude = 44.95354

#Get R.a for Oudin location
get_R.a = function(doy, lat, elev){
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  return(R.a)
}

#Oudin location
get_Oudin = function(){
  Oudin = ifelse(pack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.405)/100,0))
  return(Oudin)
}

# Heatload (then can determine PET)

# PET

# W - PET

# Soil (inital value of 104)

# Delta soil

# AET (use initial soil value as 104)

# W-ET-Delta soil

# Deficit (PET-AET)

#GDD (based off tmean and Tbase constant[0])

