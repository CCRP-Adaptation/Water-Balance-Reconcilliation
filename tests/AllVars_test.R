## froging multiple variables to compare to D. Thoma's v2 spreadsheet model

rm(list = ls())

library(lubridate)
library(dplyr)
library(REdaS)

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

# Get rain - corrpesonds with 'RAIN' in D.Thoma v2 model
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

# Get melt - corresponds with 'MELT' in D.Thoma v2 model
get_melt = function(tmean,low_thresh_temp, hock, snow, pack=0){
  melt <- vector()
  pack <- 0 #this is the init value
  for(i in 1:length(tmean)){
    for (j in 2:(length(tmean))){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||pack[i-1]==0, 0, 
                            ifelse(((tmean[i]-low_thresh_temp)*hock[i])>pack[i-1], 
                                   pack[i-1], ((tmean[i]-low_thresh_temp)*hock[i])))
      pack[j] = pack[j-1]+snow[j]-melt[j]
    }
  }
  return(melt)
}

frog$melt <- get_melt(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, pack=0)

# Get pack - corresponds with 'PACK' in D. Thoma v2 model
get_pack = function(tmean,low_thresh_temp, hock, snow, pack=0){
  melt <- vector()
  pack <- 0 #this is the init value
  for(i in 1:length(tmean)){
    for (j in 2:(length(tmean))){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||pack[i-1]==0, 0, 
                       ifelse(((tmean[i]-low_thresh_temp)*hock[i])>pack[i-1], 
                              pack[i-1], ((tmean[i]-low_thresh_temp)*hock[i])))
      pack[j] = pack[j-1]+snow[j]-melt[j]
    }
  }
  return(pack)
}

frog$pack <- get_pack(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, pack=0)

# W (water reaching soil as rain + melt) - rain + melt
get_W = function(rain, melt){
  W = (melt+rain)
  return(W)
}

frog$W <- get_W(frog$rain, frog$melt)

# Oudin PET (Oudin location * heatload * PET shade coeff) - corresponds with 'PET' in D. Thoma v2 model
#!!!!!!!# use this Oudin PET function in the WB model since it actually uses pi
#get_OudinPET = function(doy, lat, pack, tmean, slope, aspect, shadecoeff=1){
#  d.r = 1 + 0.033*cos((2*pi/365)*doy)
#  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
#  lat.rad = (pi/180)*lat
#  sunset.ang = acos(-tan(lat.rad)*tan(declin))
#  R.a = ((24*60)/pi)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
#  Oudin = ifelse(pack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
#  Folded_aspect = abs(180-abs((aspect)-225))
#  Heatload = (0.339+0.808*cos(deg2rad(lat))*cos(deg2rad(slope)))-(0.196*sin(deg2rad(lat))*sin(deg2rad(slope)))-(0.482*cos(deg2rad(Folded_aspect))*sin(deg2rad(slope)))
#  OudinPET = Oudin * Heatload * shadecoeff
#  return(OudinPET)
#}

#frog$PET <- get_OudinPET(frog$yday, latitude, frog$pack, frog$tmean, frog$slope, frog$aspect, shadecoeff=1)

latitude=44.95354

#!! this is the same function as above EXCEPT it usues 3.14 or 3.14519 instead of pi in order to replciate what Thoma did
get_OudinPET = function(doy, lat, pack, tmean, slope, aspect, shadecoeff=1){
  d.r = 1 + 0.033*cos((2*3.14159/365)*doy)
  declin = 0.409*sin((((2*3.14)/365)*doy)-1.39)
  lat.rad = (3.14159/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/3.14159)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
  Oudin = ifelse(pack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
  Folded_aspect = abs(180-abs((aspect)-225))
  Heatload = (0.339+0.808*cos(deg2rad(lat))*cos(deg2rad(slope)))-(0.196*sin(deg2rad(lat))*sin(deg2rad(slope)))-(0.482*cos(deg2rad(Folded_aspect))*sin(deg2rad(slope)))
  OudinPET = Oudin * Heatload * shadecoeff
  return(OudinPET)
}

frog$PET <- get_OudinPET(frog$yday, latitude, frog$pack, frog$tmean, slope=2, frog$aspect, shadecoeff=1)

# W - PET- corresponds with 'W-PET' in D. Thoma v2 model
get_W_PET = function(W, PET){
  W_PET = (W-PET)
  return(W_PET)
}

frog$W_PET <- get_W_PET(frog$W, frog$PET)


# Soil (inital value of 104)

# Delta soil

# AET (use initial soil value as 104)

# W-ET-Delta soil

# Deficit (PET-AET)

#GDD (based off tmean and Tbase constant[0])

## Write as csv to compare to DT and MT
write.csv(frog, "C:/Users/msears/OneDrive - DOI/WB_crosscheck/frog_check.csv")

