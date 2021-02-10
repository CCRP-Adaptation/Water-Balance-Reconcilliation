#############################################################################
######    TEST SNOW-WATER EQUIVALENT AGAINST D. THOMA'S EXCEL MODEL  ########
#############################################################################

# PACK (D. Thoma) = init.value - melt + snowfall

rm(list = ls())

library(lubridate)
library(dplyr)

## Input and prep

setwd("C:/Users/msears/OneDrive - DOI/WB_crosscheck/")

frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

## Functions

# Get freeze first
low_thresh_temp = frog$Jenningcoeff-3 
high_thresh_temp = frog$Jenningcoeff+3

get_freeze = function(low_thresh_temp, tmean, high_thresh_temp){
  freeze = ifelse(
    tmean <= low_thresh_temp, 0, 
    ifelse(tmean >= high_thresh_temp,
           1, (0.167*(tmean-low_thresh_temp))))
}

frog$freeze <- get_freeze(low_thresh_temp, frog$tmean, high_thresh_temp)

# Get rain
get_rain = function(freeze, precip){
  rain = freeze*precip
  return(rain)
}

frog$rain <- get_rain(frog$freeze, frog$precip_mmday)

# Get snow
get_snow = function(freeze, precip){
  snow = (1-freeze)*precip
  return(snow)
}

frog$snow <- get_snow(frog$freeze, frog$precip_mmday)

# Get melt
get_melt = function(tmean, low_thresh_temp, pack, hock, p.0=NULL){
  p.i = ifelse(!is.null(p.0), p.0, 0)
  melt = c()
  for(i in 1:length(tmean)){
    melt[i] = ifelse(tmean[i]<low_thresh_temp||p.i==0, 0, 
                     ifelse((tmean[i]-low_thresh_temp)*hock[i]>p.i, 
                            p.i, (tmean[i]-low_thresh_temp)*hock[i]))
    p.i = pack[i]
  }
  
  return(melt)
}

frog$melt <- get_melt(frog$tmean, low_thresh_temp, frog$pack, frog$hock, p.0 = NULL)

# Get pack
get_pack = function(snow, melt, p.0=NULL){
  p.i = ifelse(!is.null(p.0), p.0, 0)
  pack = c()
  for (i in 1:length(snow)){
    pack[i] = p.i+snow[i]-melt[i]
    p.i=pack[i]
  }
  return(pack)
}

frog$pack <- get_pack(frog$snow, frog$melt, p.0=NULL)

#--------------------------------------------------------------------------------#
#adding melt and pack loops together
get_pack_melt = function(tmean,low_thresh_temp, hock, snow, p.0=NULL){
    p.i = ifelse(!is.null(p.0), p.0, 0)
    melt=c()
    pack=c()
    for(i in 1:length(tmean)){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||p.i==0, 0, 
                       ifelse((tmean[i]-low_thresh_temp)*hock[i]>p.i, 
                              p.i, (tmean[i]-low_thresh_temp)*hock[i]))
      pack[i] = p.i+snow[i]-melt[i]
        p.i=pack[i]
    }
    return(melt)
    return(pack)
}      
        
frog$pack <- get_pack_melt(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, p.0=NULL)
frog$melt <- get_pack_melt(frog$tmean, low_thresh_temp, frog$Hock, frog$snow, p.0=NULL)

## Write as csv to compare to DT and MT
write.csv(frog_input, "C:/Users/msears/OneDrive - DOI/WB-cross check/SWE_output")
