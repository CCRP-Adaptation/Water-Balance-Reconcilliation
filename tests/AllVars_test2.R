### Test 2 - using the package to test ###

library(dplyr)

## Input and prep

setwd("C:/Users/msears/OneDrive - DOI/WB_crosscheck/")

frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

Lat <- 44.95354
Lon <- -110.54083

## Test functions from WB v2 package

frog$jtemp <- get_jtemp(Lat, Lon)

frog$freeze <- get_freeze(frog$jtemp, frog$tmean)

frog$rain <- get_rain(frog$precip_mmday,frog$freeze)

frog$snow <- get_snow(frog$precip_mmday, frog$freeze)

frog$melt <- get_melt(frog$tmean, frog$jtemp, frog$Hock, frog$snow)

frog$snowpack <- get_snowpack(frog$jtemp, frog$snow, frog$melt)

frog$W <- get_w(frog$rain, frog$melt)

frog$PET <- get_OudinPET(frog$d)

