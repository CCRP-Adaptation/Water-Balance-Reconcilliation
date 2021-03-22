### Test 2 - using the package to test ###

library(dplyr)

devtools::load_all()

## Input and prep

#setwd("C:/Users/msears/OneDrive - DOI/WB_crosscheck/")

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

frog$PET <- get_OudinPET(frog$yday, Lat, frog$snowpack, frog$tmean, frog$slope, frog$aspect, shade.coeff = 1)

frog$w_pet <- get_w_pet(frog$W, frog$PET)

frog$swc <- get_soil(frog$W, swc.0=104, frog$PET, frog$w_pet, swc.max=104)

frog$delta.soil <- get_d_soil(frog$swc, swc.0=104)

frog$aet <- get_AET(frog$W, frog$PET, frog$swc, swc.0=104)

