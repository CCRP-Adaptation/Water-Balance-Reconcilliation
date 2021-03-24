### Test 2 - using the package to test ###

library(dplyr)

devtools::load_all()

## Input and prep
frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

#correct values for compare
correct <- read.csv("./tests/CorrectVars.csv")
test <- correct #for comparing values

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

Lat <- 44.95354
Lon <- -110.54083

## Test functions from WB v2 package

frog$jtemp <- get_jtemp(Lat, Lon)

frog$freeze <- get_freeze(frog$jtemp, frog$tmean)

frog$rain <- get_rain(frog$precip_mmday,frog$freeze)
test$RAIN <- frog$rain - correct$RAIN

frog$snow <- get_snow(frog$precip_mmday, frog$freeze)
test$SNOW <- frog$snow - correct$SNOW

frog$melt <- get_melt(frog$tmean, frog$jtemp, frog$Hock, frog$snow)
test$MELT <- frog$melt - correct$MELT

frog$snowpack <- get_snowpack(frog$jtemp, frog$snow, frog$melt)
test$PACK <- frog$snowpack - correct$PACK

frog$W <- get_w(frog$rain, frog$melt)
test$W <- frog$W - correct$W

frog$PET <- get_OudinPET(frog$yday, Lat, frog$snowpack, frog$tmean, slope=2, frog$aspect, shade.coeff = 1)
test$PET <- frog$PET - correct$PET

#this is from the test 1 that uses 3.14 instead of pi - Thoma did not use "pi" in excel model
get_OudinPET = function(doy, lat, pack, tmean, slope, aspect, shadecoeff=1){
  d.r = 1 + 0.033*cos((2*3.14159/365)*doy)
  declin = 0.409*sin((((2*3.14)/365)*doy)-1.39)
  lat.rad = (3.14159/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/3.14159)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
  Oudin = ifelse(pack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
  Folded_aspect = abs(180-abs((aspect)-225))
  Heatload = (0.339+0.808*cos(REdaS::deg2rad(lat))*cos(REdaS::deg2rad(slope)))-(0.196*sin(REdaS::deg2rad(lat))*sin(REdaS::deg2rad(slope)))-(0.482*cos(REdaS::deg2rad(Folded_aspect))*sin(REdaS::deg2rad(slope)))
  OudinPET = Oudin * Heatload * shadecoeff
  return(OudinPET)
}

frog$PET2 <- get_OudinPET(frog$yday, Lat, frog$snowpack, frog$tmean, slope=2, frog$aspect, shadecoeff=1)
test$PET2 <- frog$PET2 - correct$PET # this confirms the only difference in the PET function in WB package is the difference of using pi vs. 3.14
rm(get_OudinPET)

frog$w_pet <- get_w_pet(frog$W, frog$PET2) #using PET2 here or else rest of values won't match - the only difference is a matter of 3.14 vs pi
test$W...PET <- frog$w_pet - correct$W...PET

frog$swc <- get_soil(frog$W, swc.0=104, frog$PET2, frog$w_pet, swc.max=104)
test$SOIL <- frog$swc - correct$SOIL

frog$delta.soil <- get_d_soil(frog$swc, swc.0=104)
test$X..SOIL <- frog$delta.soil - correct$X..SOIL

frog$aet <- get_AET(frog$W, frog$PET2, frog$swc, swc.0=104)
test$AET <- frog$aet - correct$AET

frog$runoff <- get_runoff(frog$precip_mmday, frog$W, frog$delta.soil, frog$aet)
test$W.ET...SOIL <- frog$runoff - correct$W.ET...SOIL

frog$D <- get_deficit(frog$PET2, frog$aet)
test$D <- frog$D - correct$D

frog$GDD <- get_GDD(frog$tmean)
test$GDD <- frog$GDD - correct$GDD
