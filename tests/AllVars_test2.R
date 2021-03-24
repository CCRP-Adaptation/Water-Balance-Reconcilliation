### Test 2 - using the package to test ###

library(dplyr)

devtools::load_all()

## Input and prep
frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

#correct values for compare
correct <- read.csv("./tests/CorrectVars.csv")
test <- correct %>%
  select(Date)

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

Lat <- 44.95354
Lon <- -110.54083

## Test functions from WB v2 package

frog$jtemp <- get_jtemp(Lat, Lon)

frog$freeze <- get_freeze(frog$jtemp, frog$tmean)

frog$rain <- get_rain(frog$precip_mmday,frog$freeze)
test$RAIN <- frog$rain - correct$RAIN
max(test$RAIN)

frog$snow <- get_snow(frog$precip_mmday, frog$freeze)
test$SNOW <- frog$snow - correct$SNOW
max(test$SNOW)

frog$melt <- get_melt(frog$tmean, frog$jtemp, frog$Hock, frog$snow, sp.0=100) #test to make sure it works with values other than 0
test$MELT <- frog$melt - correct$MELT
max(test$MELT)

frog$snowpack <- get_snowpack(frog$jtemp, frog$snow, frog$melt, sp.0=100) #test to make sure it works with values other than 0
test$PACK <- frog$snowpack - correct$PACK
max(test$PACK)

frog$W <- get_w(frog$rain, frog$melt)
test$W <- frog$W - correct$W
max(test$W)

frog$PET <- get_OudinPET(frog$yday, Lat, frog$snowpack, frog$tmean, slope=2, frog$aspect, shade.coeff = 1)
test$PET <- frog$PET - correct$PET
max(test$PET)

#this is from the test 1 that uses 3.14 instead of pi - Thoma did not use "pi" in excel model
get_OudinPET2 = function(doy, lat, pack, tmean, slope, aspect, shadecoeff=1){
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

frog$PET2 <- get_OudinPET2(frog$yday, Lat, frog$snowpack, frog$tmean, slope=2, frog$aspect, shadecoeff=1)
test$PET2 <- frog$PET2 - correct$PET # this confirms the only difference in the PET function in WB package is the difference of using pi vs. 3.14
max(test$PET2)
rm(get_OudinPET2)

frog$w_pet <- get_w_pet(frog$W, frog$PET2) #using PET2 here or else rest of values won't match - the only difference is a matter of 3.14 vs pi
test$W...PET <- frog$w_pet - correct$W...PET
max(test$W...PET)

frog$swc <- get_soil(frog$W, swc.0=104, frog$PET2, frog$w_pet, swc.max=104)
test$SOIL <- frog$swc - correct$SOIL
max(test$SOIL)

frog$delta.soil <- get_d_soil(frog$swc, swc.0=104)
test$X..SOIL <- frog$delta.soil - correct$X..SOIL
max(test$X..SOIL)

frog$aet <- get_AET(frog$W, frog$PET2, frog$swc, swc.0=104)
test$AET <- frog$aet - correct$AET
max(test$AET)

frog$runoff <- get_runoff(frog$precip_mmday, frog$W, frog$delta.soil, frog$aet)
test$W.ET...SOIL <- frog$runoff - correct$W.ET...SOIL
max(test$W.ET...SOIL)

frog$D <- get_deficit(frog$PET2, frog$aet)
test$D <- frog$D - correct$D
max(test$D)

frog$GDD <- get_GDD(frog$tmean)
test$GDD <- frog$GDD - correct$GDD
max(test$GDD)

#all Vars using WB package were equal to Thoma's spreadsheet values. This is the second check. The difference between check 1 and check 2 uses WB package functions.

############################################################################
#Melt loop currently works - trying to write it better
get_melt2 = function(tmean,j_temp, hock, snow, sp.0=NULL){
  sp.0 = ifelse(!is.null(sp.0), sp.0, 0)
  low_thresh_temp = j_temp - 3
  melt <- vector()
  for (i in 1:1){
    melt[i] = ifelse(tmean[i]<low_thresh_temp||sp.0==0, 0, 
                     ifelse(((tmean[i]-low_thresh_temp)*hock[i])>sp.0, 
                            sp.0, ((tmean[i]-low_thresh_temp)*hock[i])))
  }
  snowpack = sp.0+snow+melt
  for(i in 2:length(tmean)){
    for (j in 2:(length(tmean))){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||snowpack[i-1]==0, 0, 
                       ifelse(((tmean[i]-low_thresh_temp)*hock[i])>snowpack[i-1], 
                              snowpack[i-1], ((tmean[i]-low_thresh_temp)*hock[i])))
      snowpack[j] = snowpack[j-1]+snow[j]-melt[j]
    }
  }
  return(melt)
}


frog$melt2 <- get_melt2(frog$tmean, frog$jtemp, frog$Hock, frog$snow)

test$MELT2 <- frog$melt2 - correct$MELT #function above works. added to WB package -  now test from package (above)
max(test$MELT2)
