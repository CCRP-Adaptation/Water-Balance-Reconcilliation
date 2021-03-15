### Test 2 ###

## testing multiple variables to compare to D. Thoma's v2 spreadsheet model

library(lubridate)
library(dplyr)
library(REdaS)
library(raster)

## Input and prep

rm(list = ls())

setwd("C:/Users/msears/OneDrive - DOI/WB_crosscheck/")

frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB_crosscheck/FrogRock-inputforR.csv")

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

## Test functions from WB v2 package

