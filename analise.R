library(tidyverse)
library(readr)

ds <- read_csv("Bike_Sharing.csv")


options(scipe = 999)
attach(ds)


ds$season <- as.factor(ds$season)
ds$yr <- as.factor(ds$yr)
ds$holiday <- as.factor(ds$holiday)
ds$weekday <- as.factor(ds$weekday)
ds$workingday <- as.factor(ds$workingday)
ds$weathersit <- as.factor(ds$weathersit)
ds$mnth <- as.factor(ds$mnth)

summary(ds)

fa_holiday = table(holiday);fa_holiday
fr_holiday = fa_holiday/sum(fa_holiday)
dist_holiday = cbind(fa_holiday, fr_holiday);
dist_holiday

fa_weekday = table(weekday);fa_weekday
fr_weekday = fa_weekday/sum(fa_weekday);fr_weekday
dist_weekday = cbind(fa_weekday, fr_weekday);
dist_weekday


fa_workingday = table(workingday);fa_workingday
fr_workingday = fa_workingday/sum(fa_workingday);fr_workingday
dist_workingday = cbind(fa_workingday, fr_workingday);
dist_workingday

fa_weathersit = table(weathersit);fa_weathersit
fr_weathersit = fa_weathersit/sum(fa_weathersit);fr_weathersit
dist_weathersit = cbind(fa_weathersit, fr_weathersit);
dist_weathersit

fa_season = table(season);fa_season
fr_season = fa_season/sum(fa_season);fr_season
dist_season = cbind(fa_season, fr_season);
dist_season


correlacao = cor.test(cnt, temp, method = c("pearson"))
correlacao


