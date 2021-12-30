

rm(list=ls())
ß

library(readstata13)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(weights)
library(spdep)
library(proj4)
library(maptools)
library(randomForest)
library(caret)
library(urbnmapr)


#Filepaths
in.path = "~/Documents/county_project/Data/Processed/"
out.path = "~/Documents/county_project/Results/"

acs = readRDS(paste0(out.path,"ACS_prediction.RDS"))
yrbs = readRDS(paste0(in.path,"YRBS_final.RDS"))
yrbs$age = factor(yrbs$age, levels = c(15,16,17,18,19)) ##Dunno why this isn't saving in data processing

acs_state = acs[,.(state, prediction, pop10, county_fips, weight, age, sex)]
yrbs = yrbs[,.(sitename, weight, hadsex, age, sex)]

state_names = fread(paste0("~/Documents/county_project/Data/Raw/state_names.csv"))
yrbs = merge(yrbs, state_names[,.(sitename = State, state = Code)], by.x = "sitename", by.y = "sitename")

##Strata, state average
s = svydesign(ids = ~0, weights = ~weight, data = yrbs)
yrbs_strata = svyby(~hadsex, ~age + sex + state, s, svymean)
yrbs_strata = as.data.table(yrbs_strata)

acs_strata = copy(acs_state)[,
          lapply(.SD, function(x) weighted.mean(x,weight,na.rm=TRUE)), by = c("age","sex","state"),
          .SDcols = "prediction"]
setnames(acs_strata, "prediction","acs_state")
setnames(yrbs_strata, "hadsex","yrbs_state")


acs_strata = acs_strata[age != 19]

#Create 'raking factor', i.e. ratio of state level estimates
combined = merge(acs_strata, yrbs_strata, by = c("age","sex","state"), all.x = TRUE)
combined[,scale_factor := acs_state/yrbs_state]


#Adjust the county values according to scaling factor
acs = merge(acs, combined[,.(age,sex,state,scale_factor)], by = c("age","sex","state"))
acs[,raked := prediction * scale_factor]
acs[raked > 1] ##Checking where values are above 1


##Generate map
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

setnames(acs,"state_code","state_fips")
household_data <- left_join(counties_sf, acs[,.(county_fips, raked)], by="county_fips")
ggplot(household_data) + geom_sf(mapping = aes(fill = raked),color = "lightgrey", size = 0.1) +
  coord_sf(datum = NA)


##State averages only for comparison
s = svydesign(ids = ~0, weights = ~weight, data = yrbs)
yrbs_strata = svyby(~hadsex, ~ state, s, svymean)
yrbs_strata = as.data.table(yrbs_strata)

acs = acs[age != 19]
acs_state = acs[,.(state, prediction, pop10)]
acs_strata = copy(acs_state)[,
                             lapply(.SD, function(x) weighted.mean(x, pop10,na.rm=TRUE)), 
                             by = c("state"),
                             .SDcols = "prediction"]
setnames(acs_strata, "prediction","acs_state")
setnames(yrbs_strata, "hadsex","yrbs_state")

combined = merge(acs_strata, yrbs_strata, by = c("state"), all.x = TRUE)
combined[,scale_factor := yrbs_state/acs_state]

#Generate map
counties_sf <- get_urbn_map(map = "states", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

##ACS
household_data <- left_join(counties_sf, acs_strata[,.(state_abbv = state, acs_state)], by="state_abbv")
ggplot(household_data) + geom_sf(mapping = aes(fill = acs_state),color = "lightgrey", size = 0.1) +
  coord_sf(datum = NA)

#Y#RBS
household_data <- left_join(counties_sf, yrbs_strata[,.(state_abbv = state, yrbs_state)], by="state_abbv")
ggplot(household_data) + geom_sf(mapping = aes(fill = yrbs_state),color = "lightgrey", size = 0.1) +
  coord_sf(datum = NA)
