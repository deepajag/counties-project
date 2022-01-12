
rm(list=ls())


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
in.path = "~/Documents/data_counties/Data/Processed/"
out.path = "~/Documents/counties-project/Results/"

#Generate predictions for ACS (no need to redo every time)
# m1 = readRDS(paste0(out.path, "171221_logit_model.RDS"))
# acs = readRDS(paste0(in.path,"ACS_final.RDS"))
# 
# acs = acs[complete.cases(acs)]
# acs$age = factor(acs$age, levels = c(15,16,17,18,19)) ##Dunno why this isn't saving in data processing
# acs$prediction = predict(m1$model, newdata = acs, type="response")
# acs$prediction <- as.numeric(acs$prediction)
# acs[,county_prediction := weighted.mean(prediction, weight), by = "county_fips"]
# 
# saveRDS(acs, paste0(out.path,"ACS_prediction.RDS"))

##Generate map
acs = readRDS(paste0(out.path,"ACS_prediction.RDS"))
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)

county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

setnames(acs,"state_code","state_fips")
household_data <- left_join(counties_sf, acs[,.(county_fips, county_prediction)], by="county_fips")
ggplot(household_data) + geom_sf(mapping = aes(fill = county_prediction),color = "lightgrey", size = 0.1) +
  coord_sf(datum = NA)








