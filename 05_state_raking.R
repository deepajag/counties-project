

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
library(boot)
library(covidcast)
library(viridis)


#Filepaths
in.path = "~/Documents/data_counties/Data/Processed/"
out.path = "~/Documents/counties-project/Results/"

acs = readRDS(paste0(out.path,"ACS_prediction.RDS"))
yrbs = readRDS(paste0(in.path,"YRBS_final.RDS"))
yrbs$age = factor(yrbs$age, levels = c(15,16,17,18,19)) ##Dunno why this isn't saving in data processing

acs_state = acs[,.(state, county_prediction, pop2019, county_fips,  age, sex)]
yrbs = yrbs[,.(sitename, weight, hadsex, age, sex)]

state_names = fread(paste0("~/Documents/data_counties/Data/Raw/state_names.csv"))
yrbs = merge(yrbs, state_names[,.(sitename = State, state = Code)], by.x = "sitename", by.y = "sitename")

##Strata, state average
s = svydesign(ids = ~0, weights = ~weight, data = yrbs)
yrbs_strata = svyby(~hadsex, ~age + sex + state, s, svymean)
yrbs_strata = as.data.table(yrbs_strata)

acs_strata = copy(acs_state)[,
          lapply(.SD, function(x) weighted.mean(x,pop2019,na.rm=TRUE)), by = c("age","sex","state"),
          .SDcols = "county_prediction"]
setnames(acs_strata, "county_prediction","acs_state")
setnames(yrbs_strata, "hadsex","yrbs_state")

acs_strata = acs_strata[age != 19]

#Create 'raking factor', i.e. ratio of state level estimates
combined = merge(acs_strata, yrbs_strata, by = c("age","sex","state"), all.x = TRUE)
combined[,scale_factor := acs_state/yrbs_state]

#Adjust the county values according to scaling factor
acs[,scale_factor := NULL]
acs = merge(acs, combined[,.(age,sex,state,scale_factor)], by = c("age","sex","state"))
acs[,raked := prediction * scale_factor]

##Generate map
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

setnames(acs,"state_code","state_fips")
household_data <- left_join(counties_sf, acs[,.(county_fips, raked)], by="county_fips")
household_data$group <- cut(household_data$raked, 
                            c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                            include.lowest = TRUE)

ggplot(household_data) + 
  geom_sf(mapping = aes(fill = factor(group)), size = 0.05, col="white") +
  coord_sf(datum = NA) + 
  scale_fill_manual(breaks = sort(unique(household_data$group)),
                     values = c("darkblue","red","green","darkorange", "brown","pink"))

##State averages only for comparison
s = svydesign(ids = ~0, weights = ~weight, data = yrbs)
yrbs_strata = svyby(~hadsex, ~ state, s, svymean)
yrbs_strata = as.data.table(yrbs_strata)

acs = acs[age != 19]
acs_state = acs[,.(state, county_prediction, pop2019)]
acs_strata = copy(acs_state)[,
                             lapply(.SD, function(x) weighted.mean(x, pop2019,na.rm=TRUE)), 
                             by = c("state"),
                             .SDcols = "county_prediction"]
setnames(acs_strata, "county_prediction","acs_state")
setnames(yrbs_strata, "hadsex","yrbs_state")

#Generate map
counties_sf <- get_urbn_map(map = "states", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

##ACS
household_data <- left_join(counties_sf, acs_strata[,.(state_abbv = state, acs_state)], by="state_abbv")
household_data$group <- cut(household_data$acs_state, 
                            c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                            include.lowest = TRUE)
ggplot(household_data) + 
  geom_sf(mapping = aes(fill = factor(group)), size = 0.05, col="white") +
  coord_sf(datum = NA) + 
  scale_fill_manual(breaks = sort(unique(household_data$group)),
                     values = c("green","darkorange"))

#YRBS
household_data <- left_join(counties_sf, yrbs_strata[,.(state_abbv = state, yrbs_state)], by="state_abbv")
household_data$group <- cut(household_data$yrbs_state, 
                            c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                            include.lowest = TRUE)
ggplot(household_data) + 
  geom_sf(mapping = aes(fill = factor(group)), size = 0.05, col="white") +
  coord_sf(datum = NA) + 
  scale_fill_manual(breaks = sort(unique(household_data$group)),
                     values = c("green","darkorange", "brown"))

#Scatterplot comparing states
all_state = merge(acs_strata[,.(state_abbv = state,  acs_state)],
                  yrbs_strata[,.(state_abbv = state, yrbs_state)], by = "state_abbv")
reg <- lm(yrbs_state ~ acs_state, data = all_state)
coeff=coefficients(reg)

ggplot(all_state, aes(acs_state, yrbs_state)) + 
  geom_point(size = 2) + theme_bw() + 
  geom_text(aes(acs_state, yrbs_state, label = state_abbv), hjust = -0.5, size = 3) +
  geom_abline(intercept = coeff[1], slope = coeff[2], linetype = "dashed", size = 1, col="darkred" )










