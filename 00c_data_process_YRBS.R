
##This script processes the raw YRBS data, downloadded from public use files
##It also checks for data availability in different states

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
library(stringr)
library(matrixStats)
library(haven)
library(censusGeography)
library(urbnmapr)


#Filepaths
in.path = "~/Documents/data_counties/Data/Raw/"
out.path = "~/Documents/data_counties/Data/Processed/"

#############
##  YRBS ###
#############

dt = fread(paste0(in.path,"table_a_m.csv"))
dt1 = fread(paste0(in.path,"table_n_z.csv"))
dt = rbind(dt,dt1)

x = dt[year >= 2010]
x[!is.na(q58), unique(sitename)]
x[!is.na(q58) & year == 2019, unique(sitename)]

#Add other states - more complicated cause variables not aligned - TBD
oh = fread(paste0(in.path, "OHH2019_YRBS_Data.csv"))
ma = fread(paste0(in.path, "MAH2019_YRBS_Data.csv"))

##Sex: 1 Female, 2, Male
oh = oh[,.(year = 2019, weight, stratum, PSU=psu, sitename = "Ohio", 
           age = q1, sex = q2, grade = q3,  
           race = raceeth, hadsex = q58, agesex = q59,numbersex = q60)]
oh[,race_new := 0]
oh[race %in% c(1:3), race_new := race]
oh[race == 4, race_new := 5] ##Native Hawaiin
oh[race == 5, race_new := 6] ##White
oh[race == 6, race_new := 4] ##Hispanic Latino
oh[race == 8, race_new := 7] ##Multiple-Non-Hispanic --> Multiple-Non-Hispanic 
oh[race == 7, race_new := 4] ##Multiple-Hispanic --> Hispanic Latino
oh[,race := race_new]
oh[,race_new := NULL]

ma = ma[,.(year = 2019, weight, stratum, PSU=psu, sitename = "Massachusetts", 
           age = q1, sex = q2, grade = q3,  
           race = raceeth, hadsex = q58, agesex = q59,numbersex = q60)]
ma[,race_new := 0]
ma[race %in% c(1:3), race_new := race]
ma[race == 4, race_new := 5] ##Native Hawaiin
ma[race == 5, race_new := 6] ##White
ma[race == 6, race_new := 4] ##Hispanic Latino
ma[race == 8, race_new := 7] ##Multiple-Non-Hispanic --> Multiple-Non-Hispanic 
ma[race == 7, race_new := 4] ##Multiple-Hispanic --> Hispanic Latino
ma[,race := race_new]
ma[,race_new := NULL]

dt_small = dt[,.(year,weight, stratum, PSU, sitename,
                 age, sex, grade, race = race7,hadsex =q58,agesex =q59,numbersex =q60)]
xx = strsplit(dt_small$sitename, ' [()]')
dt_small[,sitename := unlist(lapply(xx,'[[',1))]

dt1 = dt_small[year == 2019]
dt_all = rbind(dt1, oh, ma)
dt_all = dt_all[!is.na(age) & !is.na(sex) & !is.na(grade) & !is.na(race) & !is.na(hadsex)]
dt_all[,sex := ifelse(sex==1, "female", "male")]
dt_all[,race2 := "NEW"]
dt_all[race %in% c(1,2,5,7), race2 := "Other"]
dt_all[race %in% c(6), race2 := "White"]
dt_all[race %in% c(3), race2 := "Black or African American"]
dt_all[race %in% c(4), race2 := "Hispanic/Latino"]
dt_all[race == 0, race2 := NA]

yrbs = dt_all
yrbs[,survey := "yrbs"]

yrbs = yrbs[age >= 4]
yrbs[age == 4, age := 15]
yrbs[age == 5, age := 16]
yrbs[age == 6, age := 17]
yrbs[age == 7, age := 18]

yrbs[hadsex == 1, hadsex := 1]
yrbs[hadsex == 2, hadsex := 0]


saveRDS(yrbs,paste0(out.path,"YRBS_final.RDS" ))


##YRBS data availability
available = unique(dt_all$sitename)
since_2010 = dt[year >= 2010]
xx = strsplit(since_2010$sitename, ' [()]')
since_2010$sitename = unlist(lapply(xx,'[[',1))
has_yrbs = since_2010
since_2010 = since_2010[!is.na(q58), unique(sitename)]
since_2010 = since_2010[!since_2010 %in% unique(dt_all$sitename)]
has_yrbs = has_yrbs[,unique(sitename)]
has_yrbs = has_yrbs[!(has_yrbs %in% since_2010)]
has_yrbs = has_yrbs[!has_yrbs %in% unique(dt_all$sitename)]

dt_small_x = dt_small[sitename %in% since_2010 & year >= 2010 & year < 2019]
dt_small_x = dt_small_x[!is.na(age) & !is.na(sex) & !is.na(grade) & !is.na(race) & !is.na(hadsex)]
fwrite(dt_small_x,paste0(out.path,"yrbs_since_2010.csv" ))

spdf =  geojson_read("~/Documents/counties_project/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
spdf_fortified <- data.table(spdf_fortified)
spdf_fortified[,yrbs := "no YRBS since 2009"]
spdf_fortified[id %in% available,yrbs := "Sex q available for 2019"]
spdf_fortified[id %in% since_2010,yrbs := "Sex q at least once since 2010"]
spdf_fortified[id %in% has_yrbs, yrbs := "Has YRBS but no sex q"]
spdf_fortified[id %in% c("Indiana","District of Columbia" ), 
               yrbs := "Has YRBS but unvailable data"]


ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, fill = yrbs),  color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()



