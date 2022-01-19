
##This script processes the raw NSFG,ACS and YRBS data, downloadded from public use files
##It also recodes ACS variables to match NSFG

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
library(covidcast)


#Filepaths
in.path = "~/Documents/data_counties/Data/Raw/"
out.path = "~/Documents/data_counties/Data/Processed/"

############
##  ACS ###
############
acs = fread(paste0(in.path,"usa_00007.csv"))

#acs = fread(paste0(in.path,"acs_income_only.csv"))
acs = acs[AGE <= 19 & AGE >= 15]
acs = acs[HHINCOME == 9999999, HHINCOME := NA]
#acs = acs[FTOTINC == 9999999, FTOTINC := NA]
acs$age = factor(acs$age, levels = c(15,16,17,18,19))

acs = acs[!is.na(HHINCOME) &
            !is.na(AGE_MOM) &
            !is.na(AGE) &
            !is.na(METRO) &
            !is.na(RACED) &
            !is.na(HISPAN) &
            !is.na(LANGUAGE) &
            !is.na(GRADEATTD) &
            !is.na(FAMSIZE)]

acs[,age := factor(AGE, levels = c(15,16,17,18,19))]

acs[,mom_birth_age := AGE_MOM - AGE]
acs[,mom_age_cat := "missing"]
acs[mom_birth_age <= 24, mom_age_cat := "24 or younger"]
acs[mom_birth_age >= 25, mom_age_cat := "25 or older"]


#Align household income variable
acs[, totincr := cut(HHINCOME, 
                     c(-13000,5000, 7499, 9999, 12499,
                       14999,19999,24999,29999,34999,39999,
                       49999,59999,74999,99999,
                       5000000), include.lowest = FALSE)]
acs[,household_cat := "Under $25000"]
acs[HHINCOME >=25000 & HHINCOME < 600000, household_cat := "$25000-$59,999"]
acs[HHINCOME >= 600000, household_cat := "$60,000 or more"]

##Align sex variable
acs[,SEX := ifelse(SEX == 1, "male","female")]

##Align Race variables
acs[,race := ifelse(RACE  == 1, "white","other")]
acs[RACE == 2, race := "black"]
levels(acs$race) = c("black","white","other")

acs[,ethnicity := "yes"]
acs[HISPAN %in% c(2:4), ethnicity := 1]
acs[HISPAN == 1, ethnicity := 0]
acs[,ethnicity := factor(ethnicity, levels = c(0,1), labels = c("non_hisp","hisp"))]

##Align Metro variable
#https://usa.ipums.org/usa-action/variables/METRO#codes_section 
acs[,metro := "missing"]
acs[METRO == 1, metro := "not msa"]
acs[METRO == 2, metro := "principal city of msa"]
acs[METRO %in% c(3:4), metro := "other msa"]
acs[METRO  == 0, metro := "not msa"]
levels(acs$metro) <- c("not msa","other msa", "principal city of msa")

#Alternative classification because status 0 is 'mixed' metropolitan status
acs[,metro_alt := metro]
acs[METRO  == 0, metro_alt := "other msa"]

##Number of family members in the household
acs[,nhhmembers := ifelse(FAMSIZE >= 8, 8, FAMSIZE)]
acs[,nhhmembers_cat := "1-4"]
acs[nhhmembers <= 4,nhhmembers_cat := "1-4" ]
acs[nhhmembers >= 5,nhhmembers_cat := "5 or more" ]
acs[,nhhmembers_cat := factor(nhhmembers_cat, 
                              levels = c("1-4","5 or more"), 
                              labels = c("1-4","5 or more"))]

##Grade
acs[, grade := GRADEATTD - 42]
levels(acs$grade) = c(9,10,11,12)

acs = acs[,.(year = 2019, year_cat = "2015_2019", age = AGE, race, 
             ethnicity, sex=SEX,grade,  household_cat,
             nhhmembers_cat, metro, mom_age_cat, strata = STRATA, 
             cluster = CLUSTER, state_code = STATEFIP, 
             countyICP = COUNTYICP, PUMA, weight = PERWT)]
acs[,survey := "acs"]


##Merge counties
puma_map = fread(paste0(in.path,"geocorr2018.csv")) ##File given by Marissa Reitsema
county_census = data.table(county_census)
setnames(county_census, c("FIPS","POPESTIMATE2019"),c("county_fips","pop2019"))
county_census[,county_fips := as.numeric(county_fips)]
puma_map = merge(puma_map, county_census[,.(county_fips, pop2019)])
acs <- merge(acs, puma_map[,.(cntyname, county_fips, PUMA, state_code, state, pop10,pop2019)], by = c("PUMA", "state_code"), allow.cartesian = T)
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
acs[,county_fips := as.character(county_fips)]
acs[nchar(county_fips) < 5, county_fips := paste0("0",county_fips)]
acs[,state_code := as.character(state_code)]
acs[nchar(state_code) < 2, state_code := paste0("0",state_code),state_code]

saveRDS(acs, paste0(out.path,"ACS_final.RDS"))











