
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

#Filepaths
in.path = "~/Documents/county_project/Data/Raw/"
out.path = "~/Documents/county_project/Data/Processed/"
  
############
##  NSFG ###
############
female = read.dta13(paste0(in.path,"NSFG_female.dta"))
female = as.data.table(female)
male = read.dta13(paste0(in.path,"NSFG_male.dta"))
male = as.data.table(male)
female = female[,.(AGE_R,hadsex, sexonce,sexever, VRY1STAG, SEX1AGE ,higrade ,PRIMLANG1,hisp ,race,
                   hhpartyp, AGEMOMB1 ,PARTS1YR ,lsexrage ,ANYBC12 ,metro ,religion, poverty ,totincr ,
                   TALKPAR1,sedno,knowfp,sexafmen,currpreg, roscnt)]

male = male[,.(AGE_R,hadsex, sexonce,eversex, VRY1STAG, higrade ,PRIMLANG1,educat ,hisp ,
               race,hhpartyp, AGEMOMB1 ,PARTS1YR ,lsexrage ,metro ,religion, poverty ,totincr ,TALKPAR1,
               sedno, roscnt)]
male[,gender := "male"]
female[,gender := "female"]
setnames(male, "eversex","sexever")
female[,sexever := ifelse(grepl("YES",sexever),"Yes", "No")]
nsfg = rbind(male,female, fill=TRUE, use.names = TRUE)
nsfg = nsfg[AGE_R <= 19]
nsfg = nsfg[!grepl("COLLEGE",educat)]
nsfg = nsfg[!is.na(nsfg$educat)]
nsfg[,sexever := droplevels.factor(sexever)]
fwrite(nsfg,paste0(out.path,"NSFG_final.csv"))

############
##  ACS ###
############

acs = fread(paste0(in.path,"ACS_2019.csv"))
acs = acs[AGE <= 19]
acs = acs[HHINCOME == 9999999, HHINCOME := NA]
acs[,mom_birth_age := AGE_MOM - AGE]

#Align household income variable
acs[, totincr := cut(HHINCOME, 
                    c(-13000,5000, 7499, 9999, 12499,
                      14999,19999,24999,29999,34999,39999,
                      49999,59999,74999,99999,
                      max(HHINCOME)), include.lowest = FALSE)]
levels(acs$totincr) <- as.character(unique(sort(nsfg$totincr)))

##Align Race variables
acs[,race := ifelse(RACE  == 1, "white","other")]
acs[RACE == 2, race := "black"]
levels(acs$race) = c("black","white","other")

acs[HISPAN %in% c(2:4), hisp := "Yes"]
acs[HISPAN == 1, hisp := "No"]
acs[HISPAN == 9, hisp := NA]

##Age of mother at birth
acs = acs[!mom_birth_age<= 14]
acs = acs[!mom_birth_age >= 50]
table(acs$mom_birth_age)
table(nsfg$AGEMOMB1)
acs[,AGEMOMB1 := cut(mom_birth_age, 
                    c(0,17,19,24,29,
                    max(mom_birth_age)))]

levels(acs$AGEMOMB1) <- sort(unique(levels(nsfg$AGEMOMB1)))

##Align Metro variable
#https://usa.ipums.org/usa-action/variables/METRO#codes_section 
table(acs$METRO)
table(nsfg$metro)
acs[METRO == 1, metro := "NOT MSA"]
acs[METRO == 2, metro := "PRINCIPAL CITY OF MSA"]
acs[METRO %in% c(3:4), metro := "OTHER MSA"]
acs[METRO  == 0, metro := "NOT MSA"]
table(acs$METRO,acs$metro)

#Alternative classification because status 0 is 'mixed' metropolitan status
acs[,metro_alt := metro]
acs[METRO  == 0, metro_alt := "OTHER MSA"]

##Number of family members in the household
table(acs$FAMSIZE)
table(nsfg$roscnt)
acs[,nhhmembers := ifelse(FAMSIZE >= 8, 8, FAMSIZE)]
nsfg[,nhhmembers := substring(roscnt,1,1)]
table(acs$FAMSIZE, acs$nhhmembers)
table(nsfg$nhhmembers, nsfg$roscnt)

##Primary language spoken at home
table(nsfg$PRIMLANG1)
table(acs$LANGUAGE)
acs[LANGUAGE == 1, PRIMLANG1 := "English"]
acs[LANGUAGE == 12, PRIMLANG1 := "Spanish"]
acs[!LANGUAGE %in% c(1,12), PRIMLANG1 := "Other"]
table(acs$LANGUAGE, acs$PRIMLANG1)

##Grade
table(acs$GRADEATTD)
acs[, grade := GRADEATTD - 42]
table(acs$grade)
levels(acs$grade) = c(9,10,11,12)

#Documentation says to use educat variable 
#https://www.cdc.gov/nchs/data/nsfg/2017-2019_NSFG_FemResp_SectionA_Codebook-508.pdf 
nsfg[,grade := substring(educat,1,2)]
nsfg[grade == "9T", grade := 9]
levels(nsfg$grade) = c(9,10,11,12)

colnames(nsfg)
colnames(acs)
acs = acs[,.(year = YEAR, strata = STRATA, cluster = CLUSTER, state = STATEICP, 
       county = COUNTYICP, metro_orig = METRO, hhincome_orig = HHINCOME, 
       weight = PERWT, nhhmembers_orig = FAMSIZE, sex = SEX, age = AGE, 
       race_orig = RACE, hisp_orig = HISPAN,
       language_orig = LANGUAGE, grade, agemomb_orig = mom_birth_age, 
       hhincome = totincr, race, hisp, agemomb = AGEMOMB1,
       metro, metro_alt, nhhmembers, language = PRIMLANG1, grade)]
nsfg = nsfg[,.(year = 2019, age = AGE_R, hadsex, 
               sexonce, sexever, language = PRIMLANG1, hhincome = totincr,
               agemomb = AGEMOMB1, nhhmembers, metro,grade,
               nhhmembers_orig = roscnt)] 
nsfg[,survey := "nsfg"]
acs[,survey := "acs"]

#############
##  YRBS ###
#############

dt = fread("~/Documents/county_project/table_a_m.csv")
dt[!is.na(sex)]
nrow(dt)
unique(dt[!is.na(sex)]$record)
dt[sitename == "Delaware (DE)"]

dt = rbind(dt,fread("~/Documents/county_project/table_n_z.csv"))

#Add other states - more complicated cause variables not aligned
ct = fread(paste0(in.path, "CT2019_YRBS_Data.csv"))
oh = fread(paste0(in.path, "OHH2019_YRBS_Data.csv"))
ma = fread(paste0(in.path, "MAH2019_YRBS_Data.csv"))

dt_small = dt[,.(year,survyear, weight, stratum, PSU, sitename,
           age, sex, grade, race4,q19,q20,q21,q58,q59,q60,q61,q65)]
dt1 = dt_small[year == 2019]
no_2019 = unique(dt$sitename)[!(unique(dt$sitename) %in% unique(dt1$sitename))]

dt1 = dt_small[!is.na(sex)]
length(unique(dt_small$sitename))
nrow(dt_small)

dt1 = dt_small[!is.na(q58)]
dt1 = dt1[year == 2019,unique(sitename)]
no_sexq = unique(dt_small$sitename)[!(unique(dt_small$sitename) %in% dt1)]
no_sexq = unlist(strsplit(no_sexq ," \\("))[seq(1,19,by=2)]
no_sexq = no_sexq[-4]
no_sexq = no_sexq[-8]

length(unique(dt_small$sitename))
nrow(dt_small)
dt_small = dt_small[!is.na(sex)]
length(unique(dt_small$sitename))
nrow(dt_small)
dt_small = dt_small[!is.na(age) & !is.na(grade) & !is.na(race4)]
length(unique(dt_small$sitename))
nrow(dt_small)

yrbs = dt_small
yrbs[,survey := "yrbs"]

all_dat = list(nsfg = nsfg, acs = acs, yrbs = yrbs)
saveRDS(all_dat,paste0(out.path,"all_surveys.RDS" ))



##YRBS data availability
spdf =  geojson_read("~/Documents/county_project/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
spdf_fortified <- data.table(spdf_fortified)
spdf_fortified[,yrbs := NA]
spdf_fortified[id %in% unlist(strsplit(unique(dt_small$sitename) ," \\("))[seq(1,88,by=2)],yrbs := "Available"]
spdf_fortified[id %in% c("Wyoming","Delaware",
                         "Washington","Oregon","Minnesota",
                         "Indiana", "District of Columbia"),yrbs := "No 2019 survey or No rep. results"]
spdf_fortified[id %in% no_sexq,yrbs := "No sex q in 2019"]
spdf_fortified[id %in% c("Massachusetts","Ohio"),
               yrbs := "Available, need to check sex q"]
ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, fill = yrbs),  color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()



