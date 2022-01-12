
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
library(weights)

#Filepaths
in.path = "~/Documents/data_counties/Data/Processed/"
nsfg = readRDS(paste0(in.path,"NSFG_final.RDS"))
# acs = dat$acs 
# nsfg = dat$nsfg
# yrbs = dat$yrbs

female = nsfg[sex == "female"]
table(female$year, female$hadsex, female$age)


options("survey.lonely.psu")
s = svydesign(ids = ~0, weights = ~PERWT, data = dt)
t <- svymean(~mom_birth_age,s)
mean(dt$mom_birth_age)
weighted.mean(acs$hhincome_orig,  acs$weight)
weighted.mean(acs$hhincome_orig,  acs$hhweight)
mean(acs$hhincome_orig)
median(acs$hhincome_orig)

svyby(~mom_birth_age, ~RACE, s, svymean)
svyby(~mom_birth_age, ~RACE, s, svymean)
svyby(~mom_birth_age, ~RACE + AGE, s, unwtd.count)

pct = data.table(pct = c(wpct(acs$hhincome, weight=acs$weight),wpct(acs$hhincome)),
                 hhincome = names(wpct(acs$hhincome)),
                 type = c(rep("weighted",length(wpct(acs$hhincome, weight=acs$weight))),
                          rep("unweighted",length(wpct(acs$hhincome)))))
pct[,hhincome := factor(hhincome, levels = names(wpct(acs$hhincome)))]
ggplot(pct, aes(hhincome, pct, fill=type)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
colnames(acs)
var = "age"
acs1 = wpct(acs[,get(var)], weight=acs$weight)
nsfg1 =  wpct(nsfg[,get(var)], weight=nsfg$weight)
pct = data.table(pct = c(acs1,nsfg1),
                 var = c(names(acs1),names(nsfg1)),
                 source = c(rep("ACS",length(acs1)),
                          rep("NSFG",length(nsfg1))))
ggplot(pct, aes(var, pct, fill=source)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


prop.table(table(acs$agemomb)) ##NEED TO CHECK THIS VARIABLE
prop.table(table(nsfg$race))

prop.table(tapply(acs$weight, list(acs$race, acs$hhincome), sum), 2)
prop.table(tapply(nsfg$weight, list(nsfg$race, nsfg$hhincome), sum), 2)


#YRBS
##Imputation
yrbs_impute1 = fread(paste0(in.path,"yrbs_since_2010.csv" ))
yrbs_sum = data.table(expand.grid(state = unique(yrbs_impute1$sitename),
                      no = 1,
                      yes = 1,
                      year = unique(yrbs_impute1$year),
                      sex  = unique(yrbs_impute1$sex),
                      grade = unique(yrbs_impute1$grade)))

for(s in unique(yrbs_impute1$sitename)){
  for(y in unique(yrbs_impute1$year)){
    for(f in unique(yrbs_impute1$sex)){
      for(g in unique(yrbs_impute1$grade)){
    
  ss = yrbs_impute1[sitename == s & year == y & sex == f & grade == g]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s & year == y & sex == f & grade == g, yes := as.numeric(props[1])]
  yrbs_sum[state == s & year == y & sex == f & grade == g, no := as.numeric(props[2])]
      }
    }
  }
}

national = data.table(state = "national", 
                      year = c(2011,2013,2015,2017),
                      value = c(0.47,0.47,0.4,0.39), 
                      variable = "yes")
yrbs_sum = melt(yrbs_sum[,.(state,yes,year,grade,sex)], 
                id.var = c("state","year","grade","sex"))
yrbs_sum = rbind(yrbs_sum,national)
yrbs_sum[,grade := factor(grade, levels = c(1,2,3,4), labels = c(9,10,11,12))]
yrbs_sum[,sex := factor(sex, levels = c(1,2), labels = c("male","female"))]

ggplot(yrbs_sum[state == "Delaware"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Hawaii"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "New Jersey"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Tennessee"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Wyoming"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "New Mexico"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)


yrbs_sum = data.table(state = unique(yrbs$sitename),
                      no = 1,
                      yes = 1)
for(s in unique(yrbs$sitename)){
  print(s)
  ss = yrbs[sitename == s]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s, yes := as.numeric(props[1])]
  yrbs_sum[state == s, no := as.numeric(props[2])]
}
yrbs_sum = melt(yrbs_sum[,.(state,yes)], id.var = "state")

median(yrbs_sum$value,na.rm=TRUE)
range(yrbs_sum$value,na.rm=TRUE)

spdf =  geojson_read("~/Documents/county_project/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
spdf_fortified <- data.table(spdf_fortified)
spdf_fortified[,yrbs := NA]
spdf_fortified = merge(spdf_fortified, yrbs_sum, by.x = "id", by.y = "state",all.x = TRUE)
spdf_fortified[,value := as.numeric(value)]

ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, fill = value),  color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top")

##By grade
var = "hadsex"
yrbs_sum = data.table(expand.grid(state = unique(yrbs$sitename),
                      no = 1,
                      yes = 1,
                      grade = unique(yrbs$grade)))
for(s in unique(yrbs$sitename)){
  for(j in unique(yrbs$grade)){
  ss = yrbs[sitename == s & grade == j]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s & grade == j, yes := as.numeric(props[1])]
  yrbs_sum[state == s & grade == j, no := as.numeric(props[2])]
  }
}

yrbs_sum = melt(yrbs_sum[,.(state,yes,grade)], id.var = c("state","grade"))
yrbs_sum[,grade := factor(grade, levels = c(1,2,3,4,5), labels = c(9,10,11,12,0))]
yrbs_sum = yrbs_sum[grade != 0]

ggplot(yrbs_sum, aes(state, grade, fill = value)) + geom_tile() + coord_flip() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top")

##By race
var = "hadsex"
yrbs_sum = data.table(expand.grid(state = unique(yrbs$sitename),
                                  no = 1,
                                  yes = 1,
                                  race2 = unique(yrbs$race2)))
for(s in unique(yrbs$sitename)){
  for(j in unique(yrbs$race2)){
    ss = yrbs[sitename == s & race2 == j]
    props = wpct(ss[,get(var)], weight=ss$weight)
    yrbs_sum[state == s & race2 == j, yes := as.numeric(props[1])]
    yrbs_sum[state == s & race2 == j, no := as.numeric(props[2])]
  }
}

yrbs_sum = melt(yrbs_sum[,.(state,yes,race2)], id.var = c("state","race2"))
yrbs_sum[,race := factor(race, levels = c(1,2,3,4,5,6,7), 
                          labels = c("American Indian/Alaska Native",
                                      "Asian","Black or African American",
                                     "Hispanic/Latino","Native Hawaiian/Other PI",
                                     "White","Multiple (Non Hispanic)"))]
ggplot(yrbs_sum[!is.na(race2)], aes(state, race2, fill = value)) + geom_tile() + coord_flip() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1))


yrbs[race == 2 & sitename == "Idaho"]
yrbs[race == 2 & sitename == "Mississippi"]




