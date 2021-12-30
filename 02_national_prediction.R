

rm(list=ls())
set.seed(1234)

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

  
nsfg = readRDS(paste0(in.path,"NSFG_final.RDS"))
nsfg[,hadsex1 := as.numeric(as.character(hadsex))]
reserve_sample = sample(nrow(nsfg), nrow(nsfg)*0.1)
nsfg[,year := factor(year, levels = c(2011,2015,2017,2019))]
nsfg[,year_cat := "missing"]
nsfg[year %in% c(2011,2015), year_cat := "2011_2015"]
nsfg[year %in% c(2017,2019), year_cat := "2015_2019"]
nsfg_restrict = nsfg[!reserve_sample]


get_predictions = function(dt,use_logit = FALSE, threshold,...){

  vars = c(...)
  vars = paste(vars,collapse="+")
  
  if(use_logit){
    print("using logit")
    form = as.formula(paste0("hadsex ~ ",vars))
    #mysvyglm <- glm(form,family=binomial(link="logit"), data = dt )
    mysvyglm <- svyglm(form,
                       design = ff,
                       family=quasibinomial(link="logit"))
  } else {
    form = as.formula(paste0("hadsex1 ~ ",vars))
    mysvyglm <- svyglm(form, design = ff)
  }
  model = mysvyglm
  aic  = AIC(mysvyglm)
  results = exp(cbind(coefficients(mysvyglm),confint(mysvyglm)))

  ll = list(model = model,
            aic = aic,
            results = results)
  
  return(ll)
}

vars = c("age", "year_cat","ethnicity","household_cat","metro","mom_age_cat","nhhmembers_cat","race")
dt = copy(nsfg_restrict)[sex == "female"]
dt$fpc = 64 ##Random number

ff <-
  svydesign(
    id = ~cluster ,
    strata = ~strata ,
    data = dt ,
    weights = ~weight ,
    fpc = ~fpc,
    nest = TRUE
  )


m1 = get_predictions(dt,use_logit = TRUE,  threshold = threshold, vars )
saveRDS(m1,paste0(out.path, "171221_logit_model.RDS"))

##Random Forest - testing
set.seed(222)
dt = dt[,c(vars, "hadsex"),with=FALSE]
data = dt[complete.cases(dt)]
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(hadsex~., data=train, proximity=TRUE)
rf1 = predict(rf,train)
confusionMatrix(rf1, train$hadsex)
rf1 = predict(rf,test)
confusionMatrix(rf1, test$hadsex)

varImpPlot(rf,
           sort = T,
           n.var = 5,
           main = "Top 5 - Variable Importance")





