

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
in.path = "~/Documents/counties_project/Data/Processed/"
out.path = "~/Documents/counties_project/Results/"

yrbs = readRDS(paste0(in.path,"YRBS_final.RDS"))
