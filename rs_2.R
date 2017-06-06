#------------------
# Thesis: Statistical modeling of the Urban Heat Island
# May 2017, by Nadir Ahami
# 

#### Set up environment ####
rm(list=ls())

# Set working directory
# dir <- "C:/Users/Nadir/hubiC/_Studie/MscThesis/_r_thesis"
dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
setwd(dir)

# packages
# library(R.utils)

#### Import data ####
load("obs.RData")
ancdat <- read.csv('data/Rdam/rdam_all', header = TRUE)

#### Merge df ####
knmi_aws <- KNMI_AWS[,2:6]




