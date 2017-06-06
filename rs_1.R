#------------------
# Thesis: Statistical modeling of the Urban Heat Island
# May 2017, by Nadir Ahami
# 

#### Set up environment ####
rm(list=ls())

# Set working directory
# dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
# dir <- "C:/Users/Nadir/hubiC/_Studie/MscThesis/_r_thesis"
dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
setwd(dir)

# packages
library(R.utils)

#### Import data ####
# load wow and TUD data coordinates and IDs
idlatlon <- read.csv('data/Rdam/rdam_all', header = FALSE)
colnames(idlatlon) <- c('Name', 'id', 'lat', 'lon', 'owner')
rm(idlatlon)

flwowrdam <- paste(dir,'/data/Rdam/WOW/',list.files('data/Rdam/WOW'), sep = '')
fltudrdam <- paste(dir,'/data/Rdam/TUDelft/',list.files('data/Rdam/TUDelft',pattern = '.csv'), sep = '')

datardam_wow <- lapply(X = flwowrdam,read.csv,sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-")

#### Formatting and subselection data ####
#TUD Data
for (e in seq_along(fltudrdam)){
  l2keep <- 16500
  nL <- countLines(fltudrdam[e])
  data <- read.csv(fltudrdam[e], skip=nL-l2keep,stringsAsFactors = FALSE, na.strings = "NaN")
  colnames(data) <- c( 'DateTime',
                       'Name',
                       'Battery_{Min}',
                       'LithiumBattery_{Min}',
                       'PanelTemp_{Max}',
                       'CS215Error_{Tot}',
                       'Tair_{Avg}',
                       'RH_{Avg}',
                       'e_{Avg}',
                       'e_{s, Avg}',
                       'WindSonicError_{Tot}',
                       'WindSpd_{Max}',
                       'WindSpd_{Std}',
                       'WindSpd_{Avg}',
                       'WindDir_{Avg}',
                       'WindDir_{Std}',
                       'WindDirError_{Tot}',
                       'Rain_{Tot}',
                       'SR01Up_{Avg}',
                       'SR01Dn_{Avg}',
                       'IR01Up_{Avg}',
                       'IR01Dn_{Avg}',
                       'Tir01_{Avg}',
                       'Tglobe_{Avg}',
                       'Mir01',
                       'T_{sky}',
                       'T_{surface}',
                       'NetRs',
                       'NetRl',
                       'NetR',
                       'TotUp',
                       'TotDn',
                       'Albedo'
  )
  
  data <- as.data.frame(cbind(id = data$'Name', datetime = data$'DateTime', temp = as.double(data$'Tair_{Avg}')))
  data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")
  assign(paste('tud_',data[1,1],sep = ''), data)
  print(sprintf('imported %i files',e))
  rm(data,l2keep,nL)
}

#WOW Data
for (e in seq_along(datardam_wow)) {
  data <- as.data.frame(datardam_wow[e])
  data <- cbind(id = as.numeric(gsub("\\D", "", colnames(data)[2])),data)
  st_id = as.numeric(gsub("\\D", "", colnames(data)[3]))
  data <- as.data.frame(cbind(id = data[,1], datetime = data[,2], temp = data[,3]))
  data$datetime <- strptime(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  assign(paste('wow_',st_id,sep = ''),data)
  rm(data,st_id)
  # print(e)
}

# Get list of all dfs and combine into 1 df
dfs = sapply(.GlobalEnv, is.data.frame)
alld <- do.call(rbind, mget(names(dfs)[dfs]))

#KNMI AWS 
KNMI_AWS <- read.csv(file = 'data/Rdam/KNMI_AWS/export_915096001.csv', sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-") 
KNMI_AWS <- cbind(id = as.numeric(gsub("\\D", "", colnames(KNMI_AWS)[2])),KNMI_AWS)
KNMI_AWS <- as.data.frame(cbind(id = KNMI_AWS[,1], datetime = KNMI_AWS[,2], temp_aws = KNMI_AWS[,3], ws = KNMI_AWS[,4], wd = KNMI_AWS[,5], pr = KNMI_AWS[,7]))
KNMI_AWS$datetime <- strptime(KNMI_AWS$datetime, format = "%Y-%m-%dT%H:%M:%S")

rm(e,dir,fltudrdam,flwowrdam,datardam_wow)



# save df 
save(alld,KNMI_AWS,file = 'obs.RData')

