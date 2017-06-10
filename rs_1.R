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
library(data.table)

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
# e =1
for (e in seq_along(fltudrdam)){
  l2keep <- 16500
  nL <- countLines(fltudrdam[e])
  data <- fread(fltudrdam[e], skip=nL-l2keep,stringsAsFactors = FALSE, na.strings = "NaN", blank.lines.skip = TRUE)
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
  
  data <- as.data.frame(data[,c(1,2,7)])
  colnames(data) <- c('datetime','id','temp')
  data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  data2 <- aggregate(list(temp = data$temp), by=list(datetime=cut(data$datetime,"hour")),mean)
  data2$datetime <- strptime(data2[,1], format = "%Y-%m-%d %H:%M:%S")
  data2$id <- data$id[1]
  data2 <- data2[,c(1,3,2)]

  
  # data <- cbind(datetime = as.date(datetime[,1]), id = data$id[1], temp = data2[,2]) 
  
  assign(paste('tud_',data2[1,2],sep = ''), data2)
  print(sprintf('imported %i files',e))
  rm(data,data2,l2keep,nL)
}
# e =1
#WOW Data
for (e in seq_along(datardam_wow)) {
  data <- as.data.frame(datardam_wow[e])
  st_id = as.numeric(gsub("\\D", "", colnames(data)[3]))
  data$datetime <- strptime(data$datum, format = "%Y-%m-%dT%H:%M:%S")
  data$id <- as.numeric(st_id)
  data$temp <- data[,2]
  
  data <- data[,c(7,8,9)]
  
  data2 <- aggregate(list(temp = data$temp), by=list(datetime=cut(data$datetime,"hour")),mean)
  data2$datetime <- strptime(data2[,1], format = "%Y-%m-%d %H:%M:%S")
  
  data2$id <- data$id[1]
  data2 <- data2[,c(1,3,2)]
  
  assign(paste('wow_',st_id,sep = ''),data2)
  rm(data,data2,st_id)
  # print(e)
}


# Get list of all dfs and combine into 1 df
dfs = sapply(.GlobalEnv, is.data.frame)
alld <- do.call(rbind, mget(names(dfs)[dfs]))
alld$datetime <-  as.character(alld$datetime)

#KNMI AWS 1
KNMI_AWS_1 <- fread(file = 'data/Rdam/KNMI_AWS_1/export_915096001.csv', sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-") 
KNMI_AWS_1$id <- as.numeric(gsub("\\D", "", colnames(KNMI_AWS_1)[2]))
KNMI_1 <- as.data.frame(strptime(KNMI_AWS_1$datum, format = "%Y-%m-%dT%H:%M:%S"))
KNMI_1 <- cbind(KNMI_1, KNMI_AWS_1[,c(7,2,3,4,6)])
colnames(KNMI_1) <- c('datetime','id','temp_aws','ws','wd','pr')
KNMI_1$datetime <- as.character(KNMI_1$datetime) 

#KNMI AWS 2
KNMI_AWS_2 <- fread(file = 'data/Rdam/KNMI_AWS_2/export_920316001.csv', sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-") 
KNMI_AWS_2$id <- as.numeric(gsub("\\D", "", colnames(KNMI_AWS_2)[2]))
KNMI_2 <- as.data.frame(strptime(KNMI_AWS_2$datum, format = "%Y-%m-%dT%H:%M:%S"))
KNMI_2 <- cbind(KNMI_2, KNMI_AWS_2[,c(7,2,3,4,6)])
colnames(KNMI_2) <- c('datetime','id','temp_aws','ws','wd','pr')
KNMI_2$datetime <- as.character(KNMI_2$datetime) 

rm(e,dir,fltudrdam,flwowrdam,datardam_wow, KNMI_AWS_1, KNMI_AWS_2)



# save df 
save(alld,KNMI_1,KNMI_2,file = 'obs.RData')

