# Thesis: Statistical modeling of the Urban Heat Island ----
# May 2017, by Nadir Ahami
# ...Explain script/project...

# Set up environment --------------------------
rm(list=ls())

# Set working directory
# dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
dir <- "C:/Users/Nadir/hubiC/_Studie/MscThesis/_r_thesis"
setwd(dir)

# packages
library(R.utils)


# Import data ---------------------------------
# load wow and TUD data coordinates and IDs
idlatlon <- read.csv('data/Rdam/rdam_all', header = FALSE)
colnames(idlatlon) <- c('Name', 'id', 'lat', 'lon', 'owner')

flwowrdam <- paste(dir,'/data/Rdam/WOW/',list.files('data/Rdam/WOW'), sep = '')
fltudrdam <- paste(dir,'/data/Rdam/TUDelft/',list.files('data/Rdam/TUDelft',pattern = '.csv'), sep = '')

datardam_wow <- lapply(X = flwowrdam,read.csv,sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-")
# datardam_tud <- lapply(X = fltudrdam,read.csv)

for (e in seq_along(fltudrdam)){
  l2keep <- 5000
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
  
  assign(list.files('data/Rdam/TUDelft', pattern = '.csv')[e], data)
  print(sprintf('imported %i files',e))
}

rm(data, datardam_tud)


for (e in seq_along(datardam_wow)) {
  data <- as.data.frame(datardam_wow[e])
  st_id = as.numeric(gsub("\\D", "", colnames(data)[3]))
  assign(paste('wow',st_id,sep = ''),data)
  # print(e)
}
