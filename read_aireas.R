rm(list = ls())
graphics.off()

# Load libary
library(data.table)

st <- as.Date("2017-01-01")
ed <- Sys.Date()-1
st2 <- as.Date("2016-01-01")
ed2 <- as.Date("2016-12-31")
ll <- seq(st, ed, by = "day")
ll2 <- seq(st2, ed2-45, by = "day")
ll2 <- ll2[-c(291,292,318)] #missing data
url <- paste0('http://data.aireas.com/csv/measurements-',ll,'.csv')
url2 <- paste0('http://data.aireas.com/csv/2016/measurements-',ll2,'.csv')

# Pre-allocate output
data_out <- as.data.frame(NULL)
# Combine daily data into 1 df
for (i in 1:length(ll)) {
  line_skip <- sapply(url[i],
                      function(x) grep("MEASUREMENTS", readLines(x, n = 100)))
  data <- fread(url[i], header = TRUE, sep = ';', stringsAsFactors = FALSE, skip = line_skip, blank.lines.skip = TRUE)
  data <- data[,c(1,2,3,12,16)]
  data_out <- rbind(data_out,data)
}
data_2017 <- data_out
rm(data, data_out)

# Pre-allocate output
data_out <- as.data.frame(NULL)
for (i in 1:length(ll2)) {
  line_skip <- sapply(url2[i],
                      function(x) grep("MEASUREMENTS", readLines(x, n = 100)))
  data <- fread(url2[i], header = TRUE, sep = ';', stringsAsFactors = FALSE, skip = line_skip, blank.lines.skip = TRUE)
  data <- data[,c(1,2,3,12,15)]
  data_out <- rbind(data_out,data) #gives error, try setting up using data.table::rbindlist
}
data_2016 <- data_out

save(data_2016,data_2017, file = 'aireasdata.RData')
