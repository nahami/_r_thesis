# Read data
st_a <- read.table(file = "~/mscthesis/_A1/a_export_916696001.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_a","Windsp_a","Winddi_a","RH_a","Pres_a"), 
                   na.strings = "-")
st_a$Datetime <- strptime(st_a$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")

st_b <- read.table(file = "~/mscthesis/_A1/b_export_924276001.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_b","Windsp_b","Winddi_b","RH_b","Pres_b"), 
                   na.strings = "-")
st_b$Datetime <- strptime(st_b$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")

st_c <- read.table(file = "~/mscthesis/_A1/c_export_926656002.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_c","Windsp_c","Winddi_c","RH_c","Pres_c"), 
                   na.strings = "-")
st_c$Datetime <- strptime(st_c$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")

st_d <- read.table(file = "~/mscthesis/_A1/d_export_932806001.csv",
                     header = T,
                     sep = ";",
                     col.names = c("Datetime","Temperature_d","Windsp_d","Winddi_d","RH_d","Pres_d"), 
                     na.strings = "-")
st_d$Datetime <- strptime(st_d$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")

st_e <- read.table(file = "~/mscthesis/_A1/e_export_933736001.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_e","Windsp_e","Winddi_e","RH_e","Pres_e"), 
                   na.strings = "-")
st_e$Datetime <- strptime(st_e$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")


st_f <- read.table(file = "~/mscthesis/_A1/f_export_942486001.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_f","Windsp_f","Winddi_f","RH_f","Pres_f"), 
                   na.strings = "-")
st_f$Datetime <- strptime(st_f$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")

st_g <- read.table(file = "~/mscthesis/_A1/g_export_1000000112.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_g","Windsp_g","Winddi_g","RH_g","Pres_g"), 
                   na.strings = "-")
st_g$Datetime <- strptime(st_g$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")


st_h <- read.table(file = "~/mscthesis/_A1/h_export_1000000779.csv",
                   header = T,
                   sep = ";",
                   col.names = c("Datetime","Temperature_h","Windsp_h","Winddi_h","RH_h","Pres_h"), 
                   na.strings = "-")
st_h$Datetime <- strptime(st_h$Datetime, format = "%Y-%m-%dT%H:%M:%S",tz = "GMT+1")




st_all <- cbind(st_a, st_b[,2:5], st_c[,2:5], st_d[,2:5], st_e[,2:5], st_f[,2:5], st_g[,2:5])


st_all_r <- na.omit(st_all)


library(oce)
day0="2015-02-16"
dayn = "2017-03-01"

t0 <- as.POSIXct(paste(day0, "00:00:00"), tz="UTC")
tn <- as.POSIXct(paste(dayn, "23:50:00"), tz="UTC")
t <- seq(from=t0, to=tn, by="10 min")
sunel = sunAngle(t, 53, 3)
sunel$altitude[1:100]
st_all_sun = merge(st_all, sunel$altitude,by = st_all$Datetime)