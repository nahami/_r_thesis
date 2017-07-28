#SVF eindhoven
rm(list = ls())
graphics.off()

library(sp)
library(raster)



dir <- "/usr/people/ahami/Downloads/archiveEindhoven/SVFEindhoven"
setwd(dir)

filename <- "160000_383000.grd"
svf_1 <- raster(filename)
plot(svf_1)
summary(svf_1)
hist(svf_1$Z)
