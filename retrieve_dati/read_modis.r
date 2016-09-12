library(raster)
library(leaflet)
library(htmlwidgets)
library(mapview)
library(OpenStreetMap)
library(rasterVis)
library(ggplot2)
library(rgdal)
library(ggmap)
library(grid)
library(latticeExtra)
library(maptools)
library(webshot)

setwd("/home/alf/Scrivania/lav_caldo/modis")


source("aux_map_function.r")


modis_day_LST_terra=list.files("dataMODIS","*Day_1km",full.names = T)
modis_day_QC_terra=list.files("dataMODIS","*QC_Day",full.names = T)

modis_night_terra=list.files("dataMODIS","*ght_1km",full.names = T)
modis_nightQC_terra=list.files("dataMODIS","*QC_Ni",full.names = T)

resLST=lapply(modis_day_LST_terra,function(x) {temp=raster(x);temp[temp==0]<-NA;temp=temp*0.02-273.16 ;temp=crop(temp, extent( 5, 20, 35, 50));return(temp)} )
resLST=stack(resLST)
saveRDS(resLST,"resLST.rds")

system("cdo seldate,2015-05-15,2015-09-15 redlav_tmax.nc redlav_tmax_period.nc")
system("cdo seldate,2015-05-15,2015-09-15 redlav_min.nc redlav_tmin_period.nc")

period_date=seq(as.Date("2015-05-15"),as.Date("2015-09-15"))

ita_tmax=brick("redlav_tmax_period.nc")

mapview(ita_tmax[[1]],col.regions =heat.colors(100))

###########################################################################################################
# reference

# http://journocode.com/2016/01/28/your-first-choropleth-map/
# http://gis.stackexchange.com/questions/173984/how-do-i-plot-points-as-graduated-proportional-circles-in-r

