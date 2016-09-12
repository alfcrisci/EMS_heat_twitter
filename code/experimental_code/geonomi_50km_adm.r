library(raster)
library(sp)
library(WikidataR)
library(rgdal)
library(rgeos)
library(SPARQL)
library(sp)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(mapview)

setwd("/home/alf/Scrivania/lav_caldo")

selected_point_station=readRDS("selected_point_station.rds")
proj4string(selected_point_station)=CRS( "+init=epsg:4326")
selected.projected <- spTransform(selected_point_station, CRS( "+init=epsg:32632" )) #utm32n
selected.circles.projected <- gBuffer(selected.projected, width=20000, byid=TRUE)
selected.circles<- spTransform(selected.circles.projected , CRS( "+init=epsg:4326" )) #geolatlon
saveRDS(selected.circles,"selected.circles_20km.rds")



selected.circles=readRDS("selected.circles_20km.rds")
CMProv2015_WGS84=readRDS("comprovITA_epgs_32632.rds")
Com2015_WGS84=readRDS("comITA_epgs_32632.rds")
Reg2015_WGS84=readRDS("regITA_epgs_32632.rds")

selected.circles_32632=spTransform(selected.circles, CRS=CRS(proj4string(CMProv2015_WGS84)))


res_r=list()
for ( i in 1:length(selected.circles)) {
  res_r[[i]]=Reg2015_WGS84[!is.na(over(Reg2015_WGS84,as(selected.circles_32632[i,],"SpatialPolygons"))),]
}

res_nomi_reg_20km=lapply(res_r,function(x) unique(c(as.character(x@data$REGIONE)[1])))

res=list()
for ( i in 1:length(selected.circles)) {
  res[[i]]=Com2015_WGS84[!is.na(over(Com2015_WGS84,as(selected.circles_32632[i,],"SpatialPolygons"))),]
}

res_nomi_com_20km=lapply(res,function(x) unique(c(as.character(x@data$COMUNE))))

res_nomi_adm=list()

for (i in 1:length(res_nomi_reg_20km)) {
  
  res_nomi_adm[[i]]=c(res_nomi_reg_20km[[i]],res_nomi_com_20km[[i]])  
  
}

saveRDS(res_nomi_adm,"res_nomi_adm.rds")
