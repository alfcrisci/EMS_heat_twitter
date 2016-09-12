options(geonamesHost="api.geonames.org")
options(geonamesUsername="alfcrisci")

library(geonames)
library(raster)
library(sp)
library(WikidataR)
library(WikipediR)
library(rgdal)
library(rgeos)


#############################################################################

GNID <- function(x) {
  res <- GNsearch(name=x, country="IT")
  return(as.list(res))
}

#############################################################################

selected=readRDS("cities_sp.rds")
proj4string(selected)=CRS( "+init=epsg:4326" )
selected.projected <- spTransform(selected, CRS( "+init=epsg:23032" )) #utm32n
selected.circles.projected <- gBuffer(selected.projected, width=15000, byid=TRUE)
selected.circles<- spTransform(selected.circles.projected , CRS( "+init=epsg:4326" )) #geolatlon
plot(selected.circles)
saveRDS(selected.circles,"selected.circles.rds")

#############################################################################
# citta

cities=readRDS("list_cities.rds")
cities=tolower(as.character(selected.circles@data$STATION.NAME))


cities_geonames <-lapply(cities,GNID)
cities_names=lapply(cities_geonames,function(x) x$name[which(x$fcodeName !="hotel")])

saveRDS(cities_geonames,"cities_geonames.rds")
saveRDS(cities_names,"cities_names.rds")

############################################################################
# places osm italy

places_osm_ita=read.csv("places_ita_full.csv")
places_osm_ita=na.omit(places_osm_ita)
coordinates(places_osm_ita)=~ X.lon+X.lat
proj4string(places_osm_ita)=CRS( "+init=epsg:4326" )
saveRDS(places_osm_ita,"places_osm_ita.rds")

############################################################################
options(geonamesHost="api.geonames.org")
options(geonamesUsername="alfcrisci")

library(geonames)
library(raster)
library(sp)
library(WikidataR)
library(WikipediR)
library(rgdal)
library(rgeos)

setwd("/home/alf/Scrivania/lav_caldo")

cities_names=readRDS("cities_names.rds")
places_osm_ita=readRDS("places_osm_ita.rds")
selected.circles=readRDS("selected.circles.rds")

res=list()
for ( i in 1:length(selected.circles)) {
res[[i]]=places_osm_ita[!is.na(over(places_osm_ita,as(selected.circles[i,],"SpatialPolygons"))),]
}

saveRDS(res,"osm_places.rds")

names_osm_heat=lapply(res,function(x) x@data$name)

saveRDS(names_osm_heat,"names_osm_heat.rds")

wikidata_city=lapply(cities,function(x) find_item(x,language = "it", limit = 1))

#saveRDS(wikidata_city,"wikidata_city.rds")
#wikidata_city=readRDS("wikidata_city.rds")

Q_cities=unlist(lapply(wikidata_city,function(x) {x[[1]]$id}))

wikidata_item=lapply(Q_cities,function(x) get_item(x))

#saveRDS(wikidata_item,"wikidata_item.rds")

data_cities=lapply(wikidata_item,function(x) lapply(x$claims,function(x) x$mainsnak$datavalue))

properties_cities=unique(unlist(sapply(data_cities,names)))

data_cities_desc=lapply(properties_cities,function(x) {Sys.sleep(2);return(get_property(x))})
saveRDS(data_cities_desc,"data_cities_desc.rds")

label_ita=do.call("rbind",lapply(data_cities_desc,function(x) cbind(x$id,x$labels$en$value,x$labels$it$value)))

write.csv(do.call("rbind",e),"wikidata_cat_ita.csv")



