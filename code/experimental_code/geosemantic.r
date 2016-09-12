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


options(geonamesHost="api.geonames.org")
options(geonamesUsername="alfcrisci")


#############################################################################
# creating buffer form weather station location 25 km.

selected=readRDS("selected_station_sp.rds")
selected=selected[c(1,2,3,6,7,8,12,13,14,16,17,19,20,21,23,24,25,27,29,32),]
selected@data$STATION.NAME=as.character(selected@data$STATION.NAME)

#############################################################################
# Correct names

cities=tolower(as.character(selected@data$STATION.NAME))

#############################################################################
# create buffer 25 km

proj4string(selected)=CRS( "+init=epsg:4326")
selected.projected <- spTransform(selected, CRS( "+init=epsg:23032" )) #utm32n
selected.circles.projected <- gBuffer(selected.projected, width=50000, byid=TRUE)
selected.circles<- spTransform(selected.circles.projected , CRS( "+init=epsg:4326" )) #geolatlon
plot(selected.circles)
saveRDS(selected.circles,"selected.circles.rds")


#############################################################################
# full names places osm italy

places_osm_ita=read.csv("places_ita_full.csv")
places_osm_ita=na.omit(places_osm_ita)
coordinates(places_osm_ita)=~ X.lon+X.lat
proj4string(places_osm_ita)=CRS( "+init=epsg:4326" )
saveRDS(places_osm_ita,"places_osm_ita.rds")

#############################################################################
# geonames from osm places

res=list()
for ( i in 1:length(selected.circles)) {
  res[[i]]=places_osm_ita[!is.na(over(places_osm_ita,as(selected.circles[i,],"SpatialPolygons"))),]
}

lapply(res,function(x) gsub("-"," ",unique(c(as.character(x@data$DEN_CMPRO),as.character(x@data$PROVINCIA)))))

saveRDS(res,"osm_places.rds")
names_osm_heat=lapply(res,function(x) x@data$name)
names_osm_heat=lapply(names_osm_heat, as.character)
saveRDS(names_osm_heat,"names_osm_heat.rds")

#######################################################################################
# reinitialization

cities=readRDS("list_cities.rds")
places_osm_ita=readRDS("places_osm_ita.rds")
selected.circles=readRDS("selected.circles.rds")
places_osm_selected=readRDS("osm_places.rds")
names_osm_heat=readRDS("names_osm_heat.rds")

#########################################################################################à
# find wikidata ID cities

wikidata_city=lapply(cities,function(x) find_item(x,language = "it", limit = 1))
saveRDS(wikidata_city,"wikidata_city.rds")
Q_cities=unlist(lapply(wikidata_city,function(x) {x[[1]]$id}))
saveRDS(Q_cities,"Q_cities.rds")
wikidata_city_item=lapply(Q_cities,get_item)
saveRDS(wikidata_city_item,"wikidata_city_item.rds")



##########################################################################################################à
# Sparql places

endpoint_basic <- "https://query.wikidata.org/sparql"

extract_places_label=function(x,endpoint="https://query.wikidata.org/bigdata/namespace/wdq/sparql") {
query=paste0("PREFIX wd: <http://www.wikidata.org/entity/> 
              PREFIX wdt: <http://www.wikidata.org/prop/direct/>
              PREFIX wikibase: <http://wikiba.se/ontology#>
              PREFIX p: <http://www.wikidata.org/prop/>
              PREFIX ps: <http://www.wikidata.org/prop/statement/>
              PREFIX pq: <http://www.wikidata.org/prop/qualifier/>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

       SELECT DISTINCT ?item  ?desc  ?coord ?image
               WHERE {
                        ?item wdt:P131 wd:",x,".
                        ?item rdfs:label ?desc filter (lang(?desc) = \"it\").   # desc  
                        OPTIONAL {?item wdt:P18  ?image.}                       # immagini
                        OPTIONAL {?item wdt:P625 ?coord.}                       # coordineate
                    }");
qd <- SPARQL(endpoint,query)
df <- qd$results
return(df)
}


wikidata_city_geo=lapply(Q_cities,function(x) extract_places_label(x))

saveRDS(wikidata_city_geo,"wikidata_city_geo.rds")

####################################################################################################

wikidata_geo2spdf=function(x) {
  require(rgdal)
  temp=x
  ind_na_coords=which(is.na(temp$coord))
  temp=temp[-ind_na_coords,]
  temp_geo_ls=lapply(temp$coord,function(x) rgeos::readWKT(toupper(gsub("^.","",gsub("...<http://www.opengis.net/ont/geosparql#wktLiteral>","",x,perl=T)))))
  temp_geo=do.call("rbind",temp_geo_ls)
  proj4string(temp_geo)=CRS("+init=epsg:4326")
  temp_geo_spdf=SpatialPointsDataFrame(temp_geo,temp[c("desc","image")])
  return(temp_geo_spdf)
}

wikidata_city_spdf=lapply(wikidata_city_geo[c(1:11,13:20)],wikidata_geo2spdf) #campobasso no item!
saveRDS(wikidata_city_spdf,"wikidata_city_spdf.rds")

########################################################################################################

selected.circles_new=selected.circles[c(1:11,13:20),] #to eliminate campobasso no item!
cities_new=cities[c(1:11,13:20)]
res_map=list()

for ( i in 1:length(wikidata_city_spdf)) {
temp=wikidata_city_spdf[[i]]
temp@data$image=gsub("^<","<img src = ",temp@data$image,perl=T)
temp=temp[!is.na(over(temp,as(selected.circles_new[i,],"SpatialPolygons"))),]
suppressWarnings(map <-mapview(temp))
res_map[[i]]=map
saveWidget(map@map,paste0(cities_new[i],".htm"))
}

saveRDS(res_map,"res_map.rds")

########################################################################################################




# your.map <- leaflet(pct) %>%
#   
#   # Add tiles as baseGroup
#   addProviderTiles("OpenTopoMap", group = "MapQuestOpen.Aerial") %>%
#   addProviderTiles("MapQuestOpen.Aerial", group = "MapQuestOpen.Aerial") %>%
#   addProviderTiles("OpenMapSurfer.Roads", group = "OpenMapSurfer.Roads") %>%
#   
#   # Add layers as overlayGroup
#   addPolylines(color="red", , weight = 4,  popup="PCT", , group = "PCT")  %>%
#   addMarkers(-116.4697, 32.60758, popup = "Campo", group="Southern Terminus") %>%
#   addMarkers(-120.7816, 49.06465, popup = "Manning Park, Canada", group="Northern Terminus") %>%
#   hideGroup("Southern Terminus") %>%
#   hideGroup("Northern Terminus") %>%
#   addPolygons(data=mapStates, fillColor = heat.colors(3, alpha = NULL), stroke = FALSE,
#               group = "States")  %>%
#   
#   # Layers control
#   addLayersControl(
#     baseGroups = c("MapQuestOpen.Aerial", "OpenTopoMap", "OpenMapSurfer.Roads"),
#     overlayGroups = c("PCT", "Southern Terminus", "Northern Terminus", "States"),
#     options = layersControlOptions(collapsed = FALSE)
#   ) 
