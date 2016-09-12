#####################################################################################

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
library(RColorBrewer)
library(sjPlot)
library(sjmisc)
library(gplots)
library(xts)
library(dygraphs)

setwd("/home/alf/Scrivania/lav_caldo/modis")

#####################################################################################

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

######################################################################################
# Data


ITA_reg=readRDS("ITA_reg.rds")
cities=as.character(readRDS("list_cities.rds"))
cities_sp=readRDS("cities_sp.rds")
all_stats=readRDS("all_stats_cities_regions.rds")
all_stats_reg=readRDS("all_stats_cities_regions_reg.rds")
city_stats=readRDS("city_stats.rds")
dates_HW_2015=readRDS("dates_HW_2015.rds")

######################################################################################
setwd("/home/alf/Scrivania/lav_caldo/modis/graph_ts")

names_var=c( "RTW","TW","Tappmax")

for ( i in seq_along(all_stats)) {
date_period=all_stats[[i]]$date
temp_dates=dates_HW_2015[which(as.character(dates_HW_2015$city)==cities[i]),]
temp_social=all_stats[[i]][names_var]

social_heat <- xts(temp_social,order.by= date_period)

temp_html=dygraph(social_heat, main = paste0("Twitter impact in ",simpleCap(cities[i]))) %>% dyOptions(colors = c("navy","green","red"),stepPlot = TRUE,stackedGraph = TRUE,strokeWidth = 1) %>% dySeries(c("Tappmax"),stepPlot = F,fillGraph = F,strokeWidth = 3, axis = 'y2') %>% dyLegend(width = 400)

for ( j in 1:nrow(temp_dates)) {
  temp_html =dyShading(temp_html,from=temp_dates$date_first[j],to=temp_dates$date_last[j],color="#EFEFEF")
}
saveWidget(temp_html,file=paste0("graph_",cities[i],"_two_axis_tappmax.html"), selfcontained = T)
webshot(url=paste0("graph_",cities[i],"_two_axis_tappmax.html"),file=paste0("graph_",cities[i],"_two_axis_tappmax.png"),vwidth=1500)
}


names_var=c("TW", "RTW","tmin")

for ( i in seq_along(all_stats)) {
  date_period=all_stats[[i]]$date
  temp_dates=dates_HW_2015[which(as.character(dates_HW_2015$city)==cities[i]),]
  temp=all_stats[[i]][names_var]
  social_heat <- xts(temp,order.by= date_period)
  temp_html=dygraph(social_heat, main = paste0("Twitter impact in ",simpleCap(cities[i]))) %>% dyOptions(colors = c("navy","green","red"),stepPlot = TRUE,stackedGraph = TRUE,strokeWidth = 1) %>% dySeries(c("tmin"),stepPlot = F,fillGraph = F,strokeWidth = 3, axis = 'y2') %>% dyLegend(width = 400)
  
  for ( j in 1:nrow(temp_dates)) {
    temp_html =dyShading(temp_html,from=temp_dates$date_first[j],to=temp_dates$date_last[j],color="#EFEFEF")
  }
  saveWidget(temp_html,file=paste0("graph_",cities[i],"_two_axis_tmin.html"), selfcontained = T)
  webshot(url=paste0("graph_",cities[i],"_two_axis_tmin.html"),file=paste0("graph_",cities[i],"_two_axis_tmin.png"),vwidth=1500)
}

#################################################################################################################################à

setwd("/home/alf/Scrivania/lav_caldo/data_streams")

caldo_stat_period_daily=readRDS("caldo_stat_period_daily.rds")
caldo_stat_period=readRDS("caldo_stat_period.rds")
XLConnect::writeWorksheetToFile("stats_stream_caldo.xls",caldo_stat_period,sheet="nation")
setwd("/home/alf/Scrivania/lav_caldo")

channel_caldo_sel_cities_regions=readRDS("channel_caldo_sel_cities_regions.rds")
stats_cities_regions=readRDS("sel_stat_cities_regions.rds")
XLConnect::writeWorksheetToFile("stats_cities_regions.xls",stats_cities_regions,sheet="region")

setwd("/home/alf/Scrivania/lav_caldo/modis")


mat_tmaxapp=do.call("cbind",lapply(all_stats,function(x) x$Tappmax))
tmax_ita=apply(mat_tmaxapp,1,function(x) mean(x,na.rm=T))
mat_nation=data.frame(RTW_TW=caldo_stat_period_daily$RTW,
           TW=caldo_stat_period_daily$TW,
           TmaxApp=tmax_ita)
mat_nation_ts <- xts(mat_nation,order.by= caldo_stat_period_daily$date)
temp_html=dygraph(mat_nation_ts,  main = "Filtered Channel CALDO vs mean TMaxApp in Italy")  %>% dyOptions(colors = c("navy","green","red"),stepPlot = TRUE,stackedGraph = TRUE,strokeWidth = 1) %>% dySeries(c("TmaxApp"),stepPlot = F,fillGraph = F,strokeWidth = 3, axis = 'y2')  
 
setwd("/home/alf/Scrivania/lav_caldo/modis/graph_ts")

saveWidget(temp_html,file=paste0("Channel_caldo.html"), selfcontained = T)
webshot(url=paste0("Channel_caldo.html"),file=paste0("Channel_caldo.png"),vwidth=1500)

#########################################################################################################################################################
