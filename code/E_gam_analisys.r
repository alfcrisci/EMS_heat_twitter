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

for ( i in 1:length(all_stats)) {
 
  png(paste0(cities[i],"_tw_tmaxapp_gam.png"))
  temp=all_stats[[i]]
  plot(mgcv::gam(RTW_TW~s(Tappmax),data=temp),main=paste0(simpleCap(cities[i])," - All tweets vs TmaxApp"))
  dev.off()
  png(paste0(cities[i],"_tw_tmin_gam.png"))
  temp=all_stats[[i]]
  plot(mgcv::gam(RTW_TW~s(tmin),data=temp),main=paste0(simpleCap(cities[i])," - All tweets vs Tmin"))
  dev.off()
  
}




