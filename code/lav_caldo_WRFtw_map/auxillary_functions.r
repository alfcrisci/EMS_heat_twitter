###########################################################################################################################################
# Urban hazard risk analyses for identifying urban heat-related human ederly health risk areas in main the most populous Italian cities.

# Marco Morabito  Alfonso Crisci  Beniamino Gioli  Giovanni Gualtieri  Piero Toscano, Simone Orlandini, Gian Franco Gensini
# Institute of Biometeorology, National Research Council - Via Giovanni Caproni 8, 50145 Florence, Italy.
# Interdepartmental Centre of Bioclimatology, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Department of Agrifood Production and Environmental Sciences, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Clinica Medica e Cardiologia, University of Florence - Viale Morgagni 85, 50134 Florence, Italy.
#############################################################################################################################################

##################################################################################################################################
# Auxillary function for plotting rasters and vectorl layer and palette management
##################################################################################################################################



require(OpenStreetMap)
require(rasterVis)
require(ggplot2)
require(rgdal)
require(ggmap)
require(grid)
require(latticeExtra)
require(maptools)
require(plyr)




rectify_matlab_array_mat <- function(raster,inCRS="+init=epsg:3035",outCRS="+init=epsg:4326") {
  require(matlab)
  require(ncdf)
  pp=as.vector(extent(raster))
  tempmat=flipud(rot90(as.matrix(raster)))
  r1 <- raster(nrows=nrow(tempmat), ncols=ncol(tempmat), xmn=pp[1], xmx=pp[2],ymn=pp[3],ymx=pp[4],crs=inCRS)
  values(r1)<-tempmat
  r <- projectRaster(r1, crs=outCRS)
  return(r)
}





crop_raster=function(sppolydf,r) {
                                     require(raster)
                                     if (class(sppolydf)!="SpatialPolygonsDataFrame") {stop("Is not a R Spatial polygon object.")}
                                     if (class(r)!="RasterLayer") {stop("Is not a R raster object")}
                                     proj4string(r)<-"+init=epsg:4326"
                                     proj4string(sppolydf)<-"+init=epsg:4326"
                                     r.sub <- crop(r, extent(sppolydf))
                                     r.sub <- mask(r.sub, sppolydf)
                                     return(r.sub)
}

#######################################################################################################################
# convert kml to kmz format


kml2kmz=function (file.name, zip = Sys.getenv("R_ZIPCMD", "zip"), files = "", 
    rm = FALSE,...) 
{   require(stringr) 
    extension <- str_extract(file.name, pattern = "*\\..*$")
    kmz <- str_replace(file.name, extension, ".kmz")
    file.copy(file.name,"doc.kml")
    zip(zipfile = paste( kmz, sep = "/"), files = c("doc.kml",files))
    file.remove("doc.kml")
    if (file.exists(kmz) & rm == TRUE) {
        file.remove(file.name, files)
    }
}
#######################################################################################################################
# add trasparency to rgb palette

add.alpha <- function(col, alpha=1){
if(missing(col))
stop("Please provide a vector of colours.")
apply(sapply(col, col2rgb)/255, 2,
function(x)
rgb(x[1], x[2], x[3], alpha=alpha))
}


#######################################################################################################################
# interface function between OpenStreetMap and ggmap R packages

osm2ggmap=function(map_longlat) {
          require(OpenStreetMap)
          map_raster=raster(map_longlat)
          mapcol=rgb(as.vector(map_raster[[1]]),as.vector(map_raster[[2]]),as.vector(map_raster[[3]]),maxColorValue=255)
          map=matrix(mapcol,map_raster@nrows,map_raster@ncols)
          class(map) <- c('ggmap','raster')
          # map spatial info
          attr(map, 'bb') <- data.frame(
          ll.lat = map_longlat$bbox$p2[2], ll.lon = map_longlat$bbox$p1[1],
          ur.lat = map_longlat$bbox$p1[2], ur.lon = map_longlat$bbox$p2[1])
          return(map)
    }
	
	
#######################################################################################################################
# based on work Oscar Perpi?an oscar.perpinan@upm.es  https://github.com/oscarperpinan
	
stamenmap <- function(poly, map, ...){
             bbMap <- attr(map, 'bb')
             latCenter <- with(bbMap, ll.lat + ur.lat)/2
             lonCenter <- with(bbMap, ll.lon + ur.lon)/2
             height <- with(bbMap, ur.lat - ll.lat)
             width <- with(bbMap, ur.lon - ll.lon)
             spplot(poly, ...) +
             latticeExtra::layer({
             grid.raster(map,x=lonCenter, y=latCenter,width=width, height=height,default.units='native')
             }, under=TRUE,
             data=list(map=map,
             lonCenter=lonCenter, latCenter=latCenter,
             width=width, height=height))
}

#######################################################################################################################
# based on work Oscar Perpi?an oscar.perpinan@upm.es  https://github.com/oscarperpinan

rastervismap <- function(p, map, ...){
              bbMap <- attr(map, 'bb')
              latCenter <- with(bbMap, ll.lat + ur.lat)/2
              lonCenter <- with(bbMap, ll.lon + ur.lon)/2
              height <- with(bbMap, ur.lat - ll.lat)
              width <- with(bbMap, ur.lon - ll.lon)
              levelplot(p,...)+
              latticeExtra::layer({
              grid.raster(map,
              x=lonCenter, y=latCenter,
              width=width, height=height,
              default.units='native')
              }, under=TRUE,
              data=list(map=map,
              lonCenter=lonCenter, latCenter=latCenter,
              width=width, height=height))
              }
			  
			  
#######################################################################################################################
# based on work Oscar Perpi?an oscar.perpinan@upm.es  https://github.com/oscarperpinan
			  
polyframemap <- function(poly){
                bbPoly <- bbox(poly)
                gmap <- get_map(c(bbPoly), maptype='watercolor',
                source='stamen', crop=FALSE)
                bbMap <- attr(gmap, 'bb')
                latCenter <- with(bbMap, ll.lat + ur.lat)/2
                lonCenter <- with(bbMap, ll.lon + ur.lon)/2
                height <- with(bbMap, ur.lat - ll.lat)
                width <- with(bbMap, ur.lon - ll.lon)
                spplot(poly) +
                layer({
                grid.raster(gmap,
                x=lonCenter, y=latCenter,
                width=width, height=height,
                default.units='native')
                }, under=TRUE,
                data=list(gmap=gmap,
                lonCenter=lonCenter, latCenter=latCenter,
                width=width, height=height))
}


#######################################################################################################################
# centroid assessement function

centroid <- function (x1, y1) {
n <- length(x1)
wrap <- c(n, 1:(n - 1))
x2 <- x1[wrap]
y2 <- y1[wrap]
a <- x1 * y2 - x2 * y1
s <- sum(a) * 3
if (s < 1e-3) {
c(mean(x1), mean(y1))
} else {
c(sum((x1 + x2) * a)/s, sum((y1 + y2) * a)/s)
}
}


#######################################################################################################################
# http://rstudio-pubs-static.s3.amazonaws.com/3334_35452e4a611a4fa39ccebd26dffe4c9e.html

makePalette <- function(colourvector) {
    cmat = cbind(t(col2rgb(colourvector)), 255)
    res = apply(cmat, 1, function(x) {
        sprintf("<Entry c1=\"%s\" c2=\"%s\" c3=\"%s\" c4=\"%s\"/>", x[1], x[2], 
            x[3], x[4])
    })
    res = paste(res, collapse = "\n")
    res
}

makePaletteVRT <- function(raster, colourvector) {
    s = sprintf("<VRTDataset rasterXSize=\"%s\" rasterYSize=\"%s\">\n<VRTRasterBand dataType=\"Byte\" band=\"1\">\n<ColorInterp>Palette</ColorInterp>\n<ColorTable>\n", 
        ncol(raster), nrow(raster))
    p = makePalette(colourvector)
    s = paste0(s, p, "\n</ColorTable>\n</VRTRasterBand>\n</VRTDataset>\n")
    s
}

writePaletteVRT <- function(out, raster, colourvector) {
    s = makePaletteVRT(raster, colourvector)
    cat(s, file = out)
}			  
#######################################################################################################################
			  
