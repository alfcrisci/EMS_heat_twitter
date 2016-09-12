library(raster)
library(ncdf)
library(rts)
library(leaflet)
library(weatherData)

setwd("/home/alf/Scrivania/lav_caldo/lav_caldo_tw")
source("auxillary_functions.r")

ita_bound=getData('GADM', country='ITA', level=0)


####################################################################################################

tmax_italia=brick("redlav_tmax.nc",varname=c("tmax")) 
tmed_italia=brick("redlav_tmed.nc",varname=c("tmed")) 
tmin_italia=brick("redlav_min.nc",varname=c("tmin")) 
urel_italia=brick("redlav_urel.nc",varname=c("urel")) 
prec_italia=brick("redlav_prec.nc",varname=c("prec")) 

dates=as.Date(as.POSIXct(tmax_italia@z$hours*3600, origin="1992-01-01"))

caldo_data=read.csv("caldo_data.csv")

index_date_06=grep("-06-",caldo_data$date)
index_date_07=grep("-07-",caldo_data$date)
index_date_08=grep("-08-",caldo_data$date)
index_date_09=grep("-09-",caldo_data$date)

index_date_r_06=grep("-06-",dates)
index_date_r_07=grep("-07-",dates)
index_date_r_08=grep("-08-",dates)
index_date_r_09=grep("-09-",dates)

##################################################################################################

tmax_italia06=stack(tmax_italia[[index_date_r_06]])
caldo_native_06=caldo_data$native[index_date_06]
tmax_italia06_out=tmax_italia06[[1]]*NA
names(tmax_italia06_out)="sign_06_tmax"
tmax_italia06_out=setValues(tmax_italia06_out,as.numeric(apply(as.data.frame(tmax_italia06),1,function(x) { summary(lm(x ~ caldo_native_06))$coefficients[8] })))
tmax_italia06_out_s=reclassify(tmax_italia06_out, c(-Inf,0.05,1, 0.0505,Inf,NA))

tmax_italia06_s=crop_raster(ita_bound,tmax_italia06_out_s)
tmax_italia06_r=crop_raster(ita_bound,tmax_italia06_out)

saveRDS(tmax_italia06_s,"tmax_italia06_s.rds")
saveRDS(tmax_italia06_r,"tmax_italia06_r.rds")



tmax_italia07=stack(tmax_italia[[index_date_r_07]])
caldo_native_07=caldo_data$native[index_date_07]
tmax_italia07_out=tmax_italia07[[1]]*NA
names(tmax_italia07_out)="sign_07_tmax"
tmax_italia07_out=setValues(tmax_italia07_out,as.numeric(apply(as.data.frame(tmax_italia07),1,function(x) { summary(lm(x ~ caldo_native_07))$coefficients[8] })))
tmax_italia07_out_s=reclassify(tmax_italia07_out, c(-Inf,0.05,1, 0.0505,Inf,NA))

tmax_italia07_s=crop_raster(ita_bound,tmax_italia07_out_s)
tmax_italia07_r=crop_raster(ita_bound,tmax_italia07_out)

saveRDS(tmax_italia07_s,"tmax_italia07_s.rds")
saveRDS(tmax_italia07_r,"tmax_italia07_r.rds")



tmax_italia08=stack(tmax_italia[[index_date_r_08]])
caldo_native_08=caldo_data$native[index_date_08]
tmax_italia08_out=tmax_italia08[[1]]*NA
names(tmax_italia08_out)="sign_08_tmax"
tmax_italia08_out=setValues(tmax_italia08_out,as.numeric(apply(as.data.frame(tmax_italia08),1,function(x) { summary(lm(x ~ caldo_native_08))$coefficients[8] })))
tmax_italia08_out_s=reclassify(tmax_italia08_out, c(-Inf,0.05,1, 0.0505,Inf,NA))
tmax_italia08_s=crop_raster(ita_bound,tmax_italia08_out_s)
tmax_italia08_r=crop_raster(ita_bound,tmax_italia08_out)

saveRDS(tmax_italia08_s,"tmax_italia08_s.rds")
saveRDS(tmax_italia08_r,"tmax_italia08_r.rds")


tmax_italia09=stack(tmax_italia[[index_date_r_09]])
caldo_native_09=caldo_data$native[index_date_09]
tmax_italia09_out=tmax_italia09[[1]]*NA
names(tmax_italia09_out)="sign_09_tmax"
tmax_italia09_out=setValues(tmax_italia09_out,as.numeric(apply(as.data.frame(tmax_italia09)[,1:15],1,function(x) { summary(lm(x ~ caldo_native_09))$coefficients[8] })))
tmax_italia09_out_s=reclassify(tmax_italia09_out, c(-Inf,0.05,2, 0.051,0.10,1,0.101,Inf,NA))


png(file = "tmax_italia08_r.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia08_r,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(5,"YlOrRd"),legend=T)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
dev.off()


png(file = "tmax_italia08_s.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia08_s,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(3,"Reds"),legend=F)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Yes"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()

png(file = "tmax_italia07_r.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia07_r,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(5,"YlOrRd"),legend=T)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
dev.off()


png(file = "tmax_italia07_s.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia07_s,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(3,"Reds"),legend=F)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Yes"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()

png(file = "tmax_italia06_r.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia06_r,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(5,"YlOrRd"),legend=T)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
dev.off()


png(file = "tmax_italia06_s.png",width = 1024, height = 768, bg = "transparent")
plot(tmax_italia06_s,xlim = c(-5, 20), ylim = c(35,49),col=brewer.pal(3,"Reds"),legend=F)
plot(ita_bound,xlim = c(5, 20), ylim = c(35, 49),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Yes"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()


  
