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


all_stats_HW=lapply(all_stats,function(x) x[which(x$HW_day==1),])
all_stats_crit=lapply(all_stats,function(x) x[which(x$Critical_day==1),])
all_stats_nocrit=lapply(all_stats,function(x) x[which(x$Critical_day==0),])

res_list=list()
for ( i in seq_along(all_stats_crit)) 
  {
  
   res_list[[i]]=data.frame(RTW_TW_nocrit=round(mean(all_stats_nocrit[[i]]$RTW_TW),1),
                            RTW_TW_HW=round(mean(all_stats_HW[[i]]$RTW_TW),1),
                            TW_nocrit=round(mean(all_stats_nocrit[[i]]$TW),1),
                            TW_HW=round(mean(all_stats_HW[[i]]$TW),1),
                            native_users_nocrit=round(mean(all_stats_nocrit[[i]]$U_native_users),1),
                            native_users_users_HW=round(mean(all_stats_HW[[i]]$U_native_users),1),
                            t_pvalue_RTW_TW_crit=ifelse(t.test(all_stats_nocrit[[i]]$RTW_TW,all_stats_crit[[i]]$RTW_TW)$p.value<0.05,"*","ns"),
                            t_pvalue_TW_crit=ifelse(t.test(all_stats_nocrit[[i]]$TW,all_stats_crit[[i]]$TW)$p.value<0.05,"*","ns"),
                            t_pvalue_native_users_crit=ifelse(t.test(all_stats_nocrit[[i]]$U_native_users,all_stats_crit[[i]]$U_native_users)$p.value<0.05,"*","ns"),
                            t_pvalue_TW_crit_num=ifelse(t.test(all_stats_nocrit[[i]]$TW,all_stats_crit[[i]]$TW)$p.value<0.05,1,0))
  
                          }

res_list_df=do.call("rbind",res_list)
res_list_df=data.frame(city=sapply(cities,simpleCap),res_list_df)
row.names(res_list_df)=NULL
saveRDS(res_list_df,"stratification_HW_crit.rds")
XLConnect::writeWorksheetToFile("analisi_t_student.xls",res_list_df,sheet="analisi")

res_list_df=readRDS("stratification_HW_crit.rds")

res_list_df$t_pvalue_TW_crit_num[7]=1


cities_sp@data$HW_TW_reliability=factor(ifelse(res_list_df$t_pvalue_TW_crit_num > 0.75, "Significant", "Not_significant"),c("Significant", "Not_significant"))
pal <- colorFactor(c( "red","navy"), domain = c("Significant", "Not_significant"))

m=leaflet(cities_sp) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(HW_TW_reliability == "Not_significant", 6, 10),
    color = ~pal(HW_TW_reliability),
    stroke = FALSE, fillOpacity = 0.5
  ) %>%
  addLegend("topright", pal = pal, values = ~HW_TW_reliability,
            title = "Social Heat \nReliability",
            opacity = 1
  )  %>% fitBounds(7, 36, 18, 48)

saveWidget(m,"HW_TW_reliability_summer_2015.html")
webshot(url="HW_TW_reliability_summer_2015.html",file = "HW_TW_reliability_summer_2015.png")

###############################################################################################################################

#webshot(url="table_t_student.html","table_t_student.jpg")


res_list_max=list()
for ( i in seq_along(all_stats_crit)) 
{
  
  
  res_list_max[[i]]=data.frame(RTW_TW_nocrit=round(max(all_stats_nocrit[[i]]$RTW_TW),1),
                           RTW_TW_crit=round(max(all_stats_crit[[i]]$RTW_TW),1),
                           RTW_TW_HW=round(max(all_stats_HW[[i]]$RTW_TW),1),
                           TW_nocrit=round(max(all_stats_nocrit[[i]]$TW),1),
                           TW_crit=round(max(all_stats_crit[[i]]$TW),1),
                           TW_HW=round(max(all_stats_HW[[i]]$TW),1),
                           native_users_nocrit=round(max(all_stats_nocrit[[i]]$U_native_users),1),
                           native_users_HW=round(max(all_stats_HW[[i]]$U_native_users),1))
}

res_list_max_df=do.call("rbind",res_list_max)
res_list_max_df=data.frame(city=cities,res_list_max_df)
saveRDS(res_list_max_df,"max_HW_crit.rds")
XLConnect::writeWorksheetToFile("analisi.xls",res_list_max_df,sheet="analisi")

# summary(lm(pop_cities~.-city,data=city_stats))

########################################################################################################
# Map of italian heat wave 2015

Day_of_HW=unlist(lapply(all_stats_reg, function(x) sum(x$HW_day,na.rm=T)))
ITA_reg@data$HeatWave_Days=NA
ITA_reg@data$HeatWave_Days[1:18]= Day_of_HW
HW_day_sp=ITA_reg
m=mapView(HW_day_sp[1:18,],zcol = "HeatWave_Days", stroke = F,color=rev(heat.colors(15)),alpha.regions = 0.6,layer.name="HW Days",legend=T) 
m=m@map   %>% fitBounds(7, 35, 18, 48)
saveWidget(m,"Heatwave_days_summer_2015.html")
webshot(url="Heatwave_days_summer_2015.html",file = "Heatwave_days_summer_2015.png")

#########################################################################################################

icon_social_B <- makeIcon(
  iconUrl = "http://149.139.8.55/data/icons/icon_social_B.png",
  iconWidth = 20, iconHeight = 20
)

mcities=leaflet(cities) %>% addTiles() %>%
  addMarkers(data=cities_sp,icon = icon_social_B,
              label = lapply(cities_sp$STATION.NAME,function(x) HTML(sprintf("<em>%s</em>",simpleCap(as.character(x)), SIMPLIFY = F))),
              labelOptions = lapply(1:nrow(cities_sp), function(x) {labelOptions(opacity=0.9,clickable = FALSE, noHide = TRUE)})
              ) %>% fitBounds(7, 35, 18, 48)
  
saveWidget(mcities,"Cities_analized.html")
webshot(url="Cities_analized.html",file = "Cities_analized.png")


#########################################################################################################################################################
# load data


mat_RTW_TW_df=readRDS("mat_RTW_TW.rds")
mat_TW_df=readRDS("mat_TW.rds")
mat_RTW_df=readRDS("mat_RTW.rds")
mat_U_full_users_df=readRDS("matU_full_users.rds")
mat_critical_days=readRDS("mat_critical_days.rds")



#########################################################################################################################################################
# create list of proportional icon

icon_social_A_list <- iconList( tw10=makeIcon(iconUrl = "http://149.139.8.55/data/icons/icon_social_blue_A.png",iconWidth = 10, iconHeight = 10),
                                tw20=makeIcon(iconUrl = "http://149.139.8.55/data/icons/icon_social_blue_A.png",iconWidth = 20, iconHeight = 20),
                                tw40=makeIcon(iconUrl = "http://149.139.8.55/data/icons/icon_social_blue_A.png",iconWidth = 40, iconHeight = 40),
                                tw60=makeIcon(iconUrl = "http://149.139.8.55/data/icons/icon_social_blue_A.png",iconWidth = 50, iconHeight = 50),
                                tw80=makeIcon(iconUrl = "http://149.139.8.55/data/icons/icon_social_blue_A.png",iconWidth = 60, iconHeight = 60))

class_tw=c(-1,10,30,50,100,500)

html_legend <- "<b><font size=\"10\">Date: DATE<b></font><br/>
                <img src='http://149.139.8.55/data/icons/icon_social_blue_A.png' style='width:10px;height:10px;'><br/> < 30<br/> 
                <img src='http://149.139.8.55/data/icons/icon_social_blue_A.png' style='width:20px;height:20px;'><br/> 31-50<br/> 
                <img src='http://149.139.8.55/data/icons/icon_social_blue_A.png' style='width:40px;height:40px;'><br/> 51-100<br/> 
                <img src='http://149.139.8.55/data/icons/icon_social_blue_A.png' style='width:50px;height:50px;'><br/> 101-200<br/> 
                <img src='http://149.139.8.55/data/icons/icon_social_blue_A.png' style='width:60px;height:60px;'><br/> over 200"



#########################################################################################################################################################


date_period=all_stats_reg[[1]]$date

peak_days=c("2015-06-05","2015-07-07","2015-07-22","2015-08-07","2015-08-31")
ind_days=sapply(peak_days,function(x) grep(x,caldo_stat_period_daily$date))
saveRDS(ind_days,"ind_days.rds")

ind_days=readRDS("ind_days.rds")

for ( i in ind_days) {
date_current=date_period[i]
html_legend_date=sub("DATE",as.character(date_current),html_legend)
ITA_reg@data$Critical=0
ITA_reg@data$Critical[1:18]=as.numeric(mat_critical_days[i,2:19])
ITA_reg@data$Critical=as.factor(ITA_reg@data$Critical)
cities_sp@data$temp=0
cities_sp@data$RTW_TW=NULL
cities_sp@data$RTW_TW_icon=cut(as.numeric(mat_RTW_TW_df[i,]),class_tw,label=names(icon_social_A_list))

m=mapview(ITA_reg,zcol = "Critical", stroke = FALSE,burst = F,color = c("white", "red"),layer.name=as.character(date_current),legend=F)
m=m@map %>% addMarkers(data=cities_sp,icon = ~icon_social_A_list[RTW_TW_icon])  %>% setView( 11,41, zoom = 5)
#m=m %>% addControl(html = html_legend, position = "bottomleft") 
saveWidget(m,paste0("daily_map_",date_current,".html"),selfcontained = F)
}



#########################################################################################################################################################
# correlation analisys

variables=names(all_stats[[1]])[c(2:6,13:19)]
variables_social=names(all_stats[[1]])[c(13:18)]

all_stats_p=lapply(all_stats,function(x) na.omit(x))



res_corr_cities=lapply(all_stats_p,function(x) as.data.frame(cor(x[variables])))
res_corr_sign_cities=lapply(all_stats_p,function(x) as.data.frame(cor.mtest(x[variables])))

corr_Tapp_max=lapply(res_corr_cities,function(x) x$Tappmax[6:11])
corr_Tapp_max_mat=data.frame(do.call("rbind",corr_Tapp_max),row.names =as.character(sapply(cities,simpleCap))) 
names(corr_Tapp_max_mat)=variables_social



corr_Tapp_s_max=lapply(res_corr_sign_cities,function(x) x$Tappmax[6:11])
corr_Tapp_s_max_mat=data.frame(do.call("rbind",corr_Tapp_s_max),row.names =as.character(sapply(cities,simpleCap))) 
tmaxapp_mat=as.matrix(round(corr_Tapp_max_mat,2))



corr_Tmin=lapply(res_corr_cities,function(x) x$tmin[6:11])
corr_Tmin_mat=data.frame(do.call("rbind",corr_Tmin),row.names =as.character(sapply(cities,simpleCap))) 
names(corr_Tmin_mat)=variables_social
corr_Tmin_mat=as.matrix(round(corr_Tmin_mat,2))



###################################################################################################################################
# heatmaps

palette_chart<- c(colorRampPalette(c("blue", "yellow"))(5),colorRampPalette(c("orange", "red"))(7))   
breaks <- c(seq(-1, 0.2,length=5),seq(0.3,0.5,length=5),seq(0.6,1,length=3))

###################################################################################################################################

png("heatmap_tappmax_cities.png",width=700,height = 550)
heatmap.2(x=as.matrix(round(corr_Tapp_max_mat,2)), 
          cellnote = as.matrix(round(corr_Tapp_max_mat,2)),  # same data set for cell labels
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          key=F,
          trace="none",         # turns off trace lines inside the heat map
          margins=c(7,6),       # ("margin.Y", "margin.X")
          col=palette_chart,       # use on color palette defined earlier 
          breaks=breaks,    # enable color transition at specified limits
          symkey=F,
          denscol="black",
          scale ="none", 
          dendrogram="none",     # only draw a row dendrogram
          Rowv="NA",
          Colv="NA",
          cexRow=0.8,
          cexCol=0.8)            # turn off column clustering)

legend("top", fill =c("#BFBF3F","#FFFF00","#FFA500","#FF1B00"),legend = c("No Significance","Weak Significance","Significance","Strong Significance"),
                       cex=1,horiz=T,title="Twitter Metrics Correlations N=124")
dev.off()

png("heatmap_tmin_cities.png",width=700,height = 550)

heatmap.2(x=as.matrix(round(corr_Tmin_mat,2)), 
          cellnote = as.matrix(round(corr_Tmin_mat,2)),  # same data set for cell labels
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          key=F,
          trace="none",         # turns off trace lines inside the heat map
          margins=c(7,6),      # ("margin.Y", "margin.X")
          col=palette_chart,       # use on color palette defined earlier 
          breaks=breaks,    # enable color transition at specified limits
          symkey=F,
          denscol="black",
          scale ="none", 
          dendrogram="none",     # only draw a row dendrogram
          Rowv="NA",
          Colv="NA",
          cexRow=0.8,
          cexCol=0.8)            # turn off column clustering)

legend("top", fill =c("#BFBF3F","#FFFF00","#FFA500","#FF1B00"),legend = c("No Significance","Weak Significance","Significance","Strong Significance"),
       cex=1,horiz=T,title="Twitter Metrics Correlations N=124")
dev.off()

#################################################################################################################################################################################
# References
# http://rstudio-pubs-static.s3.amazonaws.com/160113_28eed4f2eb8f46d49f6107251252086f.html




