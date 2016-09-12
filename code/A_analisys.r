options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(rTwChannel)
library(data.table)
library(sp)
library(raster)




source("aux_caldo_function.r")

#########################################################################################################################################
# geo informations

cities_sp=readRDS("cities_sp.rds") #spatial points weather
cities=readRDS("list_cities.rds")



########################################################################################################################################
# heat data

heat_data_2015=readRDS("heatwave_data_city.rds")

period_ini=grep("2015-05-15",heat_data_2015[[1]]$date) # 13284
period_fin=grep("2015-09-15",heat_data_2015[[1]]$date) # 13407


data_HW_2015=lapply(heat_data_2015,function(x) {res=data.frame(x$date,x$tmed,x$tmax,x$tmin,x$rhum,x$Tappmax,
                                                       x$TmaxAppP90_Day,x$TmaxAppMedian_Day,x$TminP90_Day,
                                                       x$CRITICAL.DAYS.1,x$HW.T.LUNGHEZZA)[period_ini:period_fin,];
                                                       names(res)=c("date","tmed","tmax","tmin","rhum","Tappmax","TmaxAppP90_Day","TmaxAppMedian_Day","TminP90_Day","Critical_day","HW_length")
                                                       return(res)
                                                       })
   
                                                       
# heat wave day calculation
                                                      
for ( i in 1:length(data_HW_2015)){
data_HW_2015[[i]]$HW_day=data_HW_2015[[i]]$Critical_day
for( j in 1:(length(data_HW_2015[[i]]$Critical_day)-1)) { if ((data_HW_2015[[i]]$Critical_day[j]==1) & (data_HW_2015[[i]]$Critical_day[j+1]==0) & (data_HW_2015[[i]]$HW_length[j]==0)) {data_HW_2015[[i]]$HW_day[j]=0} }
}

########################
# perugia missing data 

temp=data_HW_2015[[15]][,10:12]
temp[temp==-999]=0
data_HW_2015[[15]][,10:12]=temp
########################


saveRDS(data_HW_2015,"data_HW_2015.rds")


stats_HW=lapply(data_HW_2015,function(x) data.frame(sumCrit_D=sum(x$Critical_day,na.rm=T),
                                                    sumHW_D=sum(x$HW_day,na.rm=T),
                                                    Nevents_HW=length(which(x$HW_length>0)),
                                                    meanHW_l=mean(x$HW_length[which(x$HW_length>0)],na.rm=T),
                                                    maxHW_l=max(x$HW_length[which(x$HW_length>0)],na.rm=T)))

stats_HW_2015=data.frame(city=cities,do.call("rbind",stats_HW))
saveRDS(stats_HW_2015,"stats_HW_2015.rds")

list_date_HW=list()
for ( i in 1:length(data_HW_2015)){
list_date_HW[[i]]=data.frame(city=cities[i],date_last=data_HW_2015[[i]]$date[which(data_HW_2015[[i]]$HW_length>0)],date_first=data_HW_2015[[i]]$date[which(data_HW_2015[[i]]$HW_length>0)]-(data_HW_2015[[i]]$HW_length[which(data_HW_2015[[i]]$HW_length>0)]-1))
}

# 
# 
dates_HW_2015=do.call("rbind",list_date_HW)
dates_HW_2015$HW_Periods=NA
saveRDS(dates_HW_2015,"dates_HW_2015.rds")

####################################################################################################################################
dates_HW_2015=read.csv("dates_HW_2015.csv")

dates_HW_2015_episode=split(dates_HW_2015,dates_HW_2015$HW_Periods)

stats_period=lapply(dates_HW_2015_episode,function(x) data.frame(city_involved=length(unique(x$city)),
                                                    meanlength=mean(x$length),
                                                    max_length=max(x$length))
                                                    )
                                       


initial_HW=as.Date(unlist(lapply(dates_HW_2015_episode,function(x) as.Date(head(x$date_first[1],1)))),origin="1970-01-01")

##################################################################################################################################à
# twitter


channel_caldo_sel=readRDS("channel_caldo_sel.rds")
channel_caldo_period=readRDS("channel_caldo_period.rds")

caldo_stat_sel=fastChannelstat(channel_caldo_sel,stream="DISIT")

saveRDS(caldo_stat_sel,"caldo_stat_sel.rds")

caldo_stat_daily=daily_channel_stat(channel_caldo_sel,stream="DISIT")
saveRDS(caldo_stat_daily,"caldo_stat_daily.rds")

caldo_stat_period=fastChannelstat(channel_caldo_period,stream="DISIT")
saveRDS(caldo_stat_period,"caldo_stat_period.rds")

caldo_stat_period_daily=daily_channel_stat(channel_caldo_period,stream="DISIT")

saveRDS(caldo_stat_period_daily,"caldo_stat_period_daily.rds")




##########################################################################################################################

data_HW_2015=readRDS("data_HW_2015.rds")

res_geo_daily_f=readRDS("daily_stat_cities_regions.rds")

pars=c("RTW_TW","TW","RTW","U_native_users","U_full_users","U_native_hashtag","retweetCount","favoriteCount","N_geo")

for ( i in seq_along(data_HW_2015)) 
  
{
  data_HW_2015[[i]]=cbind(data_HW_2015[[i]],res_geo_daily_f[[i]][pars])
}

saveRDS(data_HW_2015,paste0("all_stats_cities_regions.rds"))


all_stats_cities_regions=readRDS("all_stats_cities_regions.rds")
cities_index=c(1,2,3,4,5,6,9,10,12,13,14,15,16,17,18,19,20,21)
all_stats_cities_regions_reg=all_stats_cities_regions[cities_index]
saveRDS(all_stats_cities_regions_reg,"all_stats_cities_regions_reg.rds")

