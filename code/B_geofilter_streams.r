################################################################################################################################
# Performs channel two chain stream extraction: terms related to heat condition perception 
# and related topics in italian. Perfoms geoterms extractions.

options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(rTwChannel)
library(stringi)


setwd("/home/alf/Scrivania/lav_caldo")

source("aux_caldo_function.r")
cities=as.character(readRDS("list_cities.rds"))

channel_caldo=readRDS("data_streams/channel_caldo_period.rds") 
res=list()
words_caldo=c("caldo","afa","canicola","sudore","caldissimo","torrido","record","allarme","emergenza","bollino","bere","anziani","sete","umidità","sudore","anticiclone","disagio","canicola","caronte","umido","flegetonte","sudo","sudato")

for ( i in seq_along(words_caldo))  
    {
     res[[i]]=findword(as.character(words_caldo[i]),channel_caldo$message)
    }

id=unique(unlist(res))
channel_caldo_sel=channel_caldo[id,]
saveRDS(channel_caldo_sel,"data_streams/channel_caldo_sel.rds")

####################################################################################################
channel_caldo_sel=readRDS("data_streams/channel_caldo_sel.rds") 




res_geo=list()
res_geo_stat=list()
res_geo_daily=list()

for ( i in seq_along(cities))  
{ key=c(paste0(" ",cities[i]),paste0("#",cities[i]),paste0(" ",regions[i]),paste0("#",regions[i]))
  
  res=list()
  for ( j in seq_along(key))  
  {
  res[[j]]=findword(as.character(key[j]),channel_caldo_sel$message)
  }
  id=unique(unlist(res))
  res_geo[[i]]=as.data.frame(channel_caldo_sel[id,])
  
  res_geo_stat[[i]]=fastChannelstat(res_geo[[i]],stream="DISIT")
  res_geo_daily[[i]]=daily_channel_stat(res_geo[[i]],stream="DISIT")
  
  }

#######################################################################################################à
# store geo filtered streams

saveRDS(res_geo,paste0("channel_caldo_sel_cities_regions.rds"))
res_geo_stat_df=data.frame(cities,regions,do.call("rbind",res_geo_stat))


saveRDS(res_geo_stat_df,paste0("sel_stat_cities_regions.rds"))
write.csv(res_geo_stat_df,"res_geo_stat_df.csv")




date_period=data.frame(date=seq(as.Date("2015-05-15"),as.Date("2015-09-15"),by=1))
          
res_geo_daily_f=lapply(res_geo_daily,function(x) merge(date_period,x,all.x=T))

saveRDS(res_geo_daily_f,paste0("daily_stat_cities_regions.rds"))


res_geo_daily_f=readRDS("daily_stat_cities_regions.rds")


###############################################################################################################
# remove missing marks in counting column. 

for ( i in 1:length(res_geo_daily_f)) {
  for ( j in 2:4) {
   x=res_geo_daily_f[[i]][,j]
   x[is.na(x)]=0
   res_geo_daily_f[[i]][,j]=x  
  }
}

for ( i in 1:length(res_geo_daily_f)) {
  for ( j in 7:29) {
    x=res_geo_daily_f[[i]][,j]
    x[is.na(x)]=0
    res_geo_daily_f[[i]][,j]=x  
  }
}

for ( i in 1:length(res_geo_daily_f)) {
    x=res_geo_daily_f[[i]][,41]
    x[is.na(x)]=0
    res_geo_daily_f[[i]][,41]=x  
}

saveRDS(res_geo_daily_f,paste0("daily_stat_cities_regions.rds"))


##########################################################################################################################

data_HW_2015=readRDS("data_HW_2015.rds")


pars=c("RTW_TW","TW","RTW","U_native_users","U_full_users","U_native_hashtag","retweetCount","favoriteCount","N_geo")

for ( i in seq_along(data_HW_2015)) 

  {
  data_HW_2015[[i]]=cbind(data_HW_2015[[i]],res_geo_daily_f[[i]][pars])
  }

saveRDS(data_HW_2015,paste0("all_stats_cities_regions.rds"))

##########################################################################################################################

all_stats_cities_regions=readRDS("all_stats_cities_regions.rds")

for ( i in seq_along(all_stats_cities_regions)) 
{
  writeWorksheetToFile(file=paste0("all_stat_",cities[i],".xls"),data=all_stats_cities_regions[[i]],sheet = cities[i])
}

##########################################################################################################################



