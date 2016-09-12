############################################################################################################################################################################################

options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(rTwChannel)
library(data.table)
library(doBy)



###################################################################################################
# Working with sentiment data 


res_final=list()

files=Sys.glob("*.xls")
files=files[c(1,3)]

for ( i in 1:length(files)) {
    template=loadWorkbook(files[i])
    res_final[[i]]= readWorksheet(template, sheet = 1)
    xlcFreeMemory()
}


res_fin=lapply(res_final,function(x) as.data.frame(apply(x[,2:ncol(x),],2,FUN=function(y) as.numeric(gsub(",",".",y)))))


res_fin_sent_daily=data.frame(date=res_final[[1]]$DateTime,
                              sentpol_native=1-(4*atan2(abs(res_fin[[1]]$Tweets.score.neg),res_fin[[1]]$Tweets.score.pos))/pi,
                              sentpol_retweet=1-(4*atan2(abs(res_fin[[1]]$Retweets.score.pos),res_fin[[1]]$Tweets.score.pos))/pi,
                              sentpol_full=1-(4*atan2(abs(res_fin[[1]]$T.RT.score.neg),res_fin[[1]]$T.RT.score.pos))/pi,
                              sent_I_native=sqrt((res_fin[[1]]$Tweets.score.neg)^2+(res_fin[[1]]$Tweets.score.pos)^2),
                              sent_I_retweet=sqrt((res_fin[[1]]$Retweets.score.pos)^2+(res_fin[[1]]$Tweets.score.pos)^2),
                              sent_I_full=sqrt((res_fin[[1]]$T.RT.score.neg)^2+(res_fin[[1]]$T.RT.score.pos)^2)
)

mean_daily_sa=summaryBy(.~ date, res_fin_sent_daily)
names(mean_daily_sa)=names(res_fin_sent_daily)
saveRDS(mean_daily_sa,"res_fin_sent_daily.rds")

res_fin_sent_intradaily=data.frame(date=res_final[[2]]$DateTime,
                                   sentpol_native=1-(4*atan2(abs(res_fin[[2]]$Tweets.score.neg),res_fin[[2]]$Tweets.score.pos))/pi,
                                   sentpol_retweet=1-(4*atan2(abs(res_fin[[2]]$Retweets.score.pos),res_fin[[2]]$Tweets.score.pos))/pi,
                                   sentpol_full=1-(4*atan2(abs(res_fin[[2]]$T.RT.score.neg),res_fin[[2]]$T.RT.score.pos))/pi,
                                   sent_I_native=sqrt((res_fin[[2]]$Tweets.score.neg)^2+(res_fin[[2]]$Tweets.score.pos)^2),
                                   sent_I_retweet=sqrt((res_fin[[2]]$Retweets.score.pos)^2+(res_fin[[2]]$Tweets.score.pos)^2),
                                   sent_I_full=sqrt((res_fin[[2]]$T.RT.score.neg)^2+(res_fin[[2]]$T.RT.score.pos)^2)
)


saveRDS(res_fin_sent_intradaily,"res_fin_sent_intradaily.rds")

res_fin_sent_afternoon=res_fin_sent_intradaily[grep("20:00:00",res_fin_sent_intradaily[,1]),]
res_fin_sent_noon=res_fin_sent_intradaily[grep("16:00:00",res_fin_sent_intradaily[,1]),]
res_fin_sent_morning=res_fin_sent_intradaily[grep("12:00:00",res_fin_sent_intradaily[,1]),]
res_fin_sent_evening=res_fin_sent_intradaily[grep("00:00:00",res_fin_sent_intradaily[,1]),]
res_fin_sent_night=res_fin_sent_intradaily[grep("04:00:00",res_fin_sent_intradaily[,1]),]

list_sent=list(res_fin_sent_daily=mean_daily_sa,
res_fin_sent_morning=res_fin_sent_morning,
res_fin_sent_noon=res_fin_sent_noon,
res_fin_sent_afternoon=res_fin_sent_afternoon,
res_fin_sent_evening=res_fin_sent_evening,
res_fin_sent_evening=res_fin_sent_night
)


list_sent_date=lapply(list_sent,function(x) {x$date=as.Date(x$date);return(x)})

ls_date=data.frame(date=seq(as.Date("2015-05-01"),as.Date("2015-09-30"),by=1))

res_list_sa=list()

for ( i in 1:length(list_sent_date)) {
             res_list_sa[[i]]=merge(ls_date,list_sent_date[[i]],by="date",all.x=T)
}

names=c("daily","morning","noon","afternoon","evening","night")

HW_summer_2015=readRDS("HW_data_summer_2015.rds")

for ( j in 1:length(list_sent_date)) {
       temp=list()
      for ( i in 1:length(HW_summer_2015)) {
                                     temp[[i]]=merge(HW_summer_2015[[i]],list_sent_date[[j]],by="date",all.x=T)
      }
  
  
        saveRDS(temp,paste0("data/national_sa_location_",names[j],".rds"))
}

for ( j in 1:length(list_sent_date)) {
    temp=merge(data.frame(date=HW_summer_2015[[1]]$date),list_sent_date[[j]],by="date",all.x=T)
  
  
  saveRDS(temp,paste0("data/national_sa_",names[j],".rds"))
}
##################################################################################################
# http://www.let.rug.nl/basile/twita/sentix.php
                                     
                                     
