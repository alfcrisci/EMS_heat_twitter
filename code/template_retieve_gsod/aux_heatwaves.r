require(xts)
require(car)
require(dplyr)
require(biometeoR)
require(sp)


###############################################################################################
# Useful function

inch2Millimeter <- function(val, ...) {
  
  val_new <- val * 25.4
  val_new <- round(val_new, ...)
  
  return(val_new)
}

knots2mps <- function(val, ...) {
  
  val_new <- val * 0.514444
  val_new <- round(val_new, ...)
  
  return(val_new)
}



knots2ms <- function(val, ...) {
  
  kmperhour = val*1.8535
  val_new = kmperhour*(1000/3600) 
  val_new <- round(val_new, ...)
  
  return(val_new)
}



retrieveGSOD=function(usaf,WBAN="99999",start_year = NA, end_year = NA, dsn = ".") 
{
  require(GSODTools)
  
  fls_gz <- sapply(start_year:end_year, function(year) {
    dlbase <- paste0(as.character(usaf), "-", WBAN, "-", year, ".op.gz")
    dlurl <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/",year, "/", dlbase)
    dlfile <- paste0(dsn, "/", dlbase)
    
    if (file.exists(dlfile)) {
      cat("File", dlfile, "already exists. Proceeding to next file... \n")
    }
    else {
      try(download.file(dlurl, dlfile), silent = FALSE)
    }
    return(dlfile)  })
  
}
is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

findindexes=function(x,window=15) {
  j=window+x
  indexdayj=c(c(365-window):365,c(1:365),c(1:window))
  indexdayj[(j-window+1):(j+window-1)]
}

index_mese=1:12

create_ts_heatwave_df=function(temp_long,name,climrange="1980-01-01/2010-12-31") {
  temp_ts=xts(temp_long[2:7],order.by=as.Date(temp_long$date))
  temp_ts$vmed=temp_ts$vmed
  temp_ts$vmax=temp_ts$vmax
  temp_ts$Tappmax=round(sapply(1:nrow(temp_ts$tmed),function(x) biometeoR::steadman_indoor(temp_ts$tmax[x],temp_ts$rhum[x])),1)
  temp_ts$Tappmed=round(sapply(1:nrow(temp_ts$tmed),function(x) biometeoR::steadman_indoor(temp_ts$tmed[x],temp_ts$rhum[x])),1)
  temp_ts$Tappmax_v=round(sapply(1:nrow(temp_ts$tmed),function(x) biometeoR::steadman_outdoor_shade(temp_ts$tmax[x],temp_ts$rhum[x],temp_ts$vmed[x])),1)
  temp_ts$Tappmed_v=round(sapply(1:nrow(temp_ts$tmed),function(x) biometeoR::steadman_outdoor_shade(temp_ts$tmed[x],temp_ts$rhum[x],temp_ts$vmed[x])),1)
  temp_ts$yday=strptime(as.Date(temp_long$date), format = "%Y-%m-%d")$yday+1
  temp_ts$yday365=strptime(as.Date(temp_long$date), format = "%Y-%m-%d")$yday+1
  temp_ts$yday365[which(temp_ts$yday==29)]=28
  temp_ts$yday365[which(temp_ts$yday==366)]=365
  temp_ts$mese=as.numeric(format(as.Date(temp_long$date),"%m"))
  temp_mat=as.data.frame(temp_ts[climrange])
  
  ##########################################################################################################################################################################
  
  temp_ts$TmaxMedian_Day=NA
  temp_ts$TmaxP90_Day=NA
  temp_ts$TmaxP95_Day=NA
  temp_ts$TmaxAppMedian_Day=NA
  temp_ts$TmaxAppP90_Day=NA
  temp_ts$TmaxAppP95_Day=NA
  temp_ts$TmaxApp_V_Median_Day=NA
  temp_ts$TmaxApp_V_P90_Day=NA
  temp_ts$TmaxApp_V_P95_Day=NA
  temp_ts$TminMedian_Day=NA
  temp_ts$TminP90_Day=NA
  temp_ts$TminP95_Day=NA
  temp_ts$TmedMedian_Day=NA
  temp_ts$TmedP90_Day=NA         
  temp_ts$TmedP95_Day=NA
  
  temp_ts$TmaxMedian_Month=NA
  temp_ts$TmaxP90_Month=NA
  temp_ts$TmaxP95_Month=NA
  temp_ts$TmaxAppMedian_Month=NA
  temp_ts$TmaxAppP90_Month=NA
  temp_ts$TmaxAppP95_Month=NA
  temp_ts$TmaxApp_V_Median_Month=NA
  temp_ts$TmaxApp_V_P90_Month=NA
  temp_ts$TmaxApp_V_P95_Month=NA
  temp_ts$TminMedian_Month=NA
  temp_ts$TminP90_Month=NA
  temp_ts$TminP95_Month=NA
  temp_ts$TmedMedian_Month=NA
  temp_ts$TmedP90_Month=NA         
  temp_ts$TmedP95_Month=NA
  
  ########################################################################################################################################################################
  
  temp_clim_daily=data.frame(TmaxMedian=NA,TmaxP90=NA,TmaxP95=NA,
                             TmaxAppMedian=NA,TmaxAppP90=NA,TmaxAppP95=NA,
                             TmaxApp_V_Median=NA, TmaxApp_V_P90=NA, TmaxApp_V_P95=NA,
                             TminMedian=NA,TminP90=NA,TminP95=NA,
                             TmedMedian=NA,TmedP90=NA,TmedP95=NA)
  
  temp_clim_daily[1:365,]=NA
  
  for ( i in 1:365) {
    
    temp_clim_daily$TmaxMedian[i]=as.numeric(median(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmax,na.rm=T)) 
    temp_clim_daily$TmaxP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmax,c(0.90),na.rm=T)[1])
    temp_clim_daily$TmaxP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmax,c(0.95),na.rm=T)[1])
    
    temp_clim_daily$TmaxAppMedian[i]=as.numeric(median(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax,na.rm=T)) 
    temp_clim_daily$TmaxAppP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax,c(0.90),na.rm=T)[1])
    temp_clim_daily$TmaxAppP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax,c(0.95),na.rm=T)[1])
    
    temp_clim_daily$TmaxApp_V_Median[i]=as.numeric(median(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax_v,na.rm=T)) 
    temp_clim_daily$TmaxApp_V_P90[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax_v,c(0.90),na.rm=T)[1])
    temp_clim_daily$TmaxApp_V_P95[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$Tappmax_v,c(0.95),na.rm=T)[1])
    
    
    temp_clim_daily$TmedMedian[i]=as.numeric(median(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmed,na.rm=T)) 
    temp_clim_daily$TmedP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmed,c(0.90),na.rm=T)[1])
    temp_clim_daily$TmedP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmed,c(0.95),na.rm=T)[1])
    
    temp_clim_daily$TminMedian[i]=as.numeric(median(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmin,na.rm=T)) 
    temp_clim_daily$TminP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmin,c(0.90),na.rm=T)[1])
    temp_clim_daily$TminP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$yday365 %in% findindexes(i))==TRUE),]$tmin,c(0.95),na.rm=T)[1])
    
  }
  
  saveRDS(temp_clim_daily,paste0(name,"_clim_daily.rds"))
  write.csv(temp_clim_daily,paste0(name,"_clim_daily.csv"))
            
  temp_clim_monthly=data.frame(TmaxMedian=NA,TmaxP90=NA,TmaxP95=NA,
                               TmaxAppMedian=NA,TmaxAppP90=NA,TmaxAppP95=NA,
                               TmaxApp_V_Median=NA, TmaxApp_V_P90=NA, TmaxApp_V_P95=NA,
                               TminMedian=NA,TminP90=NA,TminP95=NA,
                               TmedMedian=NA,TmedP90=NA,TmedP95=NA)
  
  temp_clim_monthly[1:12,]=NA
  
  for ( i in 1:12) {
    
    temp_clim_monthly$TmaxMedian[i]=as.numeric(median(temp_mat[which((temp_mat$mese == i)),]$tmax,na.rm=T)) 
    temp_clim_monthly$TmaxP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmax,c(0.90),na.rm=T)[1])
    temp_clim_monthly$TmaxP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmax,c(0.95),na.rm=T)[1])
    
    temp_clim_monthly$TmaxAppMedian[i]=as.numeric(median(temp_mat[which((temp_mat$mese == i)),]$Tappmax,na.rm=T)) 
    temp_clim_monthly$TmaxAppP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$Tappmax,c(0.90),na.rm=T)[1])
    temp_clim_monthly$TmaxAppP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$Tappmax,c(0.95),na.rm=T)[1])
    
    temp_clim_monthly$TmaxApp_V_Median[i]=as.numeric(median(temp_mat[which((temp_mat$mese == i)),]$Tappmax_v,na.rm=T)) 
    temp_clim_monthly$TmaxApp_V_P90[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$Tappmax_v,c(0.90),na.rm=T)[1])
    temp_clim_monthly$TmaxApp_V_P95[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$Tappmax_v,c(0.95),na.rm=T)[1])
    
    
    temp_clim_monthly$TmedMedian[i]=as.numeric(median(temp_mat[which((temp_mat$mese == i)),]$tmed,na.rm=T)) 
    temp_clim_monthly$TmedP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmed,c(0.90),na.rm=T)[1])
    temp_clim_monthly$TmedP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmed,c(0.95),na.rm=T)[1])
    
    temp_clim_monthly$TminMedian[i]=as.numeric(median(temp_mat[which((temp_mat$mese == i)),]$tmin,na.rm=T)) 
    temp_clim_monthly$TminP90[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmin,c(0.90),na.rm=T)[1])
    temp_clim_monthly$TminP95[i]=as.numeric(quantile(temp_mat[which((temp_mat$mese == i)),]$tmin,c(0.95),na.rm=T)[1])
  }
  
  saveRDS(temp_clim_monthly,paste0(name,"_clim_monthly.rds"))
  write.csv(temp_clim_monthly,paste0(name,"_clim_monthly.csv"))
  
  #######################################################################################################################################################################
  
  temp_ts$TmaxMedian_Day=temp_clim_daily[temp_ts$yday365,1]
  temp_ts$TmaxP90_Day=temp_clim_daily[temp_ts$yday365,2]
  temp_ts$TmaxP95_Day=temp_clim_daily[temp_ts$yday365,3]
  
  temp_ts$TmaxAppMedian_Day=temp_clim_daily[temp_ts$yday365,4]
  temp_ts$TmaxAppP90_Day=temp_clim_daily[temp_ts$yday365,5]
  temp_ts$TmaxAppP95_Day=temp_clim_daily[temp_ts$yday365,6]
  
  temp_ts$TmaxApp_V_Median_Day=temp_clim_daily[temp_ts$yday365,7]
  temp_ts$TmaxApp_V_P90_Day=temp_clim_daily[temp_ts$yday365,8]
  temp_ts$TmaxApp_V_P95_Day=temp_clim_daily[temp_ts$yday365,9]
  
  temp_ts$TminMedian_Day=temp_clim_daily[temp_ts$yday365,10]
  temp_ts$TminP90_Day=temp_clim_daily[temp_ts$yday365,11]
  temp_ts$TminP95_Day=temp_clim_daily[temp_ts$yday365,12]
  
  temp_ts$TmedMedian_Day=temp_clim_daily[temp_ts$yday365,13]
  temp_ts$TmedP90_Day=temp_clim_daily[temp_ts$yday365,14]         
  temp_ts$TmedP95_Day=temp_clim_daily[temp_ts$yday365,15]
  
  temp_ts$TmaxMedian_Month=temp_clim_monthly[temp_ts$mese,1]
  temp_ts$TmaxP90_Month=temp_clim_monthly[temp_ts$mese,2]
  temp_ts$TmaxP95_Month=temp_clim_monthly[temp_ts$mese,3]
  
  temp_ts$TmaxAppMedian_Month=temp_clim_monthly[temp_ts$mese,4]
  temp_ts$TmaxAppP90_Month=temp_clim_monthly[temp_ts$mese,5]
  temp_ts$TmaxAppP95_Month=temp_clim_monthly[temp_ts$mese,6]
  
  temp_ts$TmaxApp_V_Median_Month=temp_clim_monthly[temp_ts$mese,7]
  temp_ts$TmaxApp_V_P90_Month=temp_clim_monthly[temp_ts$mese,8]
  temp_ts$TmaxApp_V_P95_Month=temp_clim_monthly[temp_ts$mese,9]
  
  temp_ts$TminMedian_Month=temp_clim_monthly[temp_ts$mese,10]
  temp_ts$TminP90_Month=temp_clim_monthly[temp_ts$mese,11]
  temp_ts$TminP95_Month=temp_clim_monthly[temp_ts$mese,12]
  
  temp_ts$TmedMedian_Month=temp_clim_monthly[temp_ts$mese,13]
  temp_ts$TmedP90_Month=temp_clim_monthly[temp_ts$mese,14]         
  temp_ts$TmedP95_Month=temp_clim_monthly[temp_ts$mese,15]

  return(temp_ts)
  }

###############################################################################################




identify_heatwave=function(Hdays, mese,meseini=5,meseend=10) {
  
  if (length(Hdays) != length(mese)) { stop("day number must be equal to mese vector.") }
   heatwave=rep(0,length(Hdays))
   count=rep(0,length(Hdays))
   icountH=rep(0,length(Hdays))
   Hdays[which(is.na(Hdays))]=0
   for ( i in 2:length(Hdays)) { if ((mese[i] < meseini) || (mese[i] > meseend)) 
                                {next}
    
                                if ((Hdays[i] == 1) &  (Hdays[i-1] == 1)) 
                                    {heatwave[i]=1;icountH[i]=1;
                                
                                     spell=data.frame(len=rle(as.numeric(icountH[1:i]))$lengths,val=rle(as.numeric(icountH[1:i]))$values)
                                
                                    spell=subset(spell,val==1)
                                    spell$ID=1:nrow(spell)
                                    if (tail(spell$val,1)==1) 
                                        { count[i]=tail(spell$ID,1)}  
                                }
                                }
  return(count)
  
}


#############################################################################################################################################

analize_GSOD=function(x,missing=NA) {
                          retrieveGSOD(usaf=x,start_year = 1979, end_year = 2015)
                          temp_daily<- gzGsodStations(x,start_year = 1979, end_year = 1996)
                          temp_daily$Date=as.Date(ymd(temp_daily$YEARMODA))
                          temp_daily$YEARMODA=NULL
                          temp_daily$tmed <- toCelsius(temp_daily$TEMP, digits = 1)
                          temp_daily$tmax <- toCelsius(temp_daily$MAX, digits = 1)
                          temp_daily$tmin <- toCelsius(temp_daily$MIN, digits = 1)
                          temp_daily$prec <-inch2Millimeter(temp_daily$PRCP, digits = 1)
                          temp_daily$DEWP <- toCelsius(temp_daily$DEWP, digits = 1)
                          temp_daily$rhum <- 100*(exp((17.625*temp_daily$DEWP)/(243.04+temp_daily$DEWP))/exp((17.625*temp_daily$tmed)/(243.04+temp_daily$tmed)))
                          temp_daily$slp=temp_daily$SLP
                          temp_daily$prec[which(temp_daily$PRCPFLAG=="I")]=missing
                          temp_daily$vmed=knots2ms(temp_daily$WDSP,1)
                          temp_daily$vmax=knots2ms(temp_daily$MXSPD,1)
                          temp_daily=temp_daily[c("date","tmed","tmax","tmin","rhum","DEWP","vmed","vmax","prec","slp")]
}








