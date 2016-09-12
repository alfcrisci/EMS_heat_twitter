


devtools::install_github("environmentalinformatics-marburg/GSODTools", ref = "develop")
devtools::install_github("alfcrisci/rWeatherITA")


install.packages("biometeoR", repos = NULL, type="source") #neeed biometeoR in the working directory


library(GSODTools)
library(XLConnect)

source("aux_heatwaves.r")


#####################################################################################################
# roma ciampino  USAF 162390

retrieveGSOD(usaf="162390",start_year = 1979, end_year = 2015)
temp_daily<- gzGsodStations("162390",start_year = 1979, end_year = 2015)
temp_daily$Date=as.Date(lubridate::ymd(temp_daily$YEARMODA))
temp_daily$YEARMODA=NULL
temp_daily$tmed <- toCelsius(temp_daily$TEMP, digits = 1)
temp_daily$tmax <- toCelsius(temp_daily$MAX, digits = 1)
temp_daily$tmin <- toCelsius(temp_daily$MIN, digits = 1)
temp_daily$prec <-inch2Millimeter(temp_daily$PRCP, digits = 1)
temp_daily$DEWP <- toCelsius(temp_daily$DEWP, digits = 1)
temp_daily$rhum <- 100*(exp((17.625*temp_daily$DEWP)/(243.04+temp_daily$DEWP))/exp((17.625*temp_daily$tmed)/(243.04+temp_daily$tmed)))
temp_daily$slp=temp_daily$SLP
temp_daily$prec[which(temp_daily$PRCPFLAG=="I")]=NA
temp_daily$vmed=knots2ms(temp_daily$WDSP,1)
temp_daily$vmax=knots2ms(temp_daily$MXSPD,1)
temp_daily=temp_daily[c("Date","tmed","tmax","tmin","rhum","DEWP","vmed","vmax","prec","slp")]
saveRDS(temp_daily,paste0("162390","_1979_2015.rds"))

dates=seq(as.Date("1979-01-01"),as.Date("2015-12-31"),by=1)
pp=data.frame(Date=dates)
temp_daily=merge(pp,temp_daily,all.x=T)
saveRDS(temp_daily,paste0("162390","_1979_2015.rds"))

names(temp_daily)[1]="date"
roma_ciampino_ts=create_ts_heatwave_df(temp_daily,"Roma_ciampino")

XLConnect::writeWorksheetToFile("roma_ciampino.xls",roma_ciampino_ts,sheet="Roma_ciampino")

template_roma_f=loadWorkbook("roma_ciampino.xls")
roma_cia=readWorksheet(template_roma_f, sheet = 1,startRow = 1,startCol = 0, endCol = 85)  
template=loadWorkbook("template_heatwave.xls")
writeWorksheet(template,roma_cia, sheet="Data_heatwave_EU",startRow = 1,startCol = 1)
XLConnect::saveWorkbook(template)
XLConnect::saveWorkbook(template,file="roma_ciampino_heatwave.xls")
roma_cia_mat= readWorksheet(template, sheet = 1,startRow = 1,startCol = 0, endCol = 85)
roma_cia_mat$date=as.Date(ISOdate(roma_cia_mat[,55],roma_cia_mat[,54],roma_cia_mat[,53]))

saveRDS(roma_cia_mat,"roma_ciampino_heatwave.rds")

file.remove(list.files(pattern = ".op.gz"))

#####################################################################################################





