options(java.parameters = "-Xmx4g" )
library(XLConnect)

setwd("/home/alf/Scrivania/lav_caldo/heatwave_data")



files=Sys.glob("*.xls")
res_final=list()

for ( i in 1:length(files)) {
  template=loadWorkbook(files[i])
  res_final[[i]]= readWorksheet(template, sheet = 1,startRow = 1,startCol = 0, endCol = 85)
  res_final[[i]]$date=as.Date(ISOdate(res_final[[i]][,55],res_final[[i]][,54],res_final[[i]][,53]))
  xlcFreeMemory() 
}



saveRDS(res_final,"heat_data_final_city.rds")
