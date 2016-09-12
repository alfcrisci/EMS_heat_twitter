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

fastConc<-function(model){
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-nrow(ones)*nrow(zeros)
  conc<-0
  disc<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    conc<-conc + sum(ones[i,"score"]>zeros[,"score"])
    disc<-disc + sum(ones[i,"score"]<zeros[,"score"])
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  ties_perc<-(1-concordance-discordance)
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}

calc_streams_caldo=function(x,citylabel="citta",tresh_glm=0.5) {
  require(segmented)
  require(ROCR)
  temp=x
  temp$deltatmaxapp=temp$tmaxapp-temp$tmaxapp90
  temp$fulltweet=temp$native_tweets+temp$native_retweets
  
  ######################################################################################à
  
  list_res_tmax=list()
  list_res_tmax$full_tmax_fulltweet=cor(temp$tmax,temp$fulltweet)
  list_res_tmax$full_tmax_originaltweet=cor(temp$tmax,temp$native_tweets)
  list_res_tmax$full_tmax_retweets=cor(temp$tmax,temp$native_retweets)
  list_res_tmax$full_tmax_Nunique_authors=cor(temp$tmax,temp$Nunique_authors) 
  list_res_tmax$full_tmax_Nuniq_hash=cor(temp$tmax,temp$Nuniq_hash) 
  list_res_tmax$full_tmax_Nuniq_links=cor(temp$tmax,temp$Nuniq_links)
  list_res_tmax$full_tmax_retweetCount=cor(temp$tmax,temp$retweetCount)
  list_res_tmax$full_tmax_N_favor=cor(temp$tmax,temp$N_favor) 
  
  segmented.mod.native_tweets <- segmented(lm(native_tweets~tmax,data=temp), seg.Z = ~tmax)
  png(paste0(citylabel,"_native_tweets_tmax.png"))
  plot(temp$tmax,temp$native_tweets, pch=16,ylab="Native tweets", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmax$break_originaltweet=segmented.mod.native_tweets$psi[2:3]
  
  segmented.mod.Nunique_authors <- segmented(lm(Nunique_authors~tmax,data=temp), seg.Z = ~tmax)
  png(paste0(citylabel,"_Nunique_authors_tmax.png"))
  plot(temp$tmax,temp$Nunique_authors, pch=16,ylab="Nunique_authors", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmax$break_Nunique_authors=segmented.mod.Nunique_authors$psi[2:3]
  
  ######################################################################################à
  
  list_res_tmin=list()
  list_res_tmin$full_tmin_fulltweet=cor(temp$tmin,temp$fulltweet)
  list_res_tmin$full_tmin_originaltweet=cor(temp$tmin,temp$native_tweets)
  list_res_tmin$full_tmin_retweets=cor(temp$tmin,temp$native_retweets)
  list_res_tmin$full_tmin_Nunique_authors=cor(temp$tmin,temp$Nunique_authors) 
  list_res_tmin$full_tmin_Nuniq_hash=cor(temp$tmin,temp$Nuniq_hash) 
  list_res_tmin$full_tmin_Nuniq_links=cor(temp$tmin,temp$Nuniq_links)
  list_res_tmin$full_tmin_retweetCount=cor(temp$tmin,temp$retweetCount)
  list_res_tmin$full_tmin_N_favor=cor(temp$tmin,temp$N_favor) 
  
  
  segmented.mod.native_tweets <- segmented(lm(native_tweets~tmin,data=temp), seg.Z = ~tmin)
  png(paste0(citylabel,"_native_tweets_tmin.png"))
  plot(temp$tmin,temp$native_tweets, pch=16,ylab="Native tweets", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmin$break_originaltweet=segmented.mod.native_tweets$psi[2:3]
  
  segmented.mod.Nunique_authors <- segmented(lm(Nunique_authors~tmin,data=temp), seg.Z = ~tmin)
  png(paste0(citylabel,"_Nunique_authors_tmin.png"))
  plot(temp$tmin,temp$Nunique_authors, pch=16,ylab="Nunique_authors", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmin$break_Nunique_authors=segmented.mod.Nunique_authors$psi[2:3]
  
  ######################################################################################à
  
  list_res_tmaxapp=list()
  list_res_tmaxapp$full_tmaxapp_fulltweet=cor(temp$tmaxapp,temp$fulltweet)
  list_res_tmaxapp$full_tmaxapp_originaltweet=cor(temp$tmaxapp,temp$native_tweets)
  list_res_tmaxapp$full_tmaxapp_retweets=cor(temp$tmaxapp,temp$native_retweets)
  list_res_tmaxapp$full_tmaxapp_Nunique_authors=cor(temp$tmaxapp,temp$Nunique_authors) 
  list_res_tmaxapp$full_tmaxapp_Nuniq_hash=cor(temp$tmaxapp,temp$Nuniq_hash) 
  list_res_tmaxapp$full_tmaxapp_Nuniq_links=cor(temp$tmaxapp,temp$Nuniq_links)
  list_res_tmaxapp$full_tmaxapp_retweetCount=cor(temp$tmaxapp,temp$retweetCount)
  list_res_tmaxapp$full_tmaxapp_N_favor=cor(temp$tmaxapp,temp$N_favor) 
  
  
  segmented.mod.native_tweets <- segmented(lm(native_tweets~tmaxapp,data=temp), seg.Z = ~tmaxapp)
  png(paste0(citylabel,"_Native_tweets_tmaxapp.png"))
  plot(temp$tmaxapp,temp$native_tweets, pch=16,ylab="Native tweets", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmaxapp$break_originaltweet=segmented.mod.native_tweets$psi[2:3]
  
  segmented.mod.Nunique_authors <- segmented(lm(Nunique_authors~tmaxapp,data=temp), seg.Z = ~tmaxapp)
  png(paste0(citylabel,"_Nunique_authors_tmaxapp.png"))
  plot(temp$tmaxapp,temp$Nunique_authors, pch=16,ylab="Nunique_authors", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_tmaxapp$break_Nunique_authors=segmented.mod.Nunique_authors$psi[2:3]
  
  list_res_deltatmaxapp=list()
  list_res_deltatmaxapp$full_deltatmaxapp_fulltweet=cor(temp$deltatmaxapp,temp$fulltweet)
  list_res_deltatmaxapp$full_deltatmaxapp_originaltweet=cor(temp$deltatmaxapp,temp$native_tweets)
  list_res_deltatmaxapp$full_deltatmaxapp_retweets=cor(temp$deltatmaxapp,temp$native_retweets)
  list_res_deltatmaxapp$full_deltatmaxapp_Nunique_authors=cor(temp$deltatmaxapp,temp$Nunique_authors) 
  list_res_deltatmaxapp$full_deltatmaxapp_Nuniq_hash=cor(temp$deltatmaxapp,temp$Nuniq_hash) 
  list_res_deltatmaxapp$full_deltatmaxapp_Nuniq_links=cor(temp$deltatmaxapp,temp$Nuniq_links)
  list_res_deltatmaxapp$full_deltatmaxapp_retweetCount=cor(temp$deltatmaxapp,temp$retweetCount)
  list_res_deltatmaxapp$full_deltatmaxapp_N_favor=cor(temp$deltatmaxapp,temp$N_favor) 
  
  
  segmented.mod.native_tweets <- segmented(lm(native_tweets~deltatmaxapp,data=temp), seg.Z = ~deltatmaxapp)
  png(paste0(citylabel,"_Native_tweets_deltatmaxapp.png"))
  plot(temp$deltatmaxapp,temp$native_tweets, pch=16,ylab="Native tweets", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_deltatmaxapp$break_originaltweet=segmented.mod.native_tweets$psi[2:3]
  
  segmented.mod.Nunique_authors <- segmented(lm(Nunique_authors~deltatmaxapp,data=temp), seg.Z = ~deltatmaxapp)
  png(paste0(citylabel,"_Nunique_authors_deltatmaxapp.png"))
  plot(temp$deltatmaxapp,temp$Nunique_authors, pch=16,ylab="Nunique_authors", xlab="Tair Max °C")
  plot(segmented.mod,col="red",add=T)
  dev.off()
  
  list_res_deltatmaxapp$break_Nunique_authors=segmented.mod.Nunique_authors$psi[2:3]
  
  ######################################################################################à
  list_res_HW=list()
  
  list_res_HW$modelbinom_nodelta_native_tweets=glm(HW_day~native_tweets,family=binomial(),data=temp)
  list_res_HW$modelbinom_delta_native_tweets=glm(HW_day~native_tweets+deltatmaxapp,family=binomial(),data=temp)
  list_res_HW$conco_nodelta_ntw=fastConc(list_res_HW$modelbinom_nodelta)
  list_res_HW$conco_wdelta_ntw=fastConc(list_res_HW$modelbinom_delta)
  
  
  val=predict(modelbinom_nodelta, type="response")
  mydf <-cbind(modelbinom_nodelta$model,val)
  mydf$response <- as.factor(ifelse(mydf$val>tresh_glm, 1, 0))
  list_res_HW$mb_nodelta_ntw_HW=mydf$response
  logit_scores <- prediction(predictions=mydf$val, labels=mydf$HW_day)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  list_res_HW$mb_nodelta_ntw_HW_logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
  logit_auc <- performance(logit_scores, "auc")
  list_res_HW$mb_nodelta_ntw_HW_AUC=as.numeric(logit_auc@y.values)
  
  val=predict(modelbinom_delta, type="response")
  mydf <-cbind(modelbinom_delta$model,val)
  mydf$response <- as.factor(ifelse(mydf$val>tresh_glm, 1, 0))
  list_res_HW$mb_wdelta_ntw_HW=mydf$response
  logit_scores <- prediction(predictions=mydf$val, labels=mydf$HW_day)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  list_res_HW$mb_wdelta_ntw_HW_logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
  logit_auc <- performance(logit_scores, "auc")
  list_res_HW$mb_wdelta_ntw_HW_AUC=as.numeric(logit_auc@y.values)
  
  return(list(list_res_tmax,list_res_tmaxapp,list_res_tmin,list_res_HW))
  
  
}

########################################################################################################
datefrom1970=function(x) as.Date(x,origin="1970-01-01")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
range0100 <- function(x){100*(x-min(x))/(max(x)-min(x))}


########################################################################################################################################################
# Create geo labels


regions=c("marche","puglia","emilia","trentino","sardegna","molise","sicilia","calabria","toscana","liguria","salento","lombardia","campania","sicilia","umbria","abruzzo",
          "calabria","lazio","piemonte","friuli","veneto")

findword <-  function(x,vec,case=TRUE) {which(grepl(paste0(x," "), vec, ignore.case=case)==T)}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

