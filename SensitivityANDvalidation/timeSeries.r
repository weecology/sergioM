library(plotrix)

#validation <-function(site, eclipseoutput.dir)
{
  predicted <- read.table(file=paste(eclipseoutput.dir, "/daily_output_",site,"_DNDC.txt", sep=""), header=TRUE, sep="\t")
  observed <-(read.table(file=paste("/Users/sergiomarconi/Documents/MarconietAl2016/",site,"EC.csv",sep=""), header=TRUE, sep=","))
  observed["NEE"] <- -observed["NEE"]
  dailyNEE.all.general <- c(cor(as.numeric(observed$NEE), as.numeric(predicted$NEE)),
                            NSE.data.frame(as.numeric(predicted$NEE), as.numeric(observed$NEE)),
                            rmse(as.numeric(predicted$NEE), as.numeric(observed$NEE)),
                            mae.data.frame(as.numeric(predicted$NEE), as.numeric(observed$NEE)))
  
  
  ######################################################################################################
  # daily -------------------------------------------------------------------
  ######################################################################################################
  
  y.0 <-  predicted[1,"YEAR"]
  y.sim <-predicted[length(predicted[,1]),"YEAR"]
  #creating annual series of daily data
  for (i in y.0:y.sim) {
    #creating the predicted series
    nam <- paste("predicted.daily",i, sep = "") 
    assign(nam, predicted[which(predicted["YEAR"] == i),])
    
    #creating the observed series 
    nam <- paste("observed.daily",i, sep = "") 
    assign(nam, observed[which(observed["YEAR"] == i),])
    
    #appending annual results
    temp.obs <- get(paste("observed.daily",i, sep = ""))
    temp.pred <- get(paste("predicted.daily",i, sep = ""))
    temp <- c(cor(as.numeric(temp.obs$NEE), as.numeric(temp.pred$NEE)),
              NSE.data.frame(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)),
              rmse(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)),
              mae.data.frame(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)))
    #giving standardized datasets
    
    temp.z <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
    
    #interannual varaibility Xi - avgX
    
    
    
    if (i == y.0){
      dailyNEE.ann.general <- temp
      zDailyNEE.ann.general <- temp.z
    }else{
      dailyNEE.ann.general <- rbind(dailyNEE.ann.general, temp)
      zDailyNEE.ann.general <- rbind(zDailyNEE.ann.general, temp.z)
    }
  }
  
  
  
  #use="pairwise.complete.obs"
  
  ######################################################################################################
  # Monthly -----------------------------------------------------------------
  ######################################################################################################
  
  #creating annual series of daily data
  for (i in y.0:y.sim) {
    for(month in 1:12)
    {
      #creating the predicted series
      nam <- paste("predicted.monthly",i,".",month, sep = "") 
      assign(nam, predicted[which(predicted["YEAR"] == i & predicted["MONTH"] == month),])
      
      #creating the observed series 
      nam <- paste("observed.monthly",i,".",month, sep = "")  
      assign(nam, observed[which(observed["YEAR"] == i & observed["MONTH"] == month),])
      
      #appending monthly results
      temp.obs <- get(paste("observed.monthly",i,".",month, sep = ""))
      temp.pred <- get(paste("predicted.monthly",i,".",month, sep = "") )
      temp <- c(cor(as.numeric(temp.obs$NEE), as.numeric(temp.pred$NEE)),
                NSE.data.frame(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)),
                rmse(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)),
                mae.data.frame(as.numeric(temp.pred$NEE), as.numeric(temp.obs$NEE)))
      #giving standardized datasets
      
      temp.z <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                  NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                  rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                  mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
      
      if (i == y.0 & month ==1){
        monthlyNEE.ann.general <- temp
        zMonthlyNEE.ann.general <- temp.z
        
      }else{
        monthlyNEE.ann.general <- rbind(monthlyNEE.ann.general, temp)
        zMonthlyNEE.ann.general <- rbind(zMonthlyNEE.ann.general, temp.z)
      }
    }
    ######################################################################################################
    # seasonal ----------------------------------------------------------------
    ######################################################################################################
    
    #seasonal matrixes
    
    # DJF ######################################################################################################
    
    #creating the predicted series
    nam <- paste("predicted.DJF.",i, sep = "") 
    if(i==y.0){
      temp.pred <- rbind(get(paste("predicted.monthly",i,".",1, sep = "")),
                         get(paste("predicted.monthly",i,".",2, sep = "")))
      assign(nam, temp.pred)
      predicted.DJF.whole <- temp.pred
    }else{
      temp.pred <- rbind(get(paste("predicted.monthly",(i-1),".",12, sep = "")),
                         get(paste("predicted.monthly",i,".",1, sep = "")),
                         get(paste("predicted.monthly",i,".",2, sep = "")))
      assign(nam, temp.pred)
      predicted.DJF.whole <- rbind(predicted.DJF.whole, temp.pred)
    }
    
    #creating the observed series
    nam <- paste("observed.DJF.",i, sep = "") 
    if(i==y.0){
      temp.obs<- rbind(get(paste("observed.monthly",i,".",1, sep = "")),
                       get(paste("observed.monthly",i,".",2, sep = "")))
      assign(nam, temp.obs)
      observed.DJF.whole <- temp.obs
      
      seasonalNEE.DJF.general <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                   NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
    }else{
      temp.obs <- rbind(get(paste("observed.monthly",(i-1),".",12, sep = "")),
                        get(paste("observed.monthly",i,".",1, sep = "")),
                        get(paste("observed.monthly",i,".",2, sep = "")))    
      assign(nam, temp.obs)
      observed.DJF.whole <- rbind(observed.DJF.whole, temp.obs)
      seasonalNEE.DJF.general <- rbind(seasonalNEE.DJF.general, 
                                       c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                         NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
    }
    
    
    # MAM ######################################################################################################
    nam <- paste("predicted.MAM.",i, sep = "") 
    temp.pred <- rbind(get(paste("predicted.monthly",i,".",3, sep = "")),
                       get(paste("predicted.monthly",i,".",4, sep = "")),
                       get(paste("predicted.monthly",i,".",5, sep = "")))
    assign(nam, temp.pred)
    if(i==y.0){
      predicted.MAM.whole <- temp.pred
    }else {
      predicted.MAM.whole <- rbind(predicted.MAM.whole, temp.pred)
    }
    
    nam <- paste("observed.MAM.",i, sep = "") 
    temp.obs <- rbind(get(paste("observed.monthly",i,".",3, sep = "")),
                      get(paste("observed.monthly",i,".",4, sep = "")),
                      get(paste("observed.monthly",i,".",5, sep = "")))
    assign(nam, temp.obs)
    if(i==y.0){
      observed.MAM.whole <- temp.obs
      seasonalNEE.MAM.general <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                   NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
    }else{
      observed.MAM.whole <- rbind(observed.MAM.whole, temp.obs)
      seasonalNEE.MAM.general <- rbind(seasonalNEE.MAM.general, 
                                       c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                         NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
    }
    
    #JJA ######################################################################################################
    
    nam <- paste("predicted.JJA.",i, sep = "") 
    temp.pred <- rbind(get(paste("predicted.monthly",i,".",6, sep = "")),
                       get(paste("predicted.monthly",i,".",7, sep = "")),
                       get(paste("predicted.monthly",i,".",8, sep = "")))
    assign(nam, temp.pred)
    if(i==y.0){
      predicted.JJA.whole <- temp.pred
    }else{
      predicted.JJA.whole <- rbind(predicted.JJA.whole, temp.pred)
    }
    
    nam <- paste("observed.JJA.",i, sep = "") 
    temp.obs <- rbind(get(paste("observed.monthly",i,".",6, sep = "")),
                      get(paste("observed.monthly",i,".",7, sep = "")),
                      get(paste("observed.monthly",i,".",8, sep = "")))
    assign(nam, temp.obs)
    if(i==y.0){
      observed.JJA.whole <- temp.obs
      seasonalNEE.JJA.general <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                   NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
    }else{
      observed.JJA.whole <- rbind(observed.JJA.whole, temp.obs)
      seasonalNEE.JJA.general <- rbind(seasonalNEE.JJA.general, 
                                       c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                         NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
    }
    
    # SON ######################################################################################################
    
    nam <- paste("predicted.SON.",i, sep = "") 
    temp.pred <- rbind(get(paste("predicted.monthly",i,".",9, sep = "")),
                       get(paste("predicted.monthly",i,".",10, sep = "")),
                       get(paste("predicted.monthly",i,".",11, sep = "")))
    assign(nam, temp.pred)
    if(i==y.0){
      predicted.SON.whole <- temp.pred
    } else{
      predicted.SON.whole <- rbind(predicted.SON.whole, temp.pred)
    }
    
    nam <- paste("observed.SON.",i, sep = "") 
    temp.obs <- rbind(get(paste("observed.monthly",i,".",9, sep = "")),
                      get(paste("observed.monthly",i,".",10, sep = "")),
                      get(paste("observed.monthly",i,".",11, sep = "")))
    assign(nam, temp.obs)
    
    if(i==y.0){
      observed.SON.whole <- temp.obs
      seasonalNEE.SON.general <- c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                   NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                   mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
    }else{
      observed.SON.whole <- rbind(observed.SON.whole, temp.obs)
      seasonalNEE.SON.general <- rbind(seasonalNEE.SON.general, 
                                       c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                         NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                         mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
    }
  }
  ######################################################################################################
  #once for all seasons
  ######################################################################################################
  temp.pred <- predicted[which(predicted["YEAR"] == i & predicted["MONTH"] <3),]
  temp.obs <- observed[which(observed["YEAR"] == i & observed["MONTH"] <3),]  
  seasonalNEE.whole.general <-c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))))
  
  temp.pred <- predicted[which(predicted["YEAR"] == i & (predicted["MONTH"] <6 & predicted["MONTH"] > 2)),]
  temp.obs <- observed[which(observed["YEAR"] == i & (observed["MONTH"] <6 & predicted["MONTH"] > 2)),] 
  seasonalNEE.whole.general <- rbind(seasonalNEE.whole.general, c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                                                  NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                  rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                  mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
  
  temp.pred <- predicted[which(predicted["YEAR"] == i & (predicted["MONTH"] <9 & predicted["MONTH"] > 5)),]
  temp.obs <- observed[which(observed["YEAR"] == i & (observed["MONTH"] <9 & predicted["MONTH"] > 5)),] 
  seasonalNEE.whole.general <- rbind(seasonalNEE.whole.general, c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                                                  NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                  rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                  mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
  
  temp.pred <- predicted[which(predicted["YEAR"] == i & predicted["MONTH"] >8),]
  temp.obs <- observed[which(observed["YEAR"] == i & observed["MONTH"] >8),] 
  seasonalNEE.whole.general <- rbind(seasonalNEE.whole.general,c(cor(scale(as.numeric(temp.obs$NEE)), scale(as.numeric(temp.pred$NEE))),
                                                                 NSE.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                 rmse(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE))),
                                                                 mae.data.frame(scale(as.numeric(temp.pred$NEE)), scale(as.numeric(temp.obs$NEE)))))
}
