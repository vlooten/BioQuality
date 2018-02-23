# Fonction de description des tendances ####
describetrends_old <- function(inputData, opt1=T, bp=NULL, setwindowSize=120){
  inputData <- sample_n(inputData, min(100000,nrow(inputData)))
  inputData <- inputData[!is.na(inputData$value),]

  getinfo <- function(x, opt2=opt1){
    if(typeof(x)=="list"){
      if(opt2){
        out <- tryCatch(c(coefficients(x)[2,1],coefficients(x)[2,2],coefficients(x)[2,3],as.vector(summary(x)$coefficients[2,2])
                          ,as.vector(dchisq(summary(x)$coefficients[2,3],1)),coefficients(x)[1,1] ), error = function(e) c(coefficients(x)[2,1],coefficients(x)[2,2],coefficients(x)[2,3],NA,NA,coefficients(x)[1,1]  ) )
      }else{
        out <- tryCatch(c(as.vector(coefficients(x)[2]),confint(x)[2,1],confint(x)[2,2],summary(x)$coefficients[2,2],dchisq(summary(x)$coefficients[2,3],1),coefficients(x)[1]), error = function(e) rep(NA,6))
      }
    }else{
      out <- rep(NA,6)
    }
    return(out) 
  }
  
  listecol <-  c("bp","datepos","Type","Intercept","coef","LB","UB","Std","p","Scale","Delta (%)")
  # res <- data.frame(matrix(NA, nrow=6*ifelse(bp$nbp==0,1,2+bp$nbp),ncol=length(listecol) ))
  nbregression <- 6
  res <- data.frame(matrix(NA, nrow=nbregression*ifelse(bp[1]==0,1,2+bp$nbp),ncol=length(listecol) ))
  colnames(res) <-listecol
  
  res[,"bp"] <- 0
  res[,"datepos"] <- NA
  res[,"Type"] <- c("LM","QR05","QR25","QR50","QR75","QR90")
  
  res[1:6,"datepos"] <- paste0(min(inputData$date,na.rm = T),"//",max(inputData$date,na.rm = T))
  res[1:6,"Delta (%)"] <- max(inputData$date,na.rm = T)-min(inputData$date,na.rm = T)
  
  obj0 <- tryCatch(lm(value~date, data=inputData), error = function(e) NA)
  obj1 <- tryCatch(rq(value~date, tau=.05, data=inputData, ci=T), error = function(e) NA)
  obj2 <- tryCatch(rq(value~date, tau=.25, data=inputData, ci=T), error = function(e) NA)
  obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData, ci=T), error = function(e) NA)
  obj4 <- tryCatch(rq(value~date, tau=.75, data=inputData, ci=T), error = function(e) NA)
  obj5 <- tryCatch(rq(value~date, tau=.9, data=inputData, ci=T), error = function(e) NA)
  
  res[1,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj0,opt2=F)
  res[2,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj1,opt2=T)
  res[3,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj2,opt2=T)
  res[4,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)
  res[5,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj4)
  res[6,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj5)
  
  res[1,"Scale"] <- mean(inputData$value,na.rm = T)
  res[2,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .05)
  res[3,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .25)
  res[4,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .5)
  res[5,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .75)
  res[6,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .9)
  
  if(bp[1]>0){
    followcut <- c(0,bp$place,Inf)
    pp <- 1
    for(kk in 1:(length(followcut)-1)){
      newind <- pp*nbregression
      res[c(1:nbregression)+newind,"bp"] <- pp
      
      # ind <- which(inputData$date<followcut[kk+1] & inputData$date>followcut[kk])
      # need to fix a marge of 15 days
      up <- max( (as.numeric(followcut[kk+1])-setwindowSize/2),(as.numeric(followcut[kk])+setwindowSize/2),na.rm = T)
      low <- min( (as.numeric(followcut[kk])+setwindowSize/2),up, na.rm=T)
      
      res[c(1:nbregression)+newind,"datepos"] <- paste0(low,"//",up)
      res[c(1:nbregression)+newind,"Delta (%)"] <- up-low
      
      if(is.na(low) | is.na(up) | up==low){
        obj0 <- NA
        obj1 <- NA
        obj2 <- NA
        obj3 <- NA
        obj4 <- NA
        obj5 <- NA
      }else{
        ind <- which(inputData$date<=up & inputData$date>=low)
        if(length(unique(inputData[ind,"date3"]))<setwindowSize){
          obj0 <- NA
          obj1 <- NA
          obj2 <- NA
          obj3 <- NA
          obj4 <- NA
          obj5 <- NA
        }else{
          # obj0 <- tryCatch(lm(value~date3, data=inputData[ind,]), error = function(e) NA, warning=function(w) NA)
          # obj1 <- tryCatch(rq(value~date3, tau=.05, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          # obj2 <- tryCatch(rq(value~date3, tau=.25, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          # obj3 <- tryCatch(rq(value~date3, tau=.5, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          # obj4 <- tryCatch(rq(value~date3, tau=.75, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          # obj5 <- tryCatch(rq(value~date3, tau=.9, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          
          obj0 <- tryCatch(lm(value~date, data=inputData[ind,]), error = function(e) NA, warning=function(w) NA)
          obj1 <- tryCatch(rq(value~date, tau=.05, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          obj2 <- tryCatch(rq(value~date, tau=.25, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          obj4 <- tryCatch(rq(value~date, tau=.75, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          obj5 <- tryCatch(rq(value~date, tau=.9, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
          
        }
      }

      res[1+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj0,opt2=F)
      res[2+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj1,opt2=T)
      res[3+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj2,opt2=T)
      res[4+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)
      res[5+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj4)
      res[6+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj5)
      
      res[1+newind,"Scale"] <- mean(inputData$value[ind],na.rm = T)
      res[2+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .05)
      res[3+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .25)
      res[4+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .5)
      res[5+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .75)
      res[6+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .9)
      
      pp <- pp + 1
    }
  }
  res[,"Delta (%)"] <- round(100*(res[,"coef"]*res[,"Delta (%)"])/res[,"Scale"],1)
  return(res)
}


describetrends <- function(inputData, opt1=T, bp=NULL, setwindowSize=120){
  inputData <- inputData[!is.na(inputData$value),]
  inputData <- sample_n(inputData, min(200000,nrow(inputData)))
  
  getinfo <- function(x, opt2=opt1){
    if(typeof(x)=="list"){
      if(opt2){
        out <- tryCatch(c(coefficients(x)[2,1],coefficients(x)[2,2],coefficients(x)[2,3],as.vector(summary(x)$coefficients[2,2])
                          ,as.vector(dchisq(summary(x)$coefficients[2,3],1)),coefficients(x)[1,1] ), error = function(e) c(coefficients(x)[2,1],coefficients(x)[2,2],coefficients(x)[2,3],NA,NA,coefficients(x)[1,1]  ) )
      }else{
        out <- tryCatch(c(as.vector(coefficients(x)[2]),confint(x)[2,1],confint(x)[2,2],summary(x)$coefficients[2,2],dchisq(summary(x)$coefficients[2,3],1),coefficients(x)[1]), error = function(e) rep(NA,6))
      }
    }else{
      out <- rep(NA,6)
    }
    return(out) 
  }
  
  listecol <-  c("bp","datepos","Type","Intercept","coef","LB","UB","Std","p","Scale","Delta (%)")
  
  res <- data.frame(matrix(NA, nrow=ifelse(bp[1]==0,1,2+bp$nbp),ncol=length(listecol) ))
  
  colnames(res) <- listecol
  
  res[,"bp"] <- 0
  # res[,"Type"] <- c("LM","QR05","QR25","QR50","QR75","QR90")
  res[,"Type"] <- c("QR50")
  
  res[1,"datepos"] <- paste0(min(inputData$date,na.rm = T),"//",max(inputData$date,na.rm = T))
  res[1,"Delta (%)"] <- max(inputData$date,na.rm = T)-min(inputData$date,na.rm = T)
  
  # obj0 <- tryCatch(lm(value~date, data=inputData), error = function(e) NA)
  obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData, ci=T), error = function(e) NA)

  
  # res[1,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj0,opt2=F)
  res[1,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)

  
  # res[1,"Scale"] <- mean(inputData$value,na.rm = T)
  res[1,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .5)
  
  if(bp[1]>0){
    followcut <- c(0,bp$place,Inf)
    pp <- 1
    nbregression <- 1
    for(kk in 1:(length(followcut)-1)){
      newind <- pp*nbregression
      res[c(1+newind),"bp"] <- pp
      
      # ind <- which(inputData$date<followcut[kk+1] & inputData$date>followcut[kk])
      # need to fix a marge of 15 days
      up <- max( (as.numeric(followcut[kk+1])-setwindowSize/2),(as.numeric(followcut[kk])+setwindowSize/2),na.rm = T)
      low <- min( (as.numeric(followcut[kk])+setwindowSize/2),up, na.rm=T)
      
      res[c(1+newind),"datepos"] <- paste0(low,"//",up)
      res[c(1:+newind),"Delta (%)"] <- up-low
      
      if(is.na(low) | is.na(up) | up==low){
        # obj0 <- NA
        obj3 <- NA
      }else{
        ind <- which(inputData$date<=up & inputData$date>=low)
        if(length(unique(inputData[ind,"date3"]))<setwindowSize){
          # obj0 <- NA
          obj3 <- NA
        }else{
          # obj0 <- tryCatch(lm(value~date, data=inputData[ind,]), error = function(e) NA, warning=function(w) NA)
          obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
        }
      }
      res[1+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)
      # res[1+newind,"Scale"] <- mean(inputData$value[ind],na.rm = T)
      res[1+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .5)
      pp <- pp + 1
    }
  }
  res[,"Delta (%)"] <- round(100*(res[,"coef"]*res[,"Delta (%)"])/res[,"Scale"],1)
  return(res)
}