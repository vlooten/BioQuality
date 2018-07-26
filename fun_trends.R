# Trend detection function ####
describetrends <- function(inputData, opt1=T, bp=NULL, setwindowSize=120){
  # This function performs quantile regression
  # Inputs : data in inputData argument, breakpoints position in bp argument, minimal interval size determined bu setwindowSize argument
  # If bp is NULL, the quantile regression is performed on whole data
  # If there are bp, quantile regressions are performed by interval
  
  # Remove the NA values
  inputData <- inputData[!is.na(inputData$value),]
  # Sample to adapt infrastructure computationnal performance
  inputData <- sample_n(inputData, min(200000,nrow(inputData)))
  
  # This functions gets results from quantile regression (local declaration)
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
  # Create dataframe for results output
  listecol <-  c("bp","datepos","Type","Intercept","coef","LB","UB","Std","p","Scale","Delta (%)")
  res <- data.frame(matrix(NA, nrow=ifelse(bp[1]==0,1,2+bp$nbp),ncol=length(listecol) ))
  colnames(res) <- listecol
  
  # If no breakpoint, regression on the whole data
  res[,"bp"] <- 0
  res[,"Type"] <- c("QR50") # Regression on the median
  res[1,"datepos"] <- paste0(min(inputData$date,na.rm = T),"//",max(inputData$date,na.rm = T))
  res[1,"Delta (%)"] <- max(inputData$date,na.rm = T)-min(inputData$date,na.rm = T)
  # Regression on the median : parameter tau sets to .5
  obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData, ci=T), error = function(e) NA)
  res[1,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)
  # Get the global median
  res[1,"Scale"] <- quantile(inputData$value,na.rm = T, probs = .5)
  
  # If breakpoints, regression by interval
  if(bp[1]>0){
    followcut <- c(0,bp$place,Inf)
    pp <- 1
    nbregression <- 1
    for(kk in 1:(length(followcut)-1)){
      newind <- pp*nbregression
      res[c(1+newind),"bp"] <- pp
      
      # need to fix a marge of x days fixed by the argument setwindowSize/2 (problem of interval without data)
      up <- max( (as.numeric(followcut[kk+1])-setwindowSize/2),(as.numeric(followcut[kk])+setwindowSize/2),na.rm = T)
      low <- min( (as.numeric(followcut[kk])+setwindowSize/2),up, na.rm=T)      
      res[c(1+newind),"datepos"] <- paste0(low,"//",up)
      res[c(1:+newind),"Delta (%)"] <- up-low
      
      if(is.na(low) | is.na(up) | up==low){
        obj3 <- NA
      }else{
        ind <- which(inputData$date<=up & inputData$date>=low)
        if(length(unique(inputData[ind,"date3"]))<setwindowSize){
          obj3 <- NA
        }else{
          # Regression on the median : parameter tau sets to .5
          obj3 <- tryCatch(rq(value~date, tau=.5, data=inputData[ind,], ci=T), error = function(e) NA, warning=function(w) NA)
        }
      }
      res[1+newind,c("coef","LB","UB","Std","p","Intercept")] <- getinfo(obj3)
      # Get the global median
      res[1+newind,"Scale"] <- quantile(inputData$value[ind],na.rm = T, probs = .5)
      pp <- pp + 1
    }
  }
  # Compute variation proportion by interval (taking into account the global median)
  res[,"Delta (%)"] <- round(100*(res[,"coef"]*res[,"Delta (%)"])/res[,"Scale"],1)
  return(res)
}
