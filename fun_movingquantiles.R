# Function ####

MovingQuantile2D <- function (inputData, windowSize = 60, prob=0.5, optmean=F , minrequired=0) 
{
  if (is.null(inputData)) {
    cat("WARNING: Please input a valid inputData\n")
    return(NULL)
  }
  if (windowSize <= 1) {
    cat("WARNING: no filtering performed: outData = inputData\n")
    return(inputData)
  }
  outData <- vector()
  startT <- vector()
  endT <- vector()
  j <- 1
  ## Fenêtre glissante
  for (i in min(inputData$date):max(inputData$date)) {
    if (windowSize/2 == floor(windowSize/2)) ##si paire
    {
      startTmp <- max(min(inputData$date), i - windowSize/2)
      endTmp <- min(max(inputData$date), i + windowSize/2 - 1)
    }
    else {
      startTmp <- max(min(inputData$date), i - (windowSize - 1)/2)
      endTmp <- min(max(inputData$date), i + (windowSize - 1)/2)
    }
    startT[j] <- startTmp
    endT[j] <- endTmp
    indice <- which(inputData$date>=startTmp & inputData$date<=endTmp)
    
    ## Calcul (application de fonction)
    if(length(indice)<=minrequired){
      outData[j] <- NA
    }else{
      if(optmean){
        outData[j] <- mean(inputData$value[indice], na.rm=T)
      }else{
        outData[j] <- quantile(inputData$value[indice], na.rm=T,probs=prob )
      }
    }
    j <- j+1
  }
  
  ### Fin fenêtre 
  
  out <- as.data.frame(cbind(value=outData, start=startT, end=endT))
  return(out)
}
