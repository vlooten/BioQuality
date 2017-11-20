loadquant <- function(nombio,type="Median", directory=paste0(rept2,"MovingQuantiles/") ){
  # Cette fonciton permet de charger les quantiles mobiles
  kk <- nombio
  nomfichier <- sub(pattern = ":",replacement = ".",x = nombio,perl = F)
  if(file.exists(paste0(directory,nomfichier,"/",nomfichier,".",type,".csv"))){
    movingquantile <- read.csv2(paste0(directory,nomfichier,"/",nomfichier,".",type,".csv") )
    return(movingquantile)
  }else{
    stop("Compute moving quantiles before breakpoint detection")
  }

}

# Fonction de detection des breakpoints ####
detectbreakpoint <- function(inputData){
  # Input : moving quantile !!
  ansmean2 <- cpt.mean(inputData$value, method="PELT")
  nbp <- ncpts(ansmean2)
  place <- inputData$start[cpts(ansmean2)]
  #
  if(nbp>0){
    toint <- c(1,cpts(ansmean2),nrow(inputData))
    tocalc <- length(toint)-1
    meanlist   <- rep(NA,tocalc)
    for(i in 1:tocalc){
      meanlist[i] <- mean(inputData$value[toint[i]:toint[i+1]], na.rm = T)
    }
    meanlist <- meanlist[which(!is.na(meanlist))]
    proplist <- rep(NA, length(meanlist)-1)
    if(length(meanlist)>1){
      for(j in 1:(length(meanlist)-1) ){
        proplist[j] <- round(100*(meanlist[j]-meanlist[j+1])/meanlist[j],1)
      }
    }else{
      proplist <- 0
    }
  }else{
    proplist <- NA
    meanlist <- NA
    maxchange <- NA
  }
  
  return(list(nbp=nbp, place=place,meanlist=meanlist,maxchange=max(proplist, na.rm=T)))
  
}


# movingmedian <- loadquant(nombio="BIO:20170821145604")
# movingmedian_NA_filtre <- apply(movingmedian, 1, function(x){any(is.na(x))})
# movingmedian <- movingmedian[!movingmedian_NA_filtre,]
# 
# for(qq in 2:nrow(movingmedian)){
#   movingmedian[qq,"value"] <-ifelse(is.na(movingmedian[qq,"value"]),
#                                     movingmedian[qq-1,"value"],
#                                     movingmedian[qq,"value"] )
# }
# detectbreakpoint(movingmedian)
# 
# inputData <- movingmedian
# ansmean2 <- cpt.mean(inputData$value, method="PELT")
# nbp <- ncpts(ansmean2)
# place <- inputData$start[cpts(ansmean2)]
# 
# c(1,cpts(ansmean2),nrow(inputData))
# mean()