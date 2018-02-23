# Fonctions :
# JumpingDensity : calcul du nombre d'examens par fenetre successives et non superposées [Necessaire]
# detect_missing : application de JumpingDensity [Necessaire]
# MovingDensity : calcul du nombre d'examens par fenetre glissante [facultatif, choix ancien]

JumpingDensity <- function(inputData=NA, windowSize = 60, name_data="BIO", borne_ratio=0)
{
  inputData <- inputData[which(!is.na(inputData$value)),]
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
  for (i in seq(min(inputData$date),max(inputData$date),by = windowSize)) {
    startTmp <- i
    endTmp <- i+windowSize
    startT[j] <- startTmp
    endT[j] <- endTmp
    indice <- which(inputData$date>=startTmp & inputData$date<=endTmp)
    outData[j] <- length(indice)
    j <- j+1
  }
  out <- data.frame(startT, endT, outData)
  return(out)
}

detect_missing <- function (df = NA, nomfichier = "BIO.1000", rept_save = paste0(rept2,"missingdata/graph_md/")){
  # borne : nombre d'examen minimum pour considérer que la fenetre n'est pas vide
  # nomfichier : nom de l'examen bio, utilisé uniquement pour la sauvegarde
  # dans l'article on utilise 1
  
  # Test
  # nomfichier <- "BIO.1015"
  # df <- loaddata("BIO:1015")
  
  print(paste0("Start:", nomfichier) )
  
  dataseg <- df %>% group_by(date) %>% summarise(value=length(value))
  # Completion
  dataseg0 <- as.data.frame(cbind(date=seq(min(dataseg$date, na.rm=T),max(dataseg$date, na.rm=T),1),value=0),stringsAsFactors = F)
  dataseg0 <- dataseg0[which(!(dataseg0$date %in% dataseg$date) ),]
  dataseg <- rbind(dataseg,dataseg0)
  dataseg <- dataseg[order(dataseg$date, decreasing = F),]
  #
  data_dec <- tryCatch(decompose(ts(dataseg$value, frequency = 365 )), error=function(e) NULL)
  if(!is.null(data_dec)){
    
    dataseg2 <- as.data.frame(cbind(value=data_dec$trend,start=dataseg$date, end=NA))
    dataseg2 <- dataseg2[!is.na(dataseg2$value),]
    out.lm <- tryCatch(lm(value~start, data=dataseg2),
                       error=function(e) NULL)
    if(is.null(out.lm)){
      return(NULL)
    }else{
      segFit <- tryCatch(segmented(out.lm,seg.Z=~start,psi=list(start=NA),
                                   control=seg.control(display=FALSE, K=10, stop.if.error=FALSE, n.boot=0, it.max=500, toll = 1e-08)),
                         error=function(e) NA)
      times_seg <- tryCatch(confint.segmented(segFit), error=function(e) NULL) # If no breakpoint
      if(!is.null(times_seg)){
        
        x <- sign(slope(segFit)$start[,1])
        if(length(x)>1){
          y <- names(x)[paste0(c(0,x[-length(x)]),x) %in% "-11"]
          y <- paste0("psi",as.numeric(gsub("slope","",y))-1,".start")
          localb <- print(segFit)
          localb <- localb$psi[rownames(localb$psi) %in% y,2]
        }
        missingData <- as.data.frame(cbind(namefile=nomfichier,startT_MD=round(as.numeric(localb)), endT_MD=NA), stringsAsFactors = F)
        missingData$startT_MD <- as.numeric(missingData$startT_MD)
        return(missingData)
      }else{
        return(NULL)
      }
    }
  }else{
    return(NULL)
  }
}

