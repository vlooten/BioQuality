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

detect_missing <- function (df = NA, borne =5 , nomfichier = "BIO.1000", rept_save = paste0(rept2,"missingdata/graph_md/")){
  # borne : nombre d'examen minimum pour considérer que la fenetre n'est pas vide
  # nomfichier : nom de l'examen bio, utilisé uniquement pour la sauvegarde
  # dans l'article on utilise 1
  
  print(paste0("Start:", nomfichier) )
  nomfichier <- make.names(nomfichier)
  res <- JumpingDensity(inputData = df, windowSize = 60)
  res$startT <- as.Date(res$startT, origin="1970-01-01")
  res$endT <- as.Date(res$endT, origin="1970-01-01")
  
  startT_MD <- vector()
  it_startT_MD <- 1
  endT_MD <- vector()
  it_endT_MD <- 1 
  
  for (i in 1:length(res$startT))
  {
    if(res[i,]$outData < borne) # marquer MD
    {
      if (i == 1 ) ## Première fenêtre est MD
      {
        startT_MD[it_startT_MD] <- res[i,]$startT
        it_startT_MD <- it_startT_MD +1
      }else{
        if (res[i-1,]$outData < borne) ## la fenêtre précédente, est déjà MD
        {
          ### Ne rien faire 
          print("Still missing.")
          
        }else{
          ### la fenêtre précédente n'est pas MD
          startT_MD[it_startT_MD] <- res[i,]$startT
          it_startT_MD <- it_startT_MD +1
        }
      }
    }else{ ### Non MD
      if(i != 1){
        if (res[i-1,]$outData < borne) ## La fenêtre précédente est MD
        {
          endT_MD[it_endT_MD] <- res[i,]$startT
          it_endT_MD <- it_endT_MD + 1
        }
      }
      
    }
  }
  
  if (i == length(res$startT) &&  it_endT_MD != it_startT_MD)
  {
    endT_MD[it_endT_MD] <- res[i,]$endT
    it_endT_MD <- it_endT_MD + 1
  }
  
  missingData_date <- data.frame(startT_MD, endT_MD)
  
  # Save graph with start and end date for missing data
  if(length(missingData_date[,1]) > 0 ) 
  {
    missingData <- cbind(nomfichier, missingData_date)
    graph_data <- graphviewreal(df, NN=200000,titre = paste0(nomfichier,": Detection of missing data") ) + geom_vline(data= missingData_date ,aes(xintercept = startT_MD),linetype = "dashed", colour = "skyblue") + geom_vline(data= missingData_date ,aes(xintercept = endT_MD),linetype = "dashed", colour = "blue")
    ggsave(plot = graph_data,filename =paste0(rept_save,nomfichier,"_missingdata.jpg"), dpi = 300,device = "jpg" )
  }else{
    missingData <- data.frame(matrix(NA, nrow=0, ncol=3))
    colnames(missingData) <- c("namefile", "startT_MD", "endT_MD")
  }
  
  return(missingData)
}