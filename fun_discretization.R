
# Step3b : Discretization ####

# Liste des fonctions :
# substrRight : Fonction pour récupérer les n digits finaux 
# get_dec : Fonction pour définir les classes de nb_digit
# Avec poids : [ Ce qui a été utilisé dans l'article]
# count_last_discretisation_weight
# MovingCountLast_weight[nécessite count_last_discretisation_weight]


# FONCTION SUBSTRRIGHT ####
### Fonction pour récupérer les n digits finaux 
### x : vector de nombres , n : n digit finaux à récupérer (par défaut, 1)
### Exemple : 105 -> 5 ; 10 -> 0  ; 1 -> 1 ; 10.5 -> 5
substrRight <- function (x=NA,n=1)
{
  substr(x, nchar(x)-n+1, nchar(x))
  
}

# FONCTION GET_DEC ####
#### Fonction pour définir les classes de nb_digit
#### ATTENTION, on donne que des NOMBRES au sens de R (donc pas de ',' seulement des '.')
#### num : nombre ou vecteur de nombres
#### Exemple : 10 -> int_2 ; 100->int_3 ; 100.5 -> 1 ; 100.05 -> 2
get_dec<- function (num)
{
  if(grepl("\\.", num, perl=FALSE, fixed= FALSE))
  {
    return(nchar(gsub(".+\\.", "", num)))
  }else
  {
    return(paste0("int_",nchar(num)))
  }
}

# FONCTION COUNT_LAST_DISCRETISATION_WEIGHT ####
### input : data (juste les valeurs), name_date (nom du concept étudié), borne_min_missing_data (entier : nombre à partir duquel on considère qu'il y a suffisament de données pour appliquer l'algo)
### output : data frame ### col : name_data, c_nb_digit_final, c_last_digit_final, c_ratio_final, c_disretisation_test_final
count_last_discretisation_weight <-  function(data = NA, name_data = "BIO" ,borne_min_missing_data = 5000, borne_ratio=0)
{    
  data <- gsub(pattern = ",",replacement = ".",x = data ,perl = F,fixed=T)
  clist <- data
  df_basebio_dec_last <- data.frame(matrix(NA, nrow= length(clist), ncol=2))
  ####### Traitement
  df_basebio_dec_last[,2] <- sapply(clist , substrRight) ## Last digit
  df_basebio_dec_last[,1] <- sapply(clist, get_dec)  ## nb digit
  
  ####### Table de comptage de freq pour nb_digit
  table2 <- as.data.frame(table(df_basebio_dec_last[,1]))
  ####### Data frame de nb_digit : nb_digit , nb_val avec ce nombre de digit, ratio dans le graphe
  df_table_ratio <- data.frame(matrix(NA, nrow = length(table2[["Freq"]]), ncol=3))
  colnames(df_table_ratio) <- c("nb_digit", "nb_val", "ratio")
  df_table_ratio[,1] <- table2[,1]
  df_table_ratio[,2] <- table2[,2]
  df_table_ratio[,3] <- 100*table2[,2]/length(data)
  
  ####### Table de comptage pour le couple (nb_digit , last_digit)
  tableclast <- as.data.frame(table(df_basebio_dec_last))
  ####### Data frame de (nb_digit,last_digit) :
  df_table_ratio_last <- data.frame(matrix(NA, nrow = length(tableclast[["Freq"]]), ncol=5))
  colnames(df_table_ratio_last) <-  c("nb_digit", "last_digit", "freq_last_digit","freq_nb_digit","ratio" )
  df_table_ratio_last[,1] <- tableclast[,1]
  df_table_ratio_last[,2] <- tableclast[,2]
  df_table_ratio_last[,3] <- tableclast[,3]
  
  
  for (j in 1:length(tableclast[["Freq"]])) 
  {
    df_table_ratio_last[j,4] <- df_table_ratio[df_table_ratio$nb_digit==df_table_ratio_last[j,1],]$nb_val
    df_table_ratio_last[j,5] <- (100*tableclast[j,3]/df_table_ratio_last[j,4])*df_table_ratio[df_table_ratio$nb_digit==df_table_ratio_last[j,1],]$ratio
  }
  
  
  nb_digit_maj <-df_table_ratio[df_table_ratio$nb_val>borne_min_missing_data,]$nb_digit
  # nb_digit_maj : nombre de classes
  
  ##### Initialisation : Ces valeurs seront utilisées si les données (ou portion de données) sont perçues comme non discrétisées
  c_nb_digit_final <- "NA"
  c_last_digit_final <- "NA"
  c_ratio_final <- "NA"
  c_disretisation_test_final <- FALSE   
  ##### 
  
  if (length(nb_digit_maj) >0 ) { # CONDITIONS POUR EVITER LES NA
    if (!any(is.na(nb_digit_maj))) # DEBUG
    {
      #### Selection des classes à sortir (borne_ratio indique à partir de quel ratio une classe indique une discrétisation (à 0, on choisit de sortir toutes les données))
      new_row_base <- subset(x=subset(x = df_table_ratio_last, nb_digit %in% nb_digit_maj), ratio>=borne_ratio)
      if (length(new_row_base[,1])>0)
      {
        c_nb_digit_final <- paste0(new_row_base$nb_digit, collapse = "@")
        c_last_digit_final <- paste0(new_row_base$last_digit, collapse = "@")
        c_ratio_final <- paste0(new_row_base$ratio, collapse = "@")
        c_disretisation_test_final <- TRUE
      }
    }
  }  
  ### Assemblage de l'output
  c_final <- paste0( c(name_data, c_nb_digit_final, c_last_digit_final, c_ratio_final, c_disretisation_test_final), collapse = ";")
  # name_data, NOM DE L'EXAM
  # c_nb_digit_final : CLASSE
  # c_last_digit_final : DERNIER DIGIT
  # c_ratio_final : RATIO
  # c_disretisation_test_final : OUI/NON
  # Detect discret si discretization en considérant le parametre borne_ratio
  # difference avec la fonction sans le poids : calcul de df_table_ratio_last
  return(c_final)  
}

# FONCTION MOVINGCOUNTLAST_WEIGHT ####
MovingCountLast_weight <- function(inputData=NA, windowSize = 60, borne_min_exam=0 , name_data="BIO", borne_ratio=0)
{
  
  if (is.null(inputData)) {
    cat("WARNING: Please input a valid inputData\n")
    return(NULL)
  }
  if (windowSize <= 1) {
    cat("WARNING: no filtering performed: outData = inputData\n")
    return(inputData)
  }
  
  # Need to remove NA before the treatment
  inputData <- inputData[which(!is.na(inputData$value)),]
  warning("NA were removed before computing")
  
  outData <- vector()
  startT <- vector()
  endT <- vector()
  j <- 1
  ## Fenêtre sautante !
  for (i in seq(min(inputData$date),max(inputData$date),by = windowSize)) {
    
    startTmp <- i
    endTmp <- i+windowSize
    startT[j] <- startTmp
    endT[j] <- endTmp
    indice <- which(inputData$date>=startTmp & inputData$date<=endTmp)
    
    ## Calcul (application de fonction)
    if(length(indice)<=borne_min_exam){ ### si on veut qu'une fenêtre soit ignorée car elle n'a pas assez de données
      outData[j] <- NA
    }else{
      ## Application de la fonction
      outData[j] <- count_last_discretisation_weight(data=inputData$value[indice], name_data = name_data, borne_min_missing_data = 50 , borne_ratio = borne_ratio)
    }
    j <- j+1
  }
  
  ### Fin fenêtre
  ### Assemblage de l'output
  out <- data.frame(startT, endT, outData)
  return(out)
}

# Fonction finale de detection des changements ####

discretization_change <- function(dt_name, target_cosine=50 ,graph_output=paste0(rept2,"discretisation/graph_discret_w/"),window_size = 60){
  
  nb_concept_ac_chgmt <- 0 # itérateur
  nb_rows_timeline_chgmt <- 1 # itérateur
  nomfichier <- make.names(dt_name)
  basebio1 <- loaddata(dt_name)
  basebio1["value"] <- gsub(pattern = ",",replacement = ".",x = basebio1[["value"]] ,perl = F,fixed=T)
  basebio1["value"] <- as.numeric(basebio1[["value"]])
  
  # Compute XXXXXXXXXXXXXXXXXX 
  d <- MovingCountLast_weight(inputData = basebio1, windowSize = window_size, borne_ratio=0)
  
  # Traitment of output
  test <- strsplit(as.character(d[["outData"]]),';', fixed = TRUE)
  outty <- data.frame(matrix(NA, nrow = length(test), ncol = 5))
  
  for(i in 1:length(test))
  {
    ctest <- test[i]
    outty[i,] <- ctest[[1]]
  }
  
  ####### Vecteurs d'informations sur les changements de discrétisation au cours du temps sur le BIO concerné
  list_startT <- vector()
  list_cosine <- vector()
  ################################################### POUR CHAQUE PAIRES DE FENETRES i ET i+1
  if (length(d[,1])-1 > 0){
    for (ii in 1:(length(d[,1])-1))
    {
      ##################### Extraction des informations relatives à deux fenêtres consécutives
      # window_i  : nb_digit, last_digit, ratio
      window_1_temp <- strsplit(outty[[ii,2]], "@", fixed = TRUE)
      window_1 <- data.frame(matrix(NA, nrow= length(window_1_temp[[1]]), ncol = 3)  )
      colnames(window_1) <- c("nb_digit", "last_digit", "ratio")
      window_1[,1] <- window_1_temp[[1]]
      window_1[,2] <- strsplit(outty[[ii,3]], "@", fixed = TRUE)
      window_1[,3] <- strsplit(outty[[ii,4]], "@", fixed = TRUE)
      
      window_2 <- strsplit(outty[[ii+1,2]], "@", fixed = TRUE)
      window_2_temp <- strsplit(outty[[ii+1,2]], "@", fixed = TRUE)
      window_2 <- data.frame(matrix(NA, nrow= length(window_2_temp[[1]]), ncol = 3)  )
      colnames(window_2) <- c("nb_digit", "last_digit", "ratio")
      window_2[,1] <- window_2_temp[[1]]
      window_2[,2] <- strsplit(outty[[ii+1,3]], "@", fixed = TRUE)
      window_2[,3] <- strsplit(outty[[ii+1,4]], "@", fixed = TRUE)
      
      w1 <- tidyr::unite(window_1, classe ,nb_digit, last_digit, sep=";")
      w2 <- tidyr::unite(window_2, classe ,nb_digit, last_digit, sep=";")
      
      w <- dplyr::full_join(w1,w2, by="classe")
      w[is.na(w)] <- 0
      
      if(length(w[w$ratio.x=="NA",]$ratio.x) !=0)
      {w[w$ratio.x=="NA",]$ratio.x <- 0}
      
      if(length(w[w$ratio.y=="NA",]$ratio.y) !=0)
      {w[w$ratio.y=="NA",]$ratio.y <- 0}
      
      
      list_startT[ii] <- d[ii,2]
      list_cosine[ii] <- cosine(as.numeric(w$ratio.x), as.numeric(w$ratio.y))
      if(list_cosine[ii] =="NaN")
      {
        list_cosine[ii] <- 1
      }
    }
    
    ##### Création du data frame final (on utilise des pourcentages pour les cosines)
    data_final_discre_base_date <- data.frame(list_startT, list_cosine*100)
    data_final_discre <- data.frame(as.Date(list_startT, origin="1970-01-01"), list_cosine*100)
    colnames(data_final_discre) <- c("startT", "cosine")
    colnames(data_final_discre_base_date) <-  c("startT", "cosine")
    
    # Change detect if target_cosine
    dates_discre <- data_final_discre_base_date[data_final_discre_base_date$cosine <= target_cosine,]
    
    ####### TODO : petite fonction pour fusionner les fenêtres adjacentes discrétisées (prendre la plus vieille (donc la première))
    # SUPER IMPORTANT : quand plusieurs changements successifs ne compter qu'un seul changement
    
    dates <- dates_discre["startT"]
    ####### Ne fonctionne que pour UNE fenêtre de distance (120 jours, ça fait déjà beaucoup)
    if (length(dates_discre[,1]) > 1)
    {
      for (xx in 1:(length(dates_discre[,1])-1) )
      {
        if (dates[xx+1,]==(dates[xx,]+windowSize  ))
        {
          dates_discre <- dates_discre[dates[xx+1,]!=dates_discre$startT,]
        }
      }
    }
    ### If Discretization detected
    if(length(dates_discre[,1])>0){
      # Graphic output
      dates_discre <- as.data.frame(dates_discre)
      mini_date <- as.Date(min(basebio1$date,na.rm=T), origin="1970-01-01")
      maxi_date <- as.Date(max(basebio1$date,na.rm=T), origin="1970-01-01")
      graph_data_lines <- graphviewreal(basebio1, NN=200000,titre = " ") + xlim(mini_date,maxi_date) + geom_vline(data=as.data.frame(dates_discre),aes(xintercept = startT),linetype = "dashed", colour = "blue")
      graph_line_consine <- ggplot(data_final_discre, aes(x = startT, y = cosine))  +  theme_bw() +  geom_line() + theme(axis.title.y = element_blank(), axis.title.x = element_blank()) + xlim(mini_date,maxi_date) + ylim(c(0,100))
      graph_data <- graphviewreal(basebio1, NN=200000,titre = " ") + xlim(mini_date,maxi_date)
      g <- arrangeGrob(graph_line_consine, graph_data_lines, nrow=2 , ncol=1) #generates g
      ggsave(file=paste0(graph_output, nomfichier,".pdf"), g, dpi = 300) #saves g
      
      return(list(discretization=TRUE,timeline_chgmnt=dates_discre, datacosine=data_final_discre))
    }else{
      return(list(discretization=FALSE, datacosine=data_final_discre))
      
    }

  }else{
    warning(paste0("Not enough data for discritization detection with the window size ",window_size))
    return(list(discretization=FALSE))
  }
  
}

