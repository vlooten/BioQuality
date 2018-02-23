source('./init.R')
creatfig <- function(x,y){
  name_exam <- x
  title_exam <- y
  basebio1 <- loaddata(name_exam)
  basebio1 <- basebio1[which(!is.na(basebio1$value)),]
  idx <- findInterval(basebio1$value,quantile(basebio1$value, probs = c(0.01,0.99)),rightmost.closed = T) %in% 1
  result_exam <- result[which(result$CONCEPT_CD==name_exam),]
  
  # Generate graph of data
  graph_data_lines <- graphviewreal(basebio1[idx,], NN=200000,titre = title_exam) + xlab("Time")+ 
    ylab("Values")
  # Add date lines detections
  exam_breakpoints <- result_breakpoints[which(result_breakpoints$exam==name_exam),]
  exam_discretization <- discretization_times[which(discretization_times$exam==name_exam),]
  dates_bp <- tryCatch(as.numeric(unlist(strsplit(exam_breakpoints$Dates,split = "@"))), error = function(e) NA)
  dates_discret <- ifelse(nrow(exam_discretization)==0,NA,exam_discretization$startT)
  # Trends
  if(result_exam$trends==TRUE & result_exam$PropVar>0){
    coeffs_trends <- read.csv2(paste0(rept2,
                                      "trendsresult/",gsub(":",".",name_exam,fixed = T),"_trends.csv"), stringsAsFactors = FALSE )
    
    if(nrow(coeffs_trends)==1){
      direct <- as.numeric(coeffs_trends$coef)
      inter <- as.numeric(coeffs_trends$Intercept)
      graph_data_lines <- graph_data_lines + geom_abline(aes(intercept = inter,slope=direct), colour="#9DD7B1")
      
    }else{
      for(u in 2:nrow(coeffs_trends)){
        if(!is.na(coeffs_trends$coef[u]) & !is.na(coeffs_trends$Intercept[u])  ){
          bornes <- as.numeric(unlist(str_split(coeffs_trends$datepos[u], pattern = "//")))
          bornes[1] <- max(bornes[1],min(basebio1$date[idx]), na.rm=T)
          bornes[2] <- ifelse(bornes[2]==Inf,max(basebio1$date,na.rm=T),bornes[2])
          direct <- as.numeric(coeffs_trends$coef[u])
          inter <- as.numeric(coeffs_trends$Intercept[u])
          graph_data_lines <- graph_data_lines + geom_abline(aes(intercept = inter,slope=direct), colour="#9DD7B1")
        }
      }
    }
  }
  
  #
  exam_missingdata <- missingdata_list[which(gsub(".",":", fixed=T, missingdata_list$namefile)==name_exam),]
  # add bp lines
  if(!all(is.na(dates_bp))){
    graph_data_lines <- graph_data_lines + geom_vline(data=data.frame(bp=unique(dates_bp)),aes(xintercept = bp),linetype = "dashed", colour = "blue")
  }
  # add change of distrib lines
  if(!all(is.na(dates_discret))){
    graph_data_lines <- graph_data_lines + geom_vline(data=data.frame(discret=unique(dates_discret)),aes(xintercept = discret),linetype = "dashed", colour = "#fb739c")
    
  }
  # add missing 
  if(nrow(exam_missingdata)>0){
    graph_data_lines <- graph_data_lines + geom_vline(data=data.frame(discret=unique(exam_missingdata$startT_MD)),aes(xintercept=discret),linetype = "dashed", colour = "#f40909")
  }
  return(graph_data_lines)
}


result_breakpoints <- read_delim(paste0(rept2,"breakpoints/result_breakpoints.csv"), 
                                 ";", escape_double = FALSE, col_types = cols(Dates = col_character()), 
                                 trim_ws = TRUE)
result_breakpoints <- result_breakpoints[which(result_breakpoints$exam %in% Listcount$CONCEPT_CD),]


discretization_results <- read_delim(paste0(rept2,"discretisation/discretization_results.csv"), 
                                     ";", escape_double = FALSE, col_types = cols(discritization = col_skip(), 
                                                                                  exam = col_skip(), numberofdetections = col_skip()), 
                                     trim_ws = TRUE)
colnames(discretization_results) <- c("exam","discretization","NuOfDiscret")
discretization_results <- discretization_results[which(discretization_results$exam %in% Listcount$CONCEPT_CD),]


discretization_times <- read_delim(paste0(rept2,"discretisation/discretization_times.csv"), 
                                   ";", escape_double = FALSE, trim_ws = TRUE)
discretization_times <- discretization_times[which(discretization_times$exam %in% Listcount$CONCEPT_CD),]

missingdata_list <- read_delim(paste0(rept2,"missingdata/missingdata_list.csv"), 
                               ";", escape_double = FALSE, trim_ws = TRUE)

missingdata_list <- missingdata_list[which(gsub(".",":", missingdata_list$namefile,fixed = T) %in% Listcount$CONCEPT_CD),]


results_trends <- read_delim(paste0(rept2,"trendsresult/results_trends.csv"), 
                             ";", escape_double = FALSE, trim_ws = TRUE)
results_trends <- results_trends[which(results_trends$exam %in% Listcount$CONCEPT_CD),]


# Transform missing data table ####
listemd <- gsub(".",":",unique(missingdata_list$namefile),fixed = TRUE)
result <- left_join(Listcount, result_breakpoints, by=c("CONCEPT_CD"="exam"))
result <- left_join(result, discretization_results, by=c("CONCEPT_CD"="exam"))
result <- left_join(result, results_trends, by=c("CONCEPT_CD"="exam"))
result$breakpoint <- result$NuOfBreak>0
result$missingdata <- FALSE
result$missingdata[which(result$CONCEPT_CD %in% listemd)] <- TRUE


# Figure globale ####

liste_bp_dates <- unlist(str_split(result_breakpoints$Dates,pattern = "@"))
liste_bp_dates <- as.numeric(liste_bp_dates[!is.na(liste_bp_dates)])

list_dates_detect <- c(discretization_times$startT,
                       missingdata_list$startT_MD,
                       liste_bp_dates)

numberofchange <- bind_rows(data.frame(date=discretization_times$startT,category="discretization", stringsAsFactors = F),data.frame(date=missingdata_list$startT_MD,category="missingdata",stringsAsFactors = F), data.frame(date=liste_bp_dates,category="breakpoints", stringsAsFactors = F))
numberofchange$Detection <- as.factor(numberofchange$category)
levels(numberofchange$Detection) <- c("Breakpoints", "Discretization", "Missing data")

numevents <- ggplot(data = numberofchange,aes(x=as.Date(date), fill=Detection)) + geom_histogram(binwidth = 240) + facet_grid(category~., scales="free_y") +  xlab("Time")+ ylab("Number of events")
