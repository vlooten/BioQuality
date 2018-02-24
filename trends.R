
# Compute trends ####
# Listcount <- read.csv2(paste0(rept,"Count.csv"))
# listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])
scanbreakbio <- read_delim(paste0(rept2,"breakpoints/result_breakpoints.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
result_bp <- data.frame(matrix(NA,nrow=length(listecalc), ncol=4))
result_bp[,1] <- as.character(listecalc)
bonferonni_threshold <- 0.05/nrow(result_bp)
for(kk in listecalc){
  print(paste0(kk, " start"))
  nomfichier <- sub(pattern = ":",replacement = ".",x = kk,perl = F)
  if (!file.exists(paste0(rept,"trendsresult/", nomfichier,"_trends.csv"))){
    dataBio <- loaddata(nomfichier)
    
    list_input_trends <- list(nbp=scanbreakbio[[which(scanbreakbio$exam==kk),c("NuOfBreak")]],place=strsplit(as.character(scanbreakbio[[which(scanbreakbio$exam==kk),c("Dates")]]),split = "@")[[1]])
    if(is.na(list_input_trends$nbp)) {list_input_trends$nbp <- 0 }
    if(is.na(list_input_trends$place)) {list_input_trends$place <- 0 }
    
    result <- describetrends(inputData = dataBio, bp = list_input_trends)
    write.table(result, file = paste0(rept2, "trendsresult/",nomfichier ,"_trends.csv"), append = FALSE, quote = FALSE, sep = ";",
                eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"),
                fileEncoding = "")
    
    ind_p <- min(result[which(result$Type %in% c("LM","QR50") ),"p"], na.rm=T)
    
    result_bp[which(result_bp[,1]==kk),4] <- ind_p
    result_bp[which(result_bp[,1]==kk),2] <- ifelse(is.na(ind_p) | ind_p>bonferonni_threshold,FALSE,TRUE)
    
    if(max(abs(result[,1]), na.rm=T)==0){
      # result_bp[which(result_bp[,1]==kk),3] <- tryCatch(max(result[which(result$Type %in% c("LM","QR50") ),11], na.rm=T), warning=function(w) 0)
      result_bp[which(result_bp[,1]==kk),3] <- tryCatch(max(result[,11], na.rm=T), warning=function(w) 0)
      
    }else{
      # result_bp[which(result_bp[,1]==kk),3] <- tryCatch(max(abs(result[which(result[,1]>0 & result$Type %in% c("LM","QR50") ),11]), na.rm=T), warning=function(w) 0)
      result_bp[which(result_bp[,1]==kk),3] <- tryCatch(max(abs(result[which(result[,1]>0),11]), na.rm=T), warning=function(w) 0)
      
    }
  }
}
colnames(result_bp) <- c("exam","trends","PropVar","pval")
write.table(result_bp, file = paste0(rept2, "trendsresult/results_trends.csv"), append = FALSE, quote = FALSE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")



# library("epiR")
# epi.simplesize(N = 1E+06, Vsq = NA, Py = 0.5, epsilon.r = 0.08,
#                method = "proportion", conf.level = 0.95)
