# Compute for all exams ####
# Listcount <- read.csv2(paste0(rept,"Count.csv"))
# listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])
result <- data.frame(matrix(NA,nrow=length(listecalc), ncol=5))
result[,1] <- as.character(listecalc)

for(kk in 1:length(listecalc) ){
  print (result[kk,1])
  movingmedian <- loadquant(nombio=result[kk,1])
  movingmedian_NA_filtre <- apply(movingmedian, 1, function(x){any(is.na(x))})
  movingmedian <- movingmedian[!movingmedian_NA_filtre,]
  
  for(qq in 2:nrow(movingmedian)){
    movingmedian[qq,"value"] <-ifelse(is.na(movingmedian[qq,"value"]),
                                      movingmedian[qq-1,"value"],
                                      movingmedian[qq,"value"] )
  }
  if (!all(is.na(movingmedian$value))) {
    breaksave <- detectbreakpoint(movingmedian)
    result[kk,2] <- breaksave$nbp
    result[kk,3] <- paste0(breaksave$place, collapse = "@")
    result[kk,4] <- ifelse(breaksave$nbp>0,breaksave$maxchange,0)
    result[kk,5] <- ifelse(breaksave$nbp>0,paste0(breaksave$meanlist, collapse = "@"),NA)
    ## Créer et sauvegarder un graph avec les breakpoints (s'ils existent)
    if(result[kk,2] >0) {
      graphvpm <- graphviewreal(loaddata(result[kk,1]), NN=100000,titre = " ") + geom_vline(data=data.frame(place=breaksave$place),aes(xintercept = place),linetype = "dashed", colour = "red")
      ggsave(graphvpm,filename =paste0(rept2,"breakpoints/graph_bp/", gsub(":",".",result[kk,1]),"_breakpoints.pdf"), dpi = 300 )
      # graphvpm <- graphviewreal(importrealdata(result[kk,1]), NN=100000,titre = " ") + geom_vline(data=as.data.frame(breaksave),aes(xintercept = place),linetype = "dashed", colour = "red")
    }
  }
}
colnames(result) <- c("exam","NuOfBreak","Dates","MaxChange","MeanList")

write.table(result, file = paste0(rept2,"breakpoints/result_breakpoints.csv"), append = FALSE, quote = FALSE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
