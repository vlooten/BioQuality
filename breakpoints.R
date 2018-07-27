# Compute for all exams ####

result <- data.frame(matrix(NA,nrow=length(listecalc), ncol=5))
result[,1] <- as.character(listecalc)

for(kk in 1:length(listecalc) ){
  print (result[kk,1])
  # Load moving median (already computed with movingquantiles.R)
  movingmedian <- loadquant(nombio=result[kk,1])
  movingmedian_NA_filtre <- apply(movingmedian, 1, function(x){any(is.na(x))})
  movingmedian <- movingmedian[!movingmedian_NA_filtre,]
  
  for(qq in 2:nrow(movingmedian)){
    movingmedian[qq,"value"] <-ifelse(is.na(movingmedian[qq,"value"]),
                                      movingmedian[qq-1,"value"],
                                      movingmedian[qq,"value"] )
  }
  if (!all(is.na(movingmedian$value))) {
    # Call the function detectbreakpoint declared in fun_breakpoints.R
    breaksave <- detectbreakpoint(movingmedian)
    result[kk,2] <- breaksave$nbp
    result[kk,3] <- paste0(breaksave$place, collapse = "@")
    result[kk,4] <- ifelse(breaksave$nbp>0,breaksave$maxchange,0)
    result[kk,5] <- ifelse(breaksave$nbp>0,paste0(breaksave$meanlist, collapse = "@"),NA)
    ## Create and save plot with breakpoints (if they exist)
    if(result[kk,2] >0) {
      graphvpm <- graphviewreal(loaddata(result[kk,1]), NN=100000,titre = " ") + geom_vline(data=data.frame(place=breaksave$place),aes(xintercept = place),linetype = "dashed", colour = "red")
      ggsave(graphvpm,filename =paste0(rept2,"breakpoints/graph_bp/", gsub(":",".",result[kk,1]),"_breakpoints.pdf"), dpi = 300 )
    }
  }
}
colnames(result) <- c("exam","NuOfBreak","Dates","MaxChange","MeanList")
# Results
# Exam : name of the exam
# NuOfBreak : number of breakpoint detected
# Dates : date of detected breakpoint
# MaxChange : max change observed
# MeanList : list of means by interval
write.table(result, file = paste0(rept2,"breakpoints/result_breakpoints.csv"), append = FALSE, quote = FALSE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
