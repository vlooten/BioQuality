result <- data.frame(matrix(NA, ncol=3,nrow=length(listecalc)))
table_chgmnt <-as.data.frame(NULL,ncol=3,nrow=0)
windowSize <- 60
# colnames(table_chgmnt) <- c("exam","startT","cosine")
for(kk in 1:length(listecalc)){
  print(listecalc[kk])
  result_tmp <- discretization_change(listecalc[kk],window_size = 60)
  result[kk,"exam"] <- listecalc[kk]
  result[kk,"discritization"] <- result_tmp$discretization
  result[kk,"numberofdetections"] <- ifelse(result_tmp$discretization==T,nrow(result_tmp$timeline_chgmnt),0)
  if(result_tmp$discretization==T){
    table_chgmnt <- bind_rows(table_chgmnt,cbind(exam=listecalc[kk],result_tmp$timeline_chgmnt))
  }
}
colnames(result) <- c("exam","discritization","numberofdetections")
write.table(result, file = paste0(rept2,"discretisation/" ,"discretization_results.csv"), append = FALSE, quote = FALSE, sep = ";",  eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
write.table(table_chgmnt, file = paste0(rept2,"discretisation/" ,"discretization_times.csv"), append = FALSE, quote = FALSE, sep = ";",  eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
