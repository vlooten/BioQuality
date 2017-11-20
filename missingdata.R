
# Date of creation : 

# Load list of exams ####

# Listcount <- read.csv2(paste0(rept,"Count.csv"))
# listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])

# Detect missing data ####

df_final <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(df_final) <- c("namefile", "startT_MD", "endT_MD")
kk <- ""
for(kk in listecalc)
{
    df <- loaddata(kk)
    result <- detect_missing(df, 1, kk, rept_save = paste0(rept2,"missingdata/graph_md/"))
    df_final <- rbind(df_final, result)
}

write.table(df_final, file = paste0(rept2, "missingdata/missingdata_list.csv"), append = FALSE, quote = FALSE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

rm(list=c("df_final","kk","result"))
