# Connect to I2B2 ####
# This script is an interface between our clinical data warehouse (CDW) and the next functions
# See ReadMe file to adapt the format to your CDW
# Set path and load connection package RJDBC
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre7")
library("RJDBC")
# Oracle Driver of the I2B2 clinical data warehouse (CDW)
drv <- JDBC("oracle.jdbc.OracleDriver", classPath="./ojdbc14-9i.jar", " ")
# Parameters of connection to CDW
con <- dbConnect(drv,"XXXDATABASEXXX", "XXXLOGINXXX", "XXXPASSWORDXXX")

# First, list of all exams of I2B2 ####
# if indexation is TRUE, request already done, just get list of exams
Listcount <- dbGetQuery(con, "SELECT * FROM(SELECT CONCEPT_CD, count(*) C FROM i2b2demodata.OBSERVATION_FACT WHERE SCHEME_KEY='BIO:' AND VALTYPE_CD = 'N' GROUP BY CONCEPT_CD ORDER BY C DESC)")
Namesconcepts <- dbGetQuery(con, "SELECT distinct CONCEPT_CD, NAME_CHAR FROM I2B2DEMODATA.CONCEPT_DIMENSION WHERE SUBSTR(CONCEPT_CD,1,3)='BIO'")
Listcount <- merge(Listcount,Namesconcepts, by="CONCEPT_CD")
Listcount <- Listcount[order(Listcount$C,decreasing = T),]
Listcount$tag1 <- str_match(Listcount$NAME_CHAR, "\\((0[1]\\w+)\\)")
Listcount$tag1 <- gsub("\\(","",Listcount$tag1)
Listcount$tag1 <- gsub("\\)","",Listcount$tag1)
Listcount$tag2 <- str_match(Listcount$NAME_CHAR, "\\((0[2]\\w+)\\)")
Listcount$tag2 <- gsub("\\(","",Listcount$tag2)
Listcount$tag2 <- gsub("\\)","",Listcount$tag2)
# Personnal setting, we get the first 302 results
# Parameter getbio=1 for yes, getbio=0 for no
Listcount$getbio <- 0
Listcount$getbio[1:302] <- 1
write.csv2(Listcount, paste0(rept,"Count.csv"),row.names = F ) # Writing index list in a local csv

# Then, get all exam from the previous list ####
listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])

import <- function(concept){
  # This function imports biology data from the i2b2 data warehouse and stores it in a local csv file
  # Input : "concept" (the concept name of the clinical data warehouse object)
  query = paste("SELECT TO_CHAR(START_DATE, 'YYYY-MM-DD'), to_char(NVAL_NUM ,'9,999.00', 'NLS_NUMERIC_CHARACTERS = '',.''')FROM i2b2demodata.OBSERVATION_FACT WHERE CONCEPT_CD = '",concept,"' AND SCHEME_KEY = 'BIO:'", sep="")
  hb = dbGetQuery(con, query)
  colnames(hb) <- c("date2", "value")
  hb$date2 = ymd(hb$date2)
  hb$date <- as.numeric(hb$date2)
  hb$date3 <- year(hb$date2)+ (month(hb$date2)-1)/12
  hb$value<- as.numeric(gsub("[[:space:]]","",hb$value))
  hb <- hb[!is.na(hb$value),]
  write.csv2(hb,paste0(rept,sub(pattern = ":",replacement = ".",x = concept,perl = F),".csv"),row.names = F)
}

for(kk in listecalc){
  import(kk)
}
