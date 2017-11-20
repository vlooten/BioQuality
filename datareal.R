# Connect to I2B2 ####
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre7")
library("RJDBC")
drv <- JDBC("oracle.jdbc.OracleDriver", classPath="./ojdbc14-9i.jar", " ")
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
Listcount$getbio <- 0
Listcount$getbio[1:302] <- 1
write.csv2(Listcount, paste0(rept,"Count.csv"),row.names = F )

# Then, get all exam from the previous list ####
listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])

import <- function(concept){
  # Cette fonction importe les data bio d'i2b2 mais ne renvoie pas le dataset
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