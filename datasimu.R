# List of simulated exams to feed algorithm #
# Necessary for execution : source('./fun_datasimu.R')
# Date of creation : 

# Simulations ####

# Setting of simulations
N <- 200 # Number of exams
namescolumns <- c("CONCEPT_CD","C","NAME_CHAR","getbio","tag0","tag1","tag2","tag3","tag4")
setting <- as.data.frame(matrix(NA,ncol=length(namescolumns), nrow=N))
colnames(setting) <- namescolumns
tag0 <- sample(c(3:10),replace=T, size=N) # Sample size
tag1 <- sample(x=c(0:5),size = N, prob = c(0.4,0.2,0.1,0.1,0.2,0.1), replace = T) # Num of scenario
d2 <- sample(x=seq(as.numeric(as.Date("2003-12-30")),as.numeric(as.Date("2016-01-04"))),replace=T, size=N)
d3 <- sample(x=seq(as.numeric(as.Date("2003-12-30")),as.numeric(as.Date("2016-01-04"))),replace=T, size=N)
tag2 <- as.Date(pmin(d2,d3),origin="1970-01-01") # date breakpoint1
tag3 <- as.Date(pmax(d2,d3),origin="1970-01-01") # date breakpoint2
d4 <- sample(x = c(0,1),size=N,replace=T)
tag4 <- ifelse(d4==1,as.Date("9999-02-01"),tag3)
setting[,"tag0"] <- tag0
setting[,"tag1"] <- tag1
setting[,"tag2"] <- tag2
setting[,"tag3"] <- tag3
setting[,"tag4"] <- tag4
setting[,"tag4"] <- as.Date(setting[,"tag4"],origin="1970-01-01")

for(kk in 1:N){
  simudata <- generationdata(scenario=setting[kk,"tag1"],breakdate=setting[kk,"tag2"] , breakdate2=setting[kk,"tag4"],volume=setting[kk,"tag0"])
  setting[kk,"C"] <- nrow(simudata$data)
  setting[kk,"CONCEPT_CD"] <- simudata$CONCEPT_CD
  setting[kk,"NAME_CHAR"] <- paste0("Simu : ",simudata$CONCEPT_CD)
  write.csv2(simudata$data,paste0(rept,sub(pattern = ":",replacement = ".",x = simudata$CONCEPT_CD,perl = F),".csv"),row.names = F) # Save data
}
setting$getbio <- 0
setting$getbio[which(setting$C>10000)] <- 1
write.csv2(setting,paste0(rept,"Count.csv"),row.names = F) # List of exams

# Create graphs ####
Listcount <- read.csv2(paste0(rept,"Count.csv"))
listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])
for(kk in listecalc){
  print(kk)
  df <- loaddata(kk)
  ggexam <- graphviewreal(df, titre = kk)
  ggsave(filename = paste0(paste0(rept2,"Graphs/"), gsub(x = kk,pattern = ":",replacement = "."),".jpg"),plot = ggexam)
}