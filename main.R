# Directory of the project

# Init directories and packages ####
source('./init.R')

# Import data ####
# Choose 1) or 2)
# 1) For simulated data
source('./datasimu.R')
# 2) For real data
# source('./datareal.R')

Listcount <- read.csv2(paste0(rept,"Count.csv"), stringsAsFactors = FALSE)


hospital <- "XXX"
if(hospital=="EGP"){
  Listcount$tag <- ifelse(is.na(Listcount$tag1.1),Listcount$tag2.1,Listcount$tag1.1)
  Listcount <- Listcount[which(Listcount$C>=10000 & Listcount$getbio==1 & !is.na(Listcount$tag)),]
}else{
  Listcount <- Listcount[which(Listcount$getbio==1),]
}
listecalc <- as.character(Listcount$CONCEPT_CD)

# Compute mowing quantiles ####
source('movingquantiles.R')
# Missing data ####
source('missingdata.R')
# Discretization ####
source('discretization.R')
# Breakpoints ####
source('breakpoints.R')
# Trends ####
source('trends.R')

# Generate exam report ####

for(qq in listecalc){
  print(qq)
  render(paste0(rept2,"model_exams.Rmd"),
         output_file = paste0(rept2,"Reports/",gsub(":",".",qq,fixed=T),"_profile.html"),
         output_format="all",encoding = "UTF-8",
         params = list(
           exam = qq ))
}
