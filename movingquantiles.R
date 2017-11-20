# Compute 2D Moving Quantiles  ####  
# Date of creation : 

# Setting ####
# Necessary for execution : source('./fun_movingquantiles.R')
# Size of temporal windows (in days) : windowSize
setwindowSize <- 60
# Minimal number of exam required by window
setminrequired <- 100


# Compute for all exams ####
# Listcount <- read.csv2(paste0(rept,"Count.csv"))
# listecalc <- as.character(Listcount$CONCEPT_CD[which(Listcount$getbio==1)])
kk <- ""

for(qq in 1:length(listecalc)){
  kk <- listecalc[qq]
  print(paste0(kk, " start"))
  # Load data
  nomfichier <- sub(pattern = ":",replacement = ".",x = kk,perl = F)
  basebio <- read.csv2(paste0(rept,nomfichier,".csv"))
  basebio <- basebio[which(!is.na(basebio$value)),]
  # Scale data
  if (sapply(basebio["value"], sd) != 0 )
  {
    basebio["value"] <- scale(basebio["value"])
  }
  # Output directory
  reptMovingQuant <- paste0(rept2,"MovingQuantiles/",nomfichier,"/")
  if(!dir.exists( reptMovingQuant)){
    dir.create( reptMovingQuant ,showWarnings = F)
  }
  # Compute moving mean
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Mean.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = T, minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Mean.csv"), row.names = F)
    print("Mean OK")
  }else{
    print("Moving mean already computed")
  }
  # Compute moving median
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Median.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Median.csv"), row.names = F)
    print("Median OK")
  }else{
    print("Moving median already computed")
  }
  # Compute moving min
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Min.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 0,minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Min.csv"), row.names = F)
    print("Min computed")
  }else{
    print("Moving min already computed")
  }
  # Compute moving max
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Max.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 1,minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Max.csv"), row.names = F)
    print("Max computed")
  }else{
    print("Moving max already computed")
  }
  # Compute moving Q1
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Q1.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 0.25,minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Q1.csv"), row.names = F)
    print("Q1 computed")
  }else{
    print("Moving Q1 already computed")
  }
  # Compute moving Q3
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Q3.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 0.75,minrequired = setminrequired,windowSize = setwindowSize)
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Q3.csv"), row.names = F)
    print("Q3 computed")
  }else{
    print("Moving Q3 already computed")
  }
  print(paste0(kk," OK"))
}
