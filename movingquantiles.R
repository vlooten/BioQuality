# Compute 2D Moving Quantiles  #### 
# In our approach, we does not take into account the individual dimension
# That's why, we proceed to a 2 dimensional moving quantile computation
# In the first dimension, for each time, we compute the median
# In the second dimension, for each moving time windows, we compute the moving median

# Setting ####
# Necessary for execution : source('./fun_movingquantiles.R')

# Enough data condition
# Size of temporal windows (in days) : windowSize
setwindowSize <- 60
# Minimal number of exam required by window
setminrequired <- 100


# Compute for all exams ####
# Exams list is saved in the object "listecalc"
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
  
  # Compute moving median
  if(!file.exists(paste0(reptMovingQuant,nomfichier,".Median.csv"))){
    calc <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = setminrequired,windowSize = setwindowSize)
    # Save result in file because this step take time with a huge amount of data
    write.csv2(calc,paste0(reptMovingQuant,nomfichier,".Median.csv"), row.names = F)
    print("Median OK")
  }else{
    print("Moving median already computed")
  }
    print(paste0(kk," OK"))
}
