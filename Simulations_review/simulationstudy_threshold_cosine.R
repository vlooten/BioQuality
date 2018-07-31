source('./init.R', echo=TRUE)

compute_performance <- function(real, pred,cat=c(0,1)){
  TP <- sum(real == cat[2] & pred == cat[2])
  TN <- sum(real == cat[1] & pred == cat[1])
  FP <- sum(real == cat[1] & pred == cat[2])
  FN <- sum(real == cat[2] & pred == cat[1])
  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)
  FMeasure <- (2 * Precision * Recall) / (Precision + Recall)
  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  results <- list(TP=TP, FP=FP, TN=TN, FN=FN,Precision=Precision,Recall=Recall,FMeasure=FMeasure,Accuracy=Accuracy)
  return (results)
}

cosinesimu <- function(percentage=0.5, discritization=T, roundpos=1, datamodel=c("vv <- runif(volume,0,10000)") ){
  # percentage: percentage of rounded/discretized value
  # roundpos: position of rounding
  # datamodel: model of data, e.g. : vv <- rnorm(volume,7,3) or vv <- runif(volume,0,10000)
  
  timeline <- seq(as.Date("2000-01-01"), as.Date("2001-01-01"), by="days") # timeline of simulations
  if(discritization){
    breakdate <- as.Date("2000-06-01") # date of discritization
  }else{
    breakdate <- as.Date("9999-01-01") # date of discritization
  }
  
  volume <- 10 # data points by time unit
  
  d = data.frame()
  for(da in timeline) {
    # Not altered data
    eval(parse(text=datamodel))
    
    # Data alteration
    if(da >= as.numeric(breakdate) ) {
      idx <- sample(c(1:length(vv)), percentage*volume,replace = F)
      vv[idx] <- round(vv[idx],roundpos)
    }
    
    # Dataset building
    for(value in vv) {
      a <- c(da, value)
      d <- rbind(d, a)
    }
  }
  colnames(d) <- c("date","value")
  d$date2 <- as.Date(d$date, origin="1970-01-01")
  d$date3 <- year(d$date2)+ (month(d$date2)-1)/12
  return(d)
}

# Simulations
# Take a long time, we provide rds files in the github repository

set.seed(1987)
prop <- 0.5
res_density = data.frame()
for(N in 1:100){
for(w in c(F,T) ){
    datasim <- cosinesimu(percentage = prop, roundpos = 1,discritization = w, datamodel=paste0("vv <- runif(volume,0,100)"))
    for(thres in seq(10,100,1)){
    cosmeth <- discretization_raw(datasim,target_cosine = thres)
    detect <-  cosmeth$discretization
    res_density <- rbind(res_density,c(w,prop,detect,thres))
    }
  }
}

colnames(res_density) <- c("discretization.y_n","percentage_discret","detected.y_n","threshold")
# res_density
saveRDS(res_density,"res_density.rds")
#
set.seed(1987)
prop <- 0.5
res_density_2 = data.frame()
for(N in 1:100){
  for(w in c(F,T) ){
      datasim <- cosinesimu(percentage = prop, roundpos = 1,discritization = w, datamodel="vv <- rnorm(volume,7,3)")
      for(thres in seq(10,100,1)){
        cosmeth <- discretization_raw(datasim,target_cosine = thres)
        detect <-  cosmeth$discretization
        res_density_2 <- rbind(res_density_2,c(w,prop,detect,thres))
      }
    }
  }
colnames(res_density_2) <- c("discretization.y_n","percentage_discret","detected.y_n","threshold")
saveRDS(res_density_2,"res_density_2.rds")

#
set.seed(1987)
prop <- 1
res_density_3 = data.frame()
for(N in 1:100){
  for(w in c(F,T) ){
    datasim <- cosinesimu(percentage = prop, roundpos = 1,discritization = w, datamodel=paste0("vv <- runif(volume,0,100)"))
    for(thres in seq(10,100,1)){
      cosmeth <- discretization_raw(datasim,target_cosine = thres)
      detect <-  cosmeth$discretization
      res_density_3 <- rbind(res_density_3,c(w,prop,detect,thres))
    }
  }
}

colnames(res_density_3) <- c("discretization.y_n","percentage_discret","detected.y_n","threshold")
saveRDS(res_density_3,"res_density_3.rds")

# Results ####
sim1 <- data.frame()
list_thres <- unique(res_density$threshold)
for(o in list_thres){
  idx <- which(res_density$threshold==o)
  toto <- compute_performance(res_density$discretization.y_n[idx],res_density$detected.y_n[idx])
  sim1 <- rbind(sim1,c(threshold=o,toto))
}
plot(sim1$threshold,sim1$FMeasure)
plot(sim1$threshold,sim1$Precision)
plot(sim1$threshold,sim1$Recall)
plot(sim1$threshold,sim1$TP+sim1$FP)
#
sim2 <- data.frame()
list_thres <- unique(res_density_2$threshold)
for(o in list_thres){
  idx <- which(res_density_2$threshold==o)
  toto <- compute_performance(res_density_2$discretization.y_n[idx],res_density_2$detected.y_n[idx])
  sim2 <- rbind(sim2,c(threshold=o,toto))
}
plot(sim2$threshold,sim2$FMeasure)
plot(sim2$threshold,sim2$Precision)
plot(sim2$threshold,sim2$Recall)
plot(sim2$threshold,sim2$TP+sim1$FP)
#
#
sim3 <- data.frame()
list_thres <- unique(res_density_3$threshold)
for(o in list_thres){
  idx <- which(res_density_3$threshold==o)
  toto <- compute_performance(res_density_3$discretization.y_n[idx],res_density_3$detected.y_n[idx])
  sim3 <- rbind(sim3,c(threshold=o,toto))
}
plot(sim3$threshold,sim3$FMeasure)
plot(sim3$threshold,sim3$Precision)
plot(sim3$threshold,sim3$Recall)
plot(sim3$threshold,sim3$TP+sim1$FP)
