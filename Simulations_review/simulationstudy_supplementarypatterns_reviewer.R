# Ubiquitous functions ####
source('./init.R', echo=TRUE)

plot_simulation <- function(df){
  ggplot(df, aes(date, value)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(method = "lm") + 
    geom_smooth(colour = "red")
}

# Data simulation ####
set.seed(12345)
# Pattern 1: Quadratic trend ####
test <- data.frame(date = rep(-2000:2000, each = 10))
test$value <- 10 + (test$date / 2300) ** 2 + rnorm(40010, 0, 1)
test$date <- test$date + 18000
test$date2 <- as.character(as.Date(test$date))
test$date3 <- year(as.Date(test$date)) + (month(as.Date(test$date)) -1 )/ 12
test_1b <- test
test_b <- data.frame(date = rep(-2000:2000, each = 10))
test_1b$value <- 10 + 2*((test_b$date / 2300))** 2 + rnorm(40010, 0, 1)
plot_simulation(test_1b)
plot_simulation(test)

# Pattern 2: Increasing variance (continuous) ####
test2 <- test
test2$value <- 10 + rnorm(40010, 0, 0.5 + (test2$date - 16000) / 5000)

# Pattern 3: Decreasing variance (discrete) ####
test3 <- test
test3$value <- 
  with(test3, if_else(date < 18000, sample(c(1:5) / 2 , length(date), replace = TRUE),
                      sample(c(1, 1.5, 2), length(date), prob = c(1/5, 3/5, 1/5), replace = TRUE)))

# Pattern 4: Increasing variance (skewed - lognormal) ####
test4 <- test
test4$value <- if_else(test2$date < 16500, rnorm(40010, 2.7, 0.5), 
                       exp(1 + rnorm(40010, 0, 0.5 + (test2$date - 16000) / 5000)))

# Pattern 5: Change from unimodal to bimodal ####
test5 <- test
test5$value <- 
  with(test5, rnorm(40010, 10, 1) + if_else(date < 18000, 0, sample(c(-2, 2), 40010, replace = TRUE)))

# Pattern 6: Appearance of sinusoidal patterns ####
test6 <- test
test6$value <- 
  rnorm(40010, 10, 1) + if_else(test6$date < 18250, 0, sin(2 * pi * (test6$date %% 365) / 365) / 2)


# New function for variance detection ####
detectbreakpoint_var <- function(inputData){
  # Input : moving quantile
  ansmean2 <- cpt.var(inputData$value, method="PELT")
  nbp <- ncpts(ansmean2)
  place <- inputData$start[cpts(ansmean2)]
  #
  if(nbp>0){
    toint <- c(1,cpts(ansmean2),nrow(inputData))
    tocalc <- length(toint)-1
    sdlist   <- rep(NA,tocalc)
    # If multiple breakpoints detected, compute the mean by interval
    for(i in 1:tocalc){
      sdlist[i] <- sd(inputData$value[toint[i]:toint[i+1]], na.rm = T)
    }
    # list of computed means
    sdlist <- sdlist[which(!is.na(sdlist))]
    proplist <- rep(NA, length(sdlist)-1)
    if(length(sdlist)>1){
      for(j in 1:(length(sdlist)-1) ){
        proplist[j] <- round(100*(sdlist[j]-sdlist[j+1])/sdlist[j],1)
      }
    }else{
      proplist <- 0
    }
  }else{
    proplist <- NA
    sdlist <- NA
    maxchange <- NA
  }
  # Output :
  # nbp : number of breakpoint
  # place : positions of breakpoint
  # sdlist : list of sd variation
  # maxchange : max variation of sd between intervals
  return(list(nbp=nbp, place=place,sdlist=sdlist,maxchange=max(proplist, na.rm=T)))
}

# Pattern 1: Quadratic trend ####
# As the reviewer example, some quadratic variations cannot be detected by our approach
basebio <- test
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint(movingmedian)
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breakk$place))
# In case of quadratic variation, an incurvation large enough is detected by the breakpoint function
basebio <- test_1b
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint(movingmedian)
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breakk$place))

# Pattern 2: Increasing variance (continuous) ####
basebio <- test2
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint_var(movingmedian)
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breaksave$place[which(breaksave$sdlist>0.1)-1]), col="red") # Threshold choice to avoid overdetection

# Pattern 4: Increasing variance (skewed - lognormal) ####
basebio <- test4
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint_var(movingmedian)
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breaksave$place[which(breaksave$sdlist>0.01)-1]), col="red") # Low threshold...

# Pattern 3: Decreasing variance (discrete) ####
# plot_simulation(test3)
basebio <- test3
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint_var(movingmedian) # No detection

# Pattern 5: Change from unimodal to bimodal ####
basebio <- test5
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint_var(movingmedian) # No detection
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breaksave$place[which(breaksave$sdlist>0.1)-1]), col="red") # Threshold choice to avoid overdetection

# Pattern 6: Appearance of sinusoidal patterns ####
basebio <- test6
if (sapply(basebio["value"], sd) != 0 ){
  basebio["value"] <- scale(basebio["value"])
}
movingmedian <- MovingQuantile2D(basebio,optmean = F, prob = 0.5,minrequired = 100,windowSize = 60)
breaksave <- detectbreakpoint_var(movingmedian) # No detection
# Plot
plot(movingmedian$start,movingmedian$value)
abline(v=as.Date(breaksave$place[which(breaksave$sdlist>0.1)-1]), col="red") # Threshold choice to avoid overdetection