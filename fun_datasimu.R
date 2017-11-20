# Function to simulate data ####
# Date of creation : 

generationdata <- function(scenario=1, mindate="2003-12-30", maxdate="2016-01-04", breakdate="2012-02-01", breakdate2="2014-02-01", volume=7){
  numsimu <- paste0("BIO:",gsub(":","",gsub("-","",gsub("[[:space:]]","",Sys.time()))))
  vd = seq(as.Date("2003-12-30"), as.Date("2016-01-04"), by="days")
  
  if(scenario==0){
    # Simulation from a normal law
    d = data.frame()
    for(da in vd) {
      vv = rnorm(volume,7,3)
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  
  if(scenario==1){
    # Simulation from a normal law with a breakpoint at breakpoint1 and breakpoint2
    d = data.frame()
    for(da in vd) {
      vv = rnorm(volume,7,3)
      if(da >= as.numeric(as.Date(breakdate)) & da <= as.numeric(as.Date(breakdate2)) ) {
        vv = rnorm(volume,11,3)
      }
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  
  if(scenario==2){
    # Simulation from a normal law with discretisation and mixed distribution
    d = data.frame()
    for(da in vd) {
      vv = rnorm(ceiling(volume/2),7,3)
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
      vv = round(rnorm(ceiling(volume/2),7,3))
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  if(scenario==3){
    mind = as.numeric(min(vd))
    maxd = as.numeric(max(vd))
    d = data.frame()
    for(da in vd) {
      m = (maxd - da)/(maxd-mind)
      vv = rnorm(volume,6+2*m,3)
      
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  
  if(scenario==4){
    # Simulation from a normal law with discretisation and breakpoint
    d = data.frame()
    for(da in vd) {
      if(da >= as.numeric(as.Date(breakdate)) & da <= as.numeric(as.Date(breakdate2)) ) {
        vv = round(rnorm(volume,7,3))
      }else{
        vv = rnorm(volume,7,3)
      }
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  
  if(scenario==5){
    # Simulation from a normal law with a breakpoint at breakpoint1 and breakpoint2
    d = data.frame()
    for(da in vd) {
      vv = rnorm(volume,7,3)
      if(da >= as.numeric(as.Date(breakdate)) & da <= as.numeric(as.Date(breakdate2)) ) {
        vv = rep(NA,volume)
      }
      for(value in vv) {
        a = c(da, value)
        d = rbind(d, a)
      }
    }
    colnames(d) = c("date","value")
  }
  
  d$date2 <- as.Date(d$date, origin="1970-01-01")
  d$date3 <- year(d$date2)+ (month(d$date2)-1)/12
  
  out <- list(CONCEPT_CD=numsimu,data=d,C=nrow(d))
  return(out)
}