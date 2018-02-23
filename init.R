
# Load packages  ####
listepackages<-c("readr","ggplot2","lubridate","quantreg","dplyr","stringr",
                 "reshape2","readr","scales","lsa","gridExtra","changepoint",
                 "knitr","DT","rmarkdown","segmented")
for (pack in listepackages) {
  if (!is.element(pack, installed.packages()[,1])){
    install.packages(pack, dep = TRUE)
  }
  eval(parse(text=paste0("library(",pack,")")))
}
rm(pack)


# Create subdirectories ####
rept2 <- paste0(getwd(),"/") 

if(!dir.exists(paste0(rept2,"Data/"))){
  dir.create(paste0(rept2,"Data/"),showWarnings = F)
}
rept <- paste0(getwd(),"/Data/") # Directory of data
if(!dir.exists(paste0(rept2,"Graphs/"))){
  dir.create(paste0(rept2,"Graphs/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"MovingQuantiles/"))){
  dir.create(paste0(rept2,"MovingQuantiles/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"MovingCount/"))){
  dir.create(paste0(rept2,"MovingCount/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"discretisation/"))){
  dir.create(paste0(rept2,"discretisation/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"trendsresult/"))){
  dir.create(paste0(rept2,"trendsresult/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"missingdata/"))){
  dir.create(paste0(rept2,"missingdata/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"breakpoints/"))){
  dir.create(paste0(rept2,"breakpoints/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"missingdata/graph_md/"))){
  dir.create(paste0(rept2,"missingdata/graph_md/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"discretisation/graph_discret_w/"))){
  dir.create(paste0(rept2,"discretisation/graph_discret_w/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"breakpoints/graph_bp/"))){
  dir.create(paste0(rept2,"breakpoints/graph_bp/"),showWarnings = F)
}
if(!dir.exists(paste0(rept2,"Reports/"))){
  dir.create(paste0(rept2,"Reports/"),showWarnings = F)
}

# Load functions ####

if(file.exists("./fun_datasimu.R")){
  source('./fun_datasimu.R')
}else{
  print('file fun_datasimu.R missing, cannot simulate data ')
}

if(file.exists("./fun_movingquantiles.R")){
  source('./fun_movingquantiles.R')
}else{
  print('file fun_movingquantiles.R missing, cannot execute algorithm ')
}

if(file.exists("./fun_missingdata.R")){
  source('./fun_missingdata.R')
}else{
  print('file fun_missingdata.R missing, cannot execute missing data detection algorithm ')
}

if(file.exists("./fun_discretization.R")){
  source('./fun_discretization.R')
}else{
  print('file fun_discretization.R missing, cannot execute missing data detection algorithm ')
}

if(file.exists("./fun_breakpoints.R")){
  source('./fun_breakpoints.R')
}else{
  print('file fun_breakpoints.R missing, cannot execute breakpoint detection algorithm ')
}
if(file.exists("./fun_trends.R")){
  source('./fun_trends.R')
  if(!file.exists("./fun_breakpoints.R")){
    print('file fun_breakpoints.R missing, cannot execute trends detection algorithm (need detect breakpoint before)')
  }
}else{
  print('file fun_trends.R missing, cannot execute trends detection algorithm ')
}

loaddata <- function(concept, opt="new", dirpath="./data/"){
  hb <- read.csv2(paste0(rept,sub(pattern = ":",replacement = ".",x = concept,perl = F),".csv"))
  return(hb)
}

graphviewreal <- function(dtt, titre="",NN=40000, optlim=F, bornes=c(0,10)){
  gg = ggplot(dplyr::sample_n(dtt,min(NN,nrow(dtt))), aes(x=as.Date(date2), y=value),environment=environment()) + 
    theme_bw() + ggtitle(titre) + geom_point(alpha=0.05) +
    theme(axis.text.x = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=.5,face="plain"),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="none")
  if(optlim){
    gg <- gg  + coord_cartesian(ylim = bornes)
  }
  return(gg)
}