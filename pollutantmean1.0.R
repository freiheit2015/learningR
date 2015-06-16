pollutantmean <- function(directory, pollutant, id = 1:332) {
  v<-c()
  for( i in id ){
    filenumber<- sprintf("%03d", i)
    filename<-paste( c(directory,"/",filenumber,".csv"), collapse="" )
    datasinglefile<- read.csv(filename)
    colsinglefileV <- datasinglefile[,pollutant]
    v<-c(v,colsinglefileV)
  }
  
  mean(v,na.rm=TRUE)
}