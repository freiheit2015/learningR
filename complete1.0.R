complete <- function(directory, id = 1:332) {
  nobs<-c()
  for( i in id ){
    filenumber<- sprintf("%03d", i)
    filename<-paste( c(directory,"/",filenumber,".csv"), collapse="" )
    datasinglefile<- read.csv(filename)
    datasinglefilecomplete<-na.omit(datasinglefile)
    nobsonefile<-nrow(datasinglefilecomplete)
    nobs<-c(nobs,nobsonefile)
  }
  x<-data.frame(id,nobs)
  x
}