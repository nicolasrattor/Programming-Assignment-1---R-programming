

pollutantmean<-function(x,y,z=1:332){
  out=NULL
  for(i in z){
    
    if(i<10){
      a<-paste0("00",i)
    } else if (i>=10&i<100) {
      a<-paste0("0",i)
    } else if (i>99) {
      a<-i
    } 
    
    c<-print(read.csv(paste0(x,"/",a,".csv")))
    out=rbind(out,c)
    
  }
  value<-mean(out[,y],na.rm = TRUE)
  return(value)
}
