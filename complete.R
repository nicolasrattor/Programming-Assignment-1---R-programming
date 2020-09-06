

complete<-function(directory, id){
  out=NULL
  for(i in id){
    
    if(i<10){
      a<-paste0("00",i)
    } else if (i>=10&i<100) {
      a<-paste0("0",i)
    } else if (i>99) {
      a<-i
    } 
    
    c<-print(read.csv(paste0(directory,"/",a,".csv")))
    out=rbind(out,c)
    
  }
  
  value<-out %>% filter(!is.na(sulfate)&!is.na(nitrate)) %>% group_by(ID) %>% count()
  return(value)
}
