

corr<-function(directory,threshold=0){
  
  dat<-complete(directory, 1:332)
  dat<-dat %>% filter(n > threshold)
  id<-dat$ID
  
  out=NULL
  for(i in id){
    
    if(i<10){
      a<-paste0("00",i)
    } else if (i>=10&i<100) {
      a<-paste0("0",i)
    } else if (i>99) {
      a<-i
    } 
    
    c<-read.csv(paste0(directory,"/",a,".csv"))
    c<-c %>% filter(!is.na(sulfate)&!is.na(nitrate)) %>% group_by(ID) %>% summarize(COR=cor(sulfate,nitrate)) %>% select(COR)
    out=rbind(out,c)
  }
  return(out$COR)
}

