carodtb_od_info<-c()
carodtb_od_info_add<-c()
carodtb_od_info_finall<-c()    

for(j in 1:length(cccarid)){
  
  carodtb_carid<-cccarid[[j]]
  
  carodtb<-filter(data_row_time_test2,ETAGPLATEID==cccarid[[j]])
  
  carodtb<-carodtb[order(carodtb$tt),]
  # carodtb_od_info_final<-c()
  
  
  for(i in 1:nrow(carodtb)){
    
    carodtb_time<-as.character(carodtb[i,]$ETAGDATE)
    carodtb_point<-carodtb[i,]$ETAGID
    
    if(i==1){
      carodtb_o_time<-carodtb_time
      carodtb_o_point<-carodtb_point
    }
    
    if(i==nrow(carodtb)){
      carodtb_d_time<-carodtb_time
      carodtb_d_point<-carodtb_point
      carodtb_od_info_stepend<-c(carodtb_carid,carodtb_o_time,carodtb_o_point,carodtb_d_time,carodtb_d_point)
      carodtb_od_info_end<-rbind(carodtb_od_info,carodtb_od_info_stepend)
      carodtb_od_info_stepend=c()
      carodtb_od_info=c()
      
    }else{
      if(i==1){next}
      
      z<-difftime(carodtb_time,carodtb[i-1,]$ETAGDATE)
      diffmins<-as.numeric(z, units = "mins")
      
      if(diffmins>=60){
        
        carodtb_d_time<-as.character(carodtb[i-1,]$ETAGDATE)
        carodtb_d_point<-carodtb[i-1,]$ETAGID
        carodtb_od_info_add<-c(carodtb_carid,carodtb_o_time,carodtb_o_point,carodtb_d_time,carodtb_d_point)
        carodtb_od_info<-rbind(carodtb_od_info,carodtb_od_info_add)
        carodtb_od_info_add=c()         
        # carodtb_od_info<-cbind(carodtb_od_info,carodtb_od_info_add)
        
        carodtb_o_time<-as.character(carodtb[i,]$ETAGDATE)
        carodtb_o_point<-carodtb[i,]$ETAGID
      }
    }
    
  }
  
  carodtb_od_info_finall<-rbind(carodtb_od_info_finall,carodtb_od_info_end)
  carodtb_od_info_end=c()
  
}

od_df<-as.data.frame(carodtb_od_info_finall)
od_df$V1<-as.character(od_df$V1)
od_df$V2<-as.POSIXct(od_df$V2)
od_df$V3<-as.character(od_df$V3)
od_df$V4<-as.POSIXct(od_df$V4)
od_df$V5<-as.character(od_df$V5)
colnames(od_df)<-c("carodtb_carid","carodtb_o_time","carodtb_o_point","carodtb_d_time","carodtb_d_point")

