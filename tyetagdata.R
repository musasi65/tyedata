library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)


load("tyetagdata.RData")

file1 ="C:/Users/62552/Desktop/桃園etag分析/tycetagdata2.csv"
file2 ="C:/Users/62552/Desktop/桃園etag分析/20150902 eTag設備位置3.xlsx"

data_row<-read.table(file1, header=T,sep = ",",encoding = "UTF-8")
 gate_xy<-read.xlsx(file2, header=T,sheetName = '1',encoding = "UTF-8")
# str(data_row)
data_row$ETAGID=as.character(data_row$ETAGID)
data_row$ETAGDATE=as.POSIXct(data_row$ETAGDATE)
data_row$ETAGPLATEID=as.character(data_row$ETAGPLATEID)

data_row_time<-mutate(data_row,
                      year=as.numeric(substr(ETAGDATE,0,4)),
                      month=as.numeric(substr(ETAGDATE,6,7)),
                      date=as.numeric(substr(ETAGDATE,9,10)),     
                      hour=as.numeric(substr(ETAGDATE,12,13)), 
                      min=as.numeric(substr(ETAGDATE,15,16)),
                      time=as.numeric(hour*60+min)
                      )
#用9/8 07~09數據實驗
data_row_time_test=filter(data_row_time,data_row_time$month==9,
                                        data_row_time$date==8,
                                        data_row_time$hour>=7&data_row_time$hour<10)
data_row_time_test2<-mutate(data_row_time_test,tt=as.numeric(data_row_time_test$ETAGDATE))

   orderdf<-data_row_time_test2[order(data_row_time_test2$ETAGPLATEID,data_row_time_test2$tt),]

   str(orderdf)
   table(orderdf$ETAGID)
   length(orderdf$ETAGPLATEID)
   cc<-table(orderdf$ETAGPLATEID)
   
   cc<-as.data.frame(cc)
   colnames(cc)=c("carid","step")
   cccarid=c(as.character(cc$carid))
   #str(cccarid)
   #cccarid同車輛過etag 次數(通過etag偵測點數) 
   
   #cccarid[[3]]
   cccarid
   #用編號10車輛測試(因其通過2個測站)
   

#    
#     carodtb_od_info<-data.frame()
#     carodtb_od_info_finall<-data.frame(carodtb_carid,carodtb_o_time,carodtb_o_point,carodtb_d_time,carodtb_d_point)

  carodtb_od_info<-c()
  carodtb_od_info_add<-c()
  carodtb_od_info_finall<-c()    
   
   for(j in 30486:30492){
      
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
       
       if(diffmins>=45){

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
  
  df<-as.data.frame(carodtb_od_info_finall)
  str(df)
  df$V1<-as.character(df$V1)
  df$V2<-as.POSIXct(df$V2)
  df$V3<-as.character(df$V3)
  df$V4<-as.POSIXct(df$V4)
  df$V5<-as.character(df$V5)
  colnames(df)<-c("carodtb_carid","carodtb_o_time","carodtb_o_point","carodtb_d_time","carodtb_d_point")
  
  
carodtb_od_info   

   df<-as.data.frame(carodtb_od_info)         
   df
   str(df)
      carodtb_od_info<-c(carodtb_carid,carodtb_o_time,carodtb_o_point,carodtb_d_time,carodtb_d_point)
      carodtb_od_info<-cbind(carodtb_od_info,carodtb_od_info_final)
      rm(carodtb_od_info_final)
     }
       
       difftime(carodtb_time
     
     
                
    }
     carodtb_od_info<-c(carodtb_carid,carodtb_o_time,carodtb_o_point,carodtb_d_time,carodtb_d_point)
     carodtb_od_info
   
   
   
   carodtb_carid<-cccarid[[1]]
   carodtb_o<-
   
   id=factor(cccarid)
   str(id)
   id

   
   cct<-as.data.table(cc)
   length(cc$Var1)
   ccm<-matrix(cc)
   cct
   id=cc[,2]
   
   id
   
   row.names(cc)<-cc$Var1
   
#    x=data.frame(name=c("张三","李四","王五","赵六"),sex=c("M","M","F","F"),
#                 age=c(20,40,22,30),
#                 height=c(166,170,150,155))
#    aggregate(x[,3:4],by=list(sex=x$sex),FUN=mean)
   
   
   for(i in 1:2){
     
     ccid<-c()
     cctest=c(cc$Var1)
   }
   

     
   }
   
   

   for(i in 1:nrow(data_row_time_test2)){
     
     carid=orderdf$ETAGPLATEID
     op=orderdf$ETAGID
     starttime=orderdf$ETAGDATE
     if()
     
     
     
     
   }
   length(diag(4))  # = 16 (4 x 4)
   length(options())  # 12 or more
   length(y ~ x1 + x2 + x3)  # 3
   length(expression(x, {y <- x^2; y+2}, x^y))  # 3

                          
                          
