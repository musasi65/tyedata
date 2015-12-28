library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggmap)


load("tyetagdata2.RData")

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
fff<-filter(data_row_time_test2,data_row_time_test2$ETAGPLATEID=="003000000100086762B73292")

   orderdf<-data_row_time_test2[order(data_row_time_test2$ETAGPLATEID,data_row_time_test2$tt),]

   #str(orderdf)
   #table(orderdf$ETAGID)
   #length(orderdf$ETAGPLATEID)
   cc<-table(orderdf$ETAGPLATEID)
   
   cc<-as.data.frame(cc)
   colnames(cc)=c("carid","step")
   cc_steplarge1<-filter(cc,cc$step>1)
   cccarid=c(as.character(cc_steplarge1$carid))
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
  
  od_df<-as.data.frame(carodtb_od_info_finall)
  od_df$V1<-as.character(od_df$V1)
  od_df$V2<-as.POSIXct(od_df$V2)
  od_df$V3<-as.character(od_df$V3)
  od_df$V4<-as.POSIXct(od_df$V4)
  od_df$V5<-as.character(od_df$V5)
  colnames(od_df)<-c("carodtb_carid","carodtb_o_time","carodtb_o_point","carodtb_d_time","carodtb_d_point")


  #o點分布
  #od_df_o_anal(各o點出發數量分布)

  

  od_df_o_anal<-table(od_df$carodtb_o_point)
  od_df_o_anal<-as.data.frame(od_df_o_anal)
  colnames(od_df_o_anal)=c("opointetgid","freq")
  opointetgid<-c(as.character(od_df_o_anal$opointetgid))
  
  odtable_from_o<-c()
  #各o出發的d點分布
  for(i in 1:length(opointetgid)){
    
#     i=2
    o_id<-opointetgid[[i]]
    d_freq<-table((filter(od_df,od_df$carodtb_o_point==o_id))$carodtb_d_point)
    d_freq<-as.data.frame(d_freq)
    o_d_freq<-mutate(d_freq,o_pint=o_id)
    o_d_freq<-data.frame(o_d_freq$o_pint,o_d_freq$Var1,o_d_freq$Freq)
    o_d_freq<-mutate(o_d_freq,o_d_freqrate=o_d_freq$o_d_freq.Freq/sum(o_d_freq$o_d_freq.Freq))
    colnames(o_d_freq)<-c("o_point","d_point","freq_num","freq_rate")
    
    odtable_from_o<-rbind(odtable_from_o,o_d_freq)#odtable_from_o(全od旅次數量分布)
  }
  colnames(odtable_from_o) <-c("設備ID","d_point","freq_num","freq_rate")
  joindb1<-inner_join(odtable_from_o,gate_xy,by="設備ID")
  colnames(joindb1)<-c("o_point",
                      "設備ID",
                      "freq_num",
                      "freq_rate",
                      "設備編號",
                      "行政區",
                      "路口",
                      "方向",
                      "lon",
                      "lat")
  joindb1<-inner_join(joindb1,gate_xy,by="設備ID")
  colnames(joindb1)<-c("o_point",
                       "d_point",
                       "freq_num",
                       "freq_rate",
                       "o_設備編號",
                       "o_行政區",
                       "o_路口",
                       "o_方向",
                       "o_lon",
                       "o_lat",
                       "d_設備編號",
                       "d_行政區",
                       "d_路口",
                       "d_方向",
                       "d_lon",
                       "d_lat")

#    for(i in 1:nrow(od_table_from_o_map_data)){
    
    nn=nrow(od_table_from_o_map_data)
    
    
    route_df_total=c()
    centerpoint="24.97627, 121.27003"
      for(i in 37:60){

      rout_od<-od_table_from_o_map_data[i,]
      
      from_lon<-rout_od$lon.x
      from_lat<-rout_od$lat.x
      
      to_lon<-rout_od$lon.y
      to_lat<-rout_od$lat.y  

      from<-paste(as.character(from_lat),as.character(from_lon),sep=",")
      to<-paste(as.character(to_lat),as.character(to_lon),sep=",")
      
      route_df <- route(from, to, structure = "route")
      route_df_total<-rbind(route_df_total,route_df)
      
      
      #Colors=grDevices::rainbow(13)

#       path=geom_path(aes(x = lon, y = lat),  
#                         # colour = Colors[i], 
#                         colour ="red",
#                         size = 1.5,
#                         data = route_df) 
#                         # lineend = "round")
      
      
# 
#       if(i==37){
#         od_map=qmap(centerpoint, zoom = 12)+path
#       }else{      
#        od_map=od_map+path
# 
#       }
#       
    }
    
    od_map
    rm(od_map)

    
    centerpoint="24.97627, 121.27003"
    for(i in 37:50){
      
      rout_od<-od_table_from_o_map_data[i,]
      
      from_lon<-rout_od$lon.x
      from_lat<-rout_od$lat.x
      
      to_lon<-rout_od$lon.y
      to_lat<-rout_od$lat.y  
      
      from<-paste(as.character(from_lat),as.character(from_lon),sep=",")
      to<-paste(as.character(to_lat),as.character(to_lon),sep=",")
      
      legs_df <-route(from, to)
      
      #Colors=grDevices::rainbow(13)
      
      path=geom_segment(
        aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
        colour = "red", size = 1.5, data = legs_df
      )
      # lineend = "round")
      
      
      if(i==37){
        od_map=qmap(centerpoint, zoom = 11)+path
      }else{      
        (od_map=od_map+path)

        
        rm(path)
        rm(rout_od)
      }
      
    }
    
    od_map
    rm(od_map)
    
    
    
    
    
    rm(od_map)
    
    
  
#   驗算 ok  
#   totalnum<-sum(odtable_from_o$freq_num)
#   totalnum
  
  
    carodtb_od_info<-c()
  carodtb_od_info_add<-c()
  carodtb_od_info_finall<-c()    
  
  
  
  
  
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

                          
                          
