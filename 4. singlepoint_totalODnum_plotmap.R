
###########################################################################
# 以下為了畫"各別"起點/迄點分析熱力圖，加總各點之總O/D旅次量，
#成果為total_sum_ana_gatexy_facet資料庫，增加 data_type欄位以分隔兩種資料
#用ggmap中facet_grid( ~ data_type)畫並列比較圖
###########################################################################

o_sum_ana<-aggregate(joindb1$freq_num,by=list(o_point=joindb1$o_point),FUN = sum)
colnames(o_sum_ana)<-c("設備ID","freq")
o_sum_ana_gatexy<-inner_join(o_sum_ana,gate_xy,by="設備ID")
colnames(o_sum_ana_gatexy)<-c("o_point",
                              "freq_num",
                              "o_設備編號",
                              "o_行政區",
                              "o_路口",
                              "o_方向",
                              "o_lon",
                              "o_lat")

d_sum_ana<-aggregate(joindb1$freq_num,by=list(d_point=joindb1$d_point),FUN = sum)
colnames(d_sum_ana)<-c("設備ID","freq")  
d_sum_ana_gatexy<-inner_join(d_sum_ana,gate_xy,by="設備ID")  
colnames(d_sum_ana_gatexy)<-c("d_point",
                              "freq_num",
                              "d_設備編號",
                              "d_行政區",
                              "d_路口",
                              "d_方向",
                              "d_lon",
                              "d_lat")

o_sum_ana_gatexy_facet<-mutate(o_sum_ana_gatexy,data_type="起點分析")
colnames(o_sum_ana_gatexy_facet)<-c("point",
                                    "freq_num",
                                    "設備編號",
                                    "行政區",
                                    "路口",
                                    "方向",
                                    "lon",
                                    "lat",
                                    "data_type")

d_sum_ana_gatexy_facet<-mutate(d_sum_ana_gatexy,data_type="迄點分析") 
colnames(d_sum_ana_gatexy_facet)<-c("point",
                                    "freq_num",
                                    "設備編號",
                                    "行政區",
                                    "路口",
                                    "方向",
                                    "lon",
                                    "lat",
                                    "data_type")

total_sum_ana_gatexy_facet<-rbind(o_sum_ana_gatexy_facet,d_sum_ana_gatexy_facet)

# centerpoint<-c(121.308029, 24.985880)
centerpoint<-c(121.267175,24.962517)

map_base2 <-get_googlemap(center=centerpoint, 
                          color = "bw",
                          zoom=12,
                          maptype='roadmap', 
                          extent='device')
#ggmap(map_base2)

#   map_base1 <- get_map(location = 'Taoyuan city', zoom = 12)
#   ggmap(map_base1)
#   map_base1_1 <- get_map(location = centerpoint, zoom = 11)
#   
map.base<-ggmap(map_base2, extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1)
#   map.base

num<-nrow(total_sum_ana_gatexy_facet)
Colors=grDevices::rainbow(num/2)
ssize_v=c()   

for(x in 1:num){  
  if(total_sum_ana_gatexy_facet[x,2]/10<3) {
    size=3
    ssize_v=c(ssize_v,size)} #依流量大小決定Pin size大小
  
  else{ 
    size=total_sum_ana_gatexy_facet[x,2]/10
    ssize_v=c(ssize_v,size)
  }
}

ssize_v<-as.data.frame(ssize_v) 

for(i in 1:num){
  
  if(i<=num/2){
    
    colournum<-Colors[i]
    
  }else{
    
    colournum<-Colors[i-num/2]
  }
  
  
  if(i==1){
    map.finall<-map.base+
      geom_point(data=total_sum_ana_gatexy_facet[i,],aes(x=lon, y=lat), colour=colournum, alpha=0.3,size=ssize_v[i,])
    
  }else{
    
    map.finall<-map.finall+
      geom_point(data=total_sum_ana_gatexy_facet[i,],aes(x=lon, y=lat), colour=colournum, alpha=0.3,size=ssize_v[i,])+
      facet_grid( ~ data_type)
  }
}
rm(ssize_v)
map.finall  #### O/D 兩張分分布圖
