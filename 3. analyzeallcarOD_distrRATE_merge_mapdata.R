
########################################################
#目的為維分析出全部OD組合之各別旅次數量
#
#
#1. 首先實作o點分布，產出od_df_o_anal(各o點出發數量分布)

od_df_o_anal<-table(od_df$carodtb_o_point)
od_df_o_anal<-as.data.frame(od_df_o_anal)
colnames(od_df_o_anal)=c("opointetgid","freq")
opointetgid<-c(as.character(od_df_o_anal$opointetgid))



#各o出發的d點分布，產出odtable_from_o

odtable_from_o<-c()

for(i in 1:length(opointetgid)){
  
  #     i=2
  o_id<-opointetgid[[i]]
  d_freq<-table((filter(od_df,od_df$carodtb_o_point==o_id))$carodtb_d_point)
  d_freq<-as.data.frame(d_freq)
  o_d_freq<-mutate(d_freq,o_pint=o_id)
  o_d_freq<-data.frame(o_d_freq$o_pint,o_d_freq$Var1,o_d_freq$Freq)
  o_d_freq<-mutate(o_d_freq,o_d_freqrate=o_d_freq$o_d_freq.Freq/sum(o_d_freq$o_d_freq.Freq))
  colnames(o_d_freq)<-c("o_point","d_point","freq_num","freq_rate")
  
  odtable_from_o<-rbind(odtable_from_o,o_d_freq)
}

##################################



#  odtable_from_o (全od旅次數量分布)




##################################

#以下為了連結mapdata，成果tb為joindb1


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










