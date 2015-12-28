library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)

#=============讀取資料庫 by TenThousand==========

#===========10/26==============

load("etclight.RData")
load("gatexy.RData")
load("qkvdata.RData")


#===========關閉原本資料來源============
#file1 ="C:/etag競賽資料/testdata4.csv"
#file2 ="C:/etag競賽資料/etag門架資料_spp.xlsx"
#data_row<-read.table(file1, header=T,sep = ",",encoding = "UTF-8",col.names = c("type", "stt", "origingatenum", "arrt", "desgatenum","avt"))
#===========10/16==============

#str(data_row)
#View(data_row)

shinyServer(function(input, output) {
  
  merger_data_finall_time$前端匝道<-as.character(merger_data_finall_time$前端匝道)
  merger_data_finall_time$前端匝道<-enc2native(merger_data_finall_time$前端匝道)
  #_P for 下匝道需求分析
  merger_data_finall_time_P<-mutate(merger_data_finall_time,stt_fulltime=stt_hour*60+stt_min,arrt_fulltime=arrt_hour*60+arrt_min)
  
   merger_data_finall_time$後端匝道<-as.character(merger_data_finall_time$後端匝道)
   merger_data_finall_time$後端匝道<-enc2native(merger_data_finall_time$後端匝道) 

  
  options(shiny.usecairo = FALSE)

  create_mccs <- reactive({
    
    if(input$method =="OD迄點分析"||input$method =="OD起點分析"){
    
    selectdes=input$destination
    selecto=input$origin
    
    if (input$method =="OD迄點分析"){
    
      selecthour=input$obs1
      selectdate=input$date1   
      datalist=filter(merger_data_finall_time,前端匝道==selecto)
      datalist=filter(datalist,stt_hour==selecthour)
      datalist=filter(datalist,arrt_date==selectdate)
      
      #按分鐘分group
      t=1   
      datalist_t<-filter(datalist,(stt_min<=15*t))
      cc<-table(datalist_t$後端匝道)
      mcc<- as.data.frame(cc)
      
      for(t in 2:4){
        datalist_t<-filter(datalist,(stt_min<=15*t & stt_min>15*(t-1)))
        cc<-table(datalist_t$後端匝道)
        cc<- as.data.frame(cc)
        mcc<-inner_join(mcc,cc,by="Var1")
      }
      }
       
      if (input$method =="OD起點分析"){
        
        selecthour=input$obs2
        selectdate=input$date2 
        datalist=filter(merger_data_finall_time,後端匝道==selectdes)
        datalist=filter(datalist,arrt_hour==selecthour)
        datalist=filter(datalist,arrt_date==selectdate)

        #按分鐘分group
        t=1   
        datalist_t<-filter(datalist,(stt_min<=15*t))
        cc<-table(datalist_t$前端匝道)
        mcc<- as.data.frame(cc)
        
        for(t in 2:4){
          datalist_t<-filter(datalist,(stt_min<=15*t & stt_min>15*(t-1)))
          cc<-table(datalist_t$前端匝道)
          cc<- as.data.frame(cc)
          mcc<-inner_join(mcc,cc,by="Var1")
        }
        }   
      
      #改欄位名      
      colnames(mcc)=c(" ","t1","t2","t3","t4")
      mccs<-filter(mcc,t1>3&t2>3&t3>3&t4>3) 
      mccs<-arrange(mccs,-t1)
      View(mccs)
      mccs<-(head(mccs,n=10))
      mccs
    }

  })
  
  create_mccsforlast <- reactive({
      if (input$method == "下匝道流量需求預測分析"){
        
        selecthour=input$obs_P
        selectdate=input$date_P
        selectdes=input$destination_P
        selectdir=input$direction
        
        #==========================組成分析開始============================  
        
        #==========================組成分析開始============================  
        
        data_check_o=filter(merger_data_finall_time,後端匝道==selectdes)
        data_check_o=filter(data_check_o,arrt_hour==selecthour)
        data_check_o=filter(data_check_o,arrt_date==selectdate)
        data_check_o<-mutate(data_check_o,dir=as.character(substr(desgatenum,8,8)))
        
        if(selectdir=="南向"){
          data_check_o=filter(data_check_o,dir=="S")
        }else{
          data_check_o=filter(data_check_o,dir=="N")
        }
        
        t=1   
        data_check_o_temp<-filter(data_check_o,(arrt_min<=15*t))
        cc<-table(data_check_o_temp$前端匝道)
        mcc<- as.data.frame(cc) 
        
        for(t in 2:4){
          data_check_o_temp<-filter(data_check_o,(arrt_min<=15*t & arrt_min>15*(t-1)))
          cc<-table(data_check_o_temp$前端匝道)
          cc<- as.data.frame(cc)
          mcc<-inner_join(mcc,cc,by="Var1")
        }
        
        colnames(mcc)=c("name","t1","t2","t3","t4")
        mccs<-filter(mcc,t1>5&t2>5&t3>5&t4>30) 
        mccs
      }
  })
  
  
  
  create_frompp<-reactive({
    
    if (input$method == "旅行時間分析"){
    
    selecthour=input$obs
    selectdate=input$date
    selecto=input$origin_T
    selectdes=input$destination_T
    
    datalist=filter(merger_data_finall_time,前端匝道==selecto)
    datalist=filter(datalist,後端匝道==selectdes)
    datalist=filter(datalist,stt_hour==as.character(selecthour))
    datalist=filter(datalist,arrt_date==selectdate)
    
    for(t in 1:12){
      
      datalist_t<-filter(datalist,(stt_min<=5*t & stt_min>5*(t-1)))
      trt=c((datalist_t$arrt_hour-datalist_t$stt_hour)*60+(datalist_t$arrt_min-datalist_t$stt_min))
      ttt = mean(c(trt))
      
      if(t==1)
        traveltime = c(as.numeric(ttt))
      else{traveltime = c(traveltime,as.numeric(ttt))}
      
    }
    
    time =c(5,10,15,20,25,30,35,40,45,50,55,60)
    frompp = cbind(time,traveltime)
    
    frompp=as.data.table(frompp)
    frompp=as.data.frame(frompp)
    frompp$traveltime=round(as.numeric(frompp$traveltime),2)
    frompp
    
    }
    
    
  })
  
  create_qkvplot_select <- reactive({
    
    selecthour=input$obs_K
    selectdate=input$date_K
    selectdes=input$destination_K
    selectdir=input$direction_K
    
    # selecthour=7
    # selectdate="20150925"
    # selectdes="台北"
    # selectdir="北向"
    
    #data for 底圖
    
    data_qkv_base<-filter(data_qkv,匝道==selectdes & 方向==selectdir)
    data_qkv_base1<-mutate(data_qkv_base,k=Q*12/V)
    data_qkv_base2<-filter(data_qkv_base1,k<=200)
    qkvplot_base<-ggplot(data_qkv_base2,aes(Q,V,colour=k))+geom_point()
    
    #data for 篩選
    
    data_qkv_select=filter(data_qkv_base2,hour==selecthour & date==selectdate & 方向==selectdir)
    data_qkv_select<-select(data_qkv_select,匝道,方向,date,hour,min,Q,V,k)
    qkvplot_select<-ggplot(data_qkv_select,aes(Q,V))+geom_point(colour="pink", size = 4)
    data_qkv_select
    
  })

  output$displot <- renderPlot({
    
    if (input$method =="OD迄點分析"){
      
      mccs<-create_mccs()

      tmccs<-t(mccs)
      tmccs<-as.data.frame(tmccs,stringsAsFactors=F)
      colnames(tmccs)=c(tmccs[1,])
      tmccs=tmccs[-1,]
      
      for(i in 1:ncol(tmccs)){
        tmccs[,i]=as.numeric(tmccs[,i])      
      }
      
      tt<-c("t1","t2","t3","t4")
      tmccs<-cbind(tt,tmccs[,1:ncol(tmccs)])
      
      frompp<-melt(tmccs)
        colnames(frompp)=c("時間","迄點匝道","車輛數")
      
      finallplot<-ggplot(frompp, aes(x=時間,y=車輛數, fill=迄點匝道))+geom_bar(stat="identity")+
        labs(x = "時間", y = "流量(輛/15min)", title = "旅次迄點分布")+theme(plot.title = element_text(size = 18, face = "bold"))
      
    }
    
    if (input$method == "OD起點分析"){ 
      
      mccs<-create_mccs()
      
      tmccs<-t(mccs)
      tmccs<-as.data.frame(tmccs,stringsAsFactors=F)
      colnames(tmccs)=c(tmccs[1,])
      tmccs=tmccs[-1,]
      
      for(i in 1:ncol(tmccs)){
        tmccs[,i]=as.numeric(tmccs[,i])      
      }
      
      tt<-c("t1","t2","t3","t4")
      tmccs<-cbind(tt,tmccs[,1:ncol(tmccs)])
      
      frompp<-melt(tmccs)
      colnames(frompp)=c("時間","起點匝道","車輛數")
      frompp<-group_by(frompp,時間)
      
      finallplot<-ggplot(frompp, aes(x=時間,y=車輛數, fill=起點匝道))+geom_bar(stat="identity")+
        labs(x = "時間", y = "流量(輛/15min)", title = "旅次起點分布")+theme(plot.title = element_text(size = 18, face = "bold"))
      finallplot
    }
    
    if (input$method == "旅行時間分析"){ 

      frompp<-create_frompp()
      selecthour=input$obs
      h=as.numeric(selecthour)
      xxx=paste("時間：",as.character(h),"點",sep="")

      finallplot=ggplot(frompp, aes(x=time, y=traveltime)) +geom_line(aes(colour = "旅行時間", group = 1),size=1.5) + geom_point(size=3)+
        labs(x = xxx, y = "分鐘", title = "旅行時間")

    }
    
      #==========================下匝道流量需求預測分析開始============================  
    if (input$method == "下匝道流量需求預測分析"){
      
      selecthour=input$obs_P
      selectdate=input$date_P
      selectdes=input$destination_P
      selectdir=input$direction
      
      #==========================組成分析開始============================  
      
      #==========================組成分析開始============================  
      
      data_check_o=filter(merger_data_finall_time_P,後端匝道==selectdes)
      data_check_o=filter(data_check_o,arrt_hour==selecthour)
      data_check_o=filter(data_check_o,arrt_date==selectdate)
      data_check_o<-mutate(data_check_o,dir=as.character(substr(desgatenum,8,8)))
      
      if(selectdir=="南向"){
        data_check_o=filter(data_check_o,dir=="S")
      }else{
        data_check_o=filter(data_check_o,dir=="N")
      }
      
      t=1   
      data_check_o_temp<-filter(data_check_o,(arrt_min<=15*t))
      cc<-table(data_check_o_temp$前端匝道)
      mcc<- as.data.frame(cc) 
      
      for(t in 2:4){
        data_check_o_temp<-filter(data_check_o,(arrt_min<=15*t & arrt_min>15*(t-1)))
        cc<-table(data_check_o_temp$前端匝道)
        cc<- as.data.frame(cc)
        mcc<-inner_join(mcc,cc,by="Var1")
      }
      
      colnames(mcc)=c("name","t1","t2","t3","t4")
      mccs<-filter(mcc,t1>3&t2>3&t3>3&t4>30) 
      
      
      #改欄位名      
      
      P_t1 = sum(mccs$t1)/sum(mcc$t1)
      P_t2 = sum(mccs$t2)/sum(mcc$t2)
      P_t3 = sum(mccs$t3)/sum(mcc$t3)
      P_t4 = sum(mccs$t4)/sum(mcc$t4)
      
      P_t=c(P_t1,P_t2,P_t3,P_t4)
      
      #mcc
      
      
      #View(mccs)
      #str(mccs)
      ramps=c(as.character(mccs$name))
      k=length(ramps)
      
      #==========================組成分析完畢============================
      #==========================OD比例開始============================
      
      rate_ramp=c()
      
      for(i in 1:k){
        
        data_rate_of_o=filter(merger_data_finall_time_P,前端匝道==ramps[i])
        data_rate_of_o=filter( data_rate_of_o,arrt_hour==selecthour)
        data_rate_of_o=filter( data_rate_of_o,arrt_date==selectdate)
        data_rate_of_o<-mutate(data_rate_of_o,dir=as.character(substr(desgatenum,8,8)))
        if(selectdir=="南向"){
          data_rate_of_o=filter(data_rate_of_o,dir=="S")
        }else{
          data_rate_of_o=filter(data_rate_of_o,dir=="N")
        }
        
        t=1
        data_rate_of_o_temp<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1))
        data_rate_of_o_temp2<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1)&後端匝道==selectdes)
        rate_ramp=c(rate_ramp,length(data_rate_of_o_temp2$desgatenum)/length(data_rate_of_o_temp$desgatenum))
        t=2
        data_rate_of_o_temp<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1))
        data_rate_of_o_temp2<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1)&後端匝道==selectdes)
        rate_ramp=c(rate_ramp,length(data_rate_of_o_temp2$desgatenum)/length(data_rate_of_o_temp$desgatenum))
        t=3
        data_rate_of_o_temp<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1))
        data_rate_of_o_temp2<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1)&後端匝道==selectdes)
        rate_ramp=c(rate_ramp,length(data_rate_of_o_temp2$desgatenum)/length(data_rate_of_o_temp$desgatenum))
        t=4
        data_rate_of_o_temp<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1))
        data_rate_of_o_temp2<-filter(data_rate_of_o,(arrt_min<=15*t)& arrt_min>15*(t-1)&後端匝道==selectdes)
        rate_ramp=c(rate_ramp,length(data_rate_of_o_temp2$desgatenum)/length(data_rate_of_o_temp$desgatenum))
      }
      #==========================OD比結束============================
      #==========================取得流量與旅行時間資料開始============================
      

      h=as.numeric(selecthour)
      flow_bytime=c()
      
      for (t in 1:12) {
        
        flow=0
        flow_all = 0
        
        for(i in 1:k){
          
          datalist=filter(merger_data_finall_time_P,前端匝道==ramps[i] & 後端匝道==selectdes)
          datalist=filter( datalist,arrt_date==selectdate)
          datalist=filter( datalist,arrt_fulltime>=h*60+5*(t-2) & arrt_fulltime<h*60+5*(t-1))
          trt=c((datalist$arrt_hour-datalist$arrt_hour)*60+(datalist$arrt_min-datalist$stt_min))
          ttt = mean(c(trt))
          
          if(ttt=="NaN"){ttt=0}
          
          
          low = as.numeric(h*60+5*(t-2)-ttt)
          up = as.numeric(h*60+5*(t-1)-ttt)
          
          datalist=filter(merger_data_finall_time_P,前端匝道==ramps[i])
          datalist=filter(datalist,arrt_date==selectdate)
          datalist=filter(datalist,stt_fulltime >=low & stt_fulltime<up)
          datalist<-mutate(datalist,dir=as.character(substr(desgatenum,8,8)))
          if(selectdir=="南向"){
            datalist=filter(datalist,dir=="S")
          }else{
            datalist=filter(datalist,dir=="N")
          }
          
          flow=0
          flow=length(datalist$desgatenum)* rate_ramp[(i-1)*4+trunc(1+((t-1)/3))]
          flow_all = flow_all +flow
          
          ttt=0
          trt=c()
        }
        
        flow_all=trunc(flow_all/P_t[trunc((t-1)/3)+1])
        
        flow_bytime=c(flow_bytime,flow_all)
      }
      
      #==========================取得流量與旅行時間資料結束============================
      
      #==========================取得實際車流資料開始============================
      
      
      
      datalist=filter(data_check_o,arrt_date==selectdate)
      flow_bytime_real=c()
      for(t in 1:12){
       datalist_temp=filter(datalist,arrt_min>=5*(t-1) & arrt_min<5*t)
        flow=length(datalist_temp$desgatenum)
        
        flow_bytime_real = c(flow_bytime_real,flow)
      }
      
      
      time =c(05,10,15,20,25,30,35,40,45,50,55,60)
      View(flow_bytime)
      
      
      #frompp = cbind(time,flow_bytime)

      data1=data.frame(time,flow_bytime, flow_bytime_real)

      tit=paste(selectdes,selectdir,"下匝道流量預測績效比較圖",sep="")
      xxx=paste("時間：",as.character(h),"點",sep="")
      
      
      finallplot=ggplot()+geom_line(data=data1,aes(x = time,y = flow_bytime,colour = "預測流量"),size=1.5)+geom_line(data=data1,aes(x = time,y = flow_bytime_real,colour = "實際流量"),size=1.5) +
        scale_x_continuous(breaks = seq(0, 60, 10))+
        labs(x = xxx, y = "流量(輛/5min)", title = tit)+theme(plot.title = element_text(size = 18, face = "bold"))+ylim(min(flow_bytime,flow_bytime_real)-20,max(flow_bytime,flow_bytime_real)+20)

      finallplot
      
      #finallplot=plot(time,flow_bytime,type="l",ylim=c(min(flow_bytime,flow_bytime_real),max(flow_bytime,flow_bytime_real)),
      #                xlab=xxx,ylab="流量",main="下匝道流量預測績效比較圖",sub="黑色為預測流量；紅色為實際流量")
    
      #finallplot=finallplot+points(time,flow_bytime)
      #finallplot=finallplot+lines(time,flow_bytime_real,col="red")+points(time,flow_bytime_real,col="red")

      rm(merger_data_finall_time_P)
      rm(data_check_o)
      rm(data_check_o_temp)
      rm(data_rate_of_o)
      rm(data_rate_of_o_temp)
      rm(data_rate_of_o_temp2)
      rm(data1)
      rm(datalist)
      rm(mcc)
      rm(mccs)
      rm(low)
      rm(up)
      rm(P_t)
      rm(P_t1)
      rm(P_t2)
      rm(P_t3)
      rm(P_t4)
      rm(rate_ramp)
      rm(flow)
      rm(flow_all)
      rm(flow_bytime)
      rm(flow_bytime_real)
      rm(h)
      rm(k)
      rm(ramps)
      rm(trt)
      rm(ttt)
      rm(xxx)
      
    }  
    
    
    if (input$method =='下匝道路段臨界密度分析'){
      
      selecthour=input$obs_K
      selectdate=input$date_K
      selectdes=input$destination_K
      selectdir=input$direction_K
      
      #data for 底圖
      
      data_qkv_base<-filter(data_qkv,匝道==selectdes & 方向==selectdir)
      data_qkv_base1<-mutate(data_qkv_base,k=Q*12/V)
      data_qkv_base2<-filter(data_qkv_base1,k<=200)
      qkvplot_base<-ggplot(data_qkv_base2,aes(Q,V,colour=k))+geom_point(pch="@",bg="darkgreen",cex=10,lwd=5)
      
      #data for 篩選
      
      data_qkv_select=filter(data_qkv_base2,hour==selecthour & date==selectdate & 方向==selectdir)
      data_qkv_select<-select(data_qkv_select,匝道,方向,date,hour,min,Q,V,k)
      qkvplot_select<-ggplot(data_qkv_select,aes(Q,V))+geom_point(colour="Red", size = 4)
      
      finallplot<-ggplot()+geom_point(data=data_qkv_select,aes(Q,V),colour="Red", size = 4)+geom_point(data=data_qkv_base2,aes(Q,V,color=k))
      finallplot<-finallplot+labs(x = "流量(輛/5分鐘", y = "速度(公里/小時)", title = "鄰近匝道路段外側車道臨界密度(k)分析")+theme(plot.title = element_text(size = 18, face = "bold"))
      
    }
    
    
    #匯出最終圖型
    finallplot
    
  }, width = 600, height = 500)
  
  output$mapplot <- renderPlot({
    
    gate_xy$匝道<-as.character(gate_xy$匝道)    
    library(ggmap)
    
    if(input$method =="OD迄點分析"||input$method =="OD起點分析"||input$method =="下匝道流量需求預測分析"){

    if(input$method =="OD迄點分析"||input$method =="OD起點分析"){ 
        mccs<-create_mccs()
        selectdes=input$destination
        selecto=input$origin
     }
    if(input$method =="下匝道流量需求預測分析"){
      mccs<-create_mccsforlast() 
      selectdes=input$destination_P
      selecto=input$origin_P 
    }

  
#     file4 ="D:/etag競賽資料/xydata.csv"
#     gate_xy<-read.table(file4, header=T,sep = ",", encoding="utf-8")

    colnames(mccs)=c("匝道","t1","t2","t3","t4")
    mccs$匝道<-as.character(mccs$匝道)
    mapmergedata<-left_join(mccs,gate_xy,by="匝道")
    mapmergedata<-mutate(mapmergedata,av=(mapmergedata$t1+mapmergedata$t2+mapmergedata$t3+mapmergedata$t4)/4)
    mapmergedata<-select(mapmergedata,lon,lat,av)
    
    
    if (input$method =="OD迄點分析"){
      
      ceterplace=selecto
      colorcenterpoint="red3"
      colorothers="green4"
    }
      
    if (input$method =="OD起點分析"||input$method =="下匝道流量需求預測分析"){
      
      ceterplace=selectdes
      colorcenterpoint="green4"
      colorothers="red3"
    } 
      
        originmap<-filter(gate_xy,匝道==ceterplace)
        #View(originmap)
        centerpointct<-c(originmap$lon,originmap$lat)
        centerpointmk=data.frame(lon=originmap$lon,
                                 lat=originmap$lat)


    map.base<-get_googlemap(center=centerpointct, 
                            color = "bw",
                            zoom=11,
                            maptype='roadmap', 
                            extent='device')
    
    map.base2 <- ggmap(map.base, extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1)
    
    rm(map.finall)
    
    
    ssize_v=c()
    
    for(x in 1:nrow(mapmergedata)){
      if(mapmergedata[x,3]/5<5) {size=5
      ssize_v=c(ssize_v,size)} #依流量大小決定Pin size大小
      else{ size=mapmergedata[x,3]/5
      ssize_v=c(ssize_v,size)};
    }
    
    
    xx_v=c(1:nrow(mapmergedata))
    x_size=data.frame(xx_v,ssize_v)
    
    x_size=x_size[order(x_size$ssize_v,decreasing = TRUE),]
    Colors=grDevices::rainbow(nrow(mapmergedata))
    
    for(i in 1:nrow(mapmergedata)){
    
                if(i==1) map.finall<-map.base2+
          geom_point(data=mapmergedata[x_size$xx_v[i],], aes(x=lon, y=lat), colour=Colors[i], alpha=0.5,size=x_size$ssize_v[i])
      else{
        map.finall<-map.finall+
          geom_point(data=mapmergedata[x_size$xx_v[i],], aes(x=lon, y=lat), colour=Colors[i], alpha=0.5,size=x_size$ssize_v[i])+
          geom_point(data=centerpointmk, aes(x=lon, y=lat), colour="darkgreen", alpha=0.5,cex=10,pch="@",bg="darkgreen",lwd=5)
      }
    
    }
    
    rm(xx_v)
    rm(ssize_v)
    rm(x_size)
    rm(Colors)
    
    
    }
    
    if (input$method == "旅行時間分析"){
      
      selectdes=input$destination_T
      selecto=input$origin_T
  
      points<-c(selecto,selectdes)
      points<-as.data.frame(points)
      colnames(points)=c("匝道")
      mapmergedata<-left_join(points,gate_xy,by="匝道")
      colorselectot="red3"
      colorselectdes="green4"
      
      centerpointct<-c(mean(mapmergedata$lon),mean(mapmergedata$lat))
      centerpointm<-data.frame(lon=mean(mapmergedata$lon),
                               lat=mean(mapmergedata$lat))
      #View(mapmergedata)
      
      map.base<-get_googlemap(center=centerpointct, 
                              color = "bw",
                              zoom=11,
                              maptype='roadmap', 
                              extent='device')
      
      map.base2 <- ggmap(map.base, extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1)
      
      #rm(map.finall)
      map.finall<-map.base2+
        geom_point(data=mapmergedata[1,], aes(x=lon, y=lat), colour=colorselectot, alpha=0.5,size=5)+
        geom_point(data=mapmergedata[2,], aes(x=lon, y=lat), colour=colorselectdes, alpha=0.5,size=5)
    
    }
    
    if (input$method =='下匝道路段臨界密度分析'){
      
      selecthour=input$obs_K
      selectdate=input$date_K
      selectdes=input$destination_K
      selectdir=input$direction_K
      
      #畫地圖
      
      points<-c(selectdes)
      points<-as.data.frame(points)
      colnames(points)=c("匝道")
      mapmergedata<-left_join(points,gate_xy,by="匝道")
      colorselectot="red3"
      colorselectdes="green4"
      
      centerpointct<-c(mapmergedata$lon,mapmergedata$lat)
      centerpointm<-data.frame(lon=mapmergedata$lon,
                               lat=mapmergedata$lat)
      #View(mapmergedata)
      
      map.base<-get_googlemap(center=centerpointct, 
                              color = "bw",
                              zoom=11,
                              maptype='roadmap', 
                              extent='device')
      
      map.base2 <- ggmap(map.base, extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1)

      map.finall<-map.base2+
        geom_point(data=centerpointm, aes(x=lon, y=lat), colour=colorselectdes , alpha=0.5,size=5)
      
    }
    
      map.finall
  }, width = 500, height = 500)
  
  output$datatable <- renderDataTable({
    
    if(input$method =="OD迄點分析"){
      
      tabledf<-create_mccs()
    }
    
    if(input$method =="OD起點分析"){
      
      tabledf<-create_mccs()
    }
    
    if(input$method == "旅行時間分析"){
      
      tabledf<-create_frompp()
    }
    
    if(input$method =='下匝道路段臨界密度分析'){
      
      tabledf<-create_qkvplot_select()
    }
    
    if(input$method =="下匝道流量需求預測分析"){
      
      tabledf<-create_mccsforlast()
      
    }
    
    tabledf
    
  }, options = list(aLengthMenu = c(10, 15, 30, 50, 100, 500), iDisplayLength = 15))
  
  #標題=選用功能
#   output$caption <- renderText({
#     input$method
#     
#   })
#   

#   
  output$downloadData1 <- downloadHandler(
    filename = 'myplots.png',
    content = function(file) {
#       png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
#       myplot()
#       
#       dev.off()
    },
    contentType = 'image/png'
  )
  
  output$downloadData2 <- downloadHandler(
    filename = 'mytable.csv',
    content = function(file) {
      #       png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
      #       myplot()
      #       
      #       dev.off()
    },
    contentType = 'csv'
  )
  
  
  
})