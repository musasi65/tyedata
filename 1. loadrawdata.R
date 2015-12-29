
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
#fff<-filter(data_row_time_test2,data_row_time_test2$ETAGPLATEID=="003000000100086762B73292")

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

