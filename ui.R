library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  options(shiny.usecairo = FALSE),
  # Application title.
  titlePanel("高速公路下匝道車流狀況預測系統"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.

  
  sidebarPanel(
    selectInput(inputId="method", 
                    label="功能選擇:", 
                    choices=c("OD迄點分析"="OD迄點分析", 
                              "OD起點分析"="OD起點分析", 
                              "旅行時間分析"="旅行時間分析", 
                              "下匝道路段臨界密度分析"="下匝道路段臨界密度分析",
                              "下匝道流量需求預測分析"="下匝道流量需求預測分析")
                    ,"OD起點分析"),
                    
    
                    helpText("選擇欲分析的項目"),
    conditionalPanel(condition="input.method=='OD迄點分析'",
                     
                     selectInput(inputId="origin", 
                                 label="起點選擇:", 
                                 choices=c("====[北部]===="="====[北部]====","汐止&汐止系統南"="汐止&汐止系統南","內湖"="內湖","圓山"="圓山","台北"="台北","三重"="三重","五股"="五股","南港"="南港","新店"="新店","中和"="中和","土城"="土城","鶯歌系統"="鶯歌系統","龍潭"="龍潭","====[中部]===="="====[中部]====","豐原"="豐原","台中"="台中","南屯"="南屯","====[南部]===="="====[南部]====","台南"="台南","岡山"="岡山","高雄北"="高雄北")
                                 ,"中和"),
                     
                     selectInput(inputId="date1", "分析日期:", 
                                 choices = c("20150907"="20150907","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
                     
                     sliderInput(inputId="obs1", 
                                 label="時間", 
                                 min = 0, max = 23, value = 7, step=1),
                     helpText("分析旅次迄點交流道分布")
                     
                     
    ),
    
    conditionalPanel(condition="input.method=='OD起點分析'",
                     
                     selectInput(inputId="destination", 
                                 label="迄點選擇:",
                                 choices=c("====[北部]===="="====[北部]====","汐止&汐止系統南"="汐止&汐止系統南","內湖"="內湖","圓山"="圓山","台北"="台北","三重"="三重","五股"="五股","南港"="南港","新店"="新店","中和"="中和","土城"="土城","鶯歌系統"="鶯歌系統","龍潭"="龍潭","====[中部]===="="====[中部]====","豐原"="豐原","台中"="台中","南屯"="南屯","====[南部]===="="====[南部]====","台南"="台南","岡山"="岡山","高雄北"="高雄北")
                                 ,"中和"),
                     selectInput(inputId="date2", "分析日期:", 
                                 choices = c("20150907"="20150907","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
                     
                     sliderInput(inputId="obs2", 
                                 label="時間", 
                                 min = 0, max = 23, value = 7, step=1),
                     helpText("分析旅次起點交流道分布")
    ),
    
    conditionalPanel(condition="input.method=='旅行時間分析'",
                     selectInput(inputId="origin_T", 
                                 label="起點選擇:", 
                                 choices=c("====[北部]===="="====[北部]====","汐止&汐止系統南"="汐止&汐止系統南","內湖"="內湖","圓山"="圓山","台北"="台北","三重"="三重","五股"="五股","南港"="南港","新店"="新店","中和"="中和","土城"="土城","鶯歌系統"="鶯歌系統","龍潭"="龍潭","====[中部]===="="====[中部]====","豐原"="豐原","台中"="台中","南屯"="南屯","====[南部]===="="====[南部]====","台南"="台南","岡山"="岡山","高雄北"="高雄北")
                     ),
                     selectInput(inputId="destination_T", 
                                 label="迄點選擇:", 
                                 choices=c("====[北部]===="="====[北部]====","汐止&汐止系統南"="汐止&汐止系統南","內湖"="內湖","圓山"="圓山","台北"="台北","三重"="三重","五股"="五股","南港"="南港","新店"="新店","中和"="中和","土城"="土城","鶯歌系統"="鶯歌系統","龍潭"="龍潭","====[中部]===="="====[中部]====","豐原"="豐原","台中"="台中","南屯"="南屯","====[南部]===="="====[南部]====","台南"="台南","岡山"="岡山","高雄北"="高雄北")
                     ),
                     selectInput(inputId="date", "分析日期:", 
                                 choices = c("20150907"="20150907","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
                     
                     sliderInput(inputId="obs", 
                                 label="時間", 
                                 min = 0, max = 23, value = 7, step=1),
                     helpText("分析旅次起迄點間的旅行時間")
    ),
    
    conditionalPanel(condition="input.method=='下匝道路段臨界密度分析'",
                     selectInput(inputId="destination_K", 
                                 label="分析交流道:", 
                                 choices=c("台北"="台北","中和"="中和","龍潭"="龍潭"),
                                 "台北"
                     ),
                     selectInput(inputId="direction_K", 
                                 label="南向/北向:", 
                                 choices=c("南向"="南向","北向"="北向")
                                 
                     ),
                     selectInput(inputId="date_K", "分析日期:", 
                                 choices = c("20150925"="20150925","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
                     
                     sliderInput(inputId="obs_K", 
                                 label="時間", 
                                 min = 0, max = 23, value = 7, step=1),
                     helpText("分析下匝道鄰近路段外側車道之車流三相圖")
    ),      
    
    conditionalPanel(condition="input.method=='下匝道流量需求預測分析'",
                     selectInput(inputId="destination_P", 
                                 label="分析交流道:", 
                                 choices=c("====[北部]===="="====[北部]====","汐止&汐止系統南"="汐止&汐止系統南","內湖"="內湖","圓山"="圓山","台北"="台北","三重"="三重","五股"="五股","南港"="南港","新店"="新店","中和"="中和","土城"="土城","鶯歌系統"="鶯歌系統","龍潭"="龍潭","====[中部]===="="====[中部]====","豐原"="豐原","台中"="台中","南屯"="南屯","====[南部]===="="====[南部]====","台南"="台南","岡山"="岡山","高雄北"="高雄北"),
                                 "台北"
                     ),
                     selectInput(inputId="direction", 
                                 label="方向:", 
                                 choices=c("南向"="南向","北向"="北向")
                     
                      ),
                     selectInput(inputId="date_P", "分析日期:", 
                                 choices = c("20150907"="20150907","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
                     
                     sliderInput(inputId="obs_P", 
                                 label="時間", 
                                 min = 0, max = 23, value = 7, step=1),
                     helpText("模式預測流量與實際流量之誤差分析")
     ),
    downloadButton('downloadData1', 'Download image'),
    downloadButton('downloadData2', 'Download table')
  ),
  
#   sidebarPanel(
#     selectInput(inputId="date", "分析日期:", 
#                 choices = c("20150907"="20150907","20150908"="20150908","20150909"="20150909","20150910"="20150910","20150911"="20150911")),
#     
#     sliderInput(inputId="obs", 
#                 label="時間", 
#                 min = 0, max = 23, value = 16, step=1)
#     
#      submitButton("Update View")
#   ),
  
  
  
    mainPanel( 
      tabsetPanel(
      #h3(textOutput("caption", container = span)),
        #tabPanel("功能簡介",),
        
        tabPanel("圖形分析",plotOutput("mapplot"),
                 #downloadButton('downloadData', 'Download'),
                 plotOutput("displot")
                 
                 ),
        
        tabPanel("數值分析", dataTableOutput("datatable"))
        
      

      )
    )  
))