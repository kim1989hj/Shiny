## app.R ##
library(shiny)
library(shinydashboard)
library(rJava)
library(DBI)
library(RJDBC)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(xlsx)
library(gdata)
library(zoo)
library(bit64)

#install.packages("bit64")
#install.packages("zoo")
#
#install.packages("devtools")
#library(devtools)
#install_github("nik01010/dashboardthemes")

## ORACLE DATABASE 연동



options(shiny.maxRequestSize=30*1024^2)

jdbcDriver<-JDBC(driverClass = "oracle.jdbc.OracleDriver",
                 classPath = "C:/app/hjkim/product/11.2.0/dbhome_1/LIB/ojdbc6.jar" )

con<-dbConnect(jdbcDriver,"jdbc:oracle:thin:@127.0.0.1:1521:orcl","u1","1234")

paste0("select DISTINCT(P_NAME) from info_detail") -> query_list

result_list=dbGetQuery(con,query_list)


ui <- dashboardPage( skin = "yellow",
    dashboardHeader(title="KECC", titleWidth = 300),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            
            # id="side1",
            # menuItem("Data INPUT", tabName = "View1", icon = icon("th")),
            # #menuItem("분석 시스템 (과거 데이터)", tabName = "View2", icon = icon("th")),
            # menuItem("분석 시스템 (과거 데이터)",icon = icon("dashboard"),
            #          menuSubItem("추정예가 / 기술순위 분포",icon=icon("dashboard"), tabName = "View2"),
            #          menuSubItem("분석 화면 2",icon=icon("dashboard")),
            #          menuSubItem("분석 화면 3",icon=icon("dashboard")),
            #          menuSubItem("분석 화면 4",icon=icon("dashboard"))),
            # menuItem("참여기업별 예정가 계산", tabName = "View3", icon = icon("th")),
            # menuItem("UP & DOWN 예측 시뮬레이션", tabName = "View4", icon = icon("dashboard"))
            # 
            
            id="sbm",
            sidebarMenuOutput("menu")),
            conditionalPanel(condition = "input.sbm == 'View2'",
                             hr(),
                             #h4(" [[조건 설정]] "  ),
                             selectInput("input_company","기업 선택",
                                         c("도화"="도화",
                                           "KECC"="KECC",
                                           "삼안"="삼안",
                                           "건화"="건화",
                                           "이산"="이산",
                                           "동명"="동명",
                                           "유신"="유신",
                                           "동부"="동부",
                                           "경호"="경호",
                                           "선진"="선진",
                                           "수성"="수성",
                                           "서영"="서영",
                                           "제일"="제일",
                                           "경동"="경동",
                                           "평화"="평화",
                                           "동일"="동일",
                                           "범한"="범한",
                                           "건일"="건일",
                                           "천일"="천일",
                                           "KG"="KG"
                                           
                                         )),
                             
                             selectInput("competition_company","비교 기업1 선택",
                                         c(" "="선택전",
                                           "도화"="도화",
                                           "KECC"="KECC",
                                           "삼안"="삼안",
                                           "건화"="건화",
                                           "이산"="이산",
                                           "동명"="동명",
                                           "유신"="유신",
                                           "동부"="동부",
                                           "경호"="경호",
                                           "선진"="선진",
                                           "수성"="수성",
                                           "서영"="서영",
                                           "제일"="제일",
                                           "경동"="경동",
                                           "평화"="평화",
                                           "동일"="동일",
                                           "범한"="범한",
                                           "건일"="건일",
                                           "천일"="천일",
                                           "KG"="KG"

                                         ),selected=NULL),
                             
                             selectInput("competition_company2","비교 기업2 선택",
                                         c(" "="선택전",
                                           "도화"="도화",
                                           "KECC"="KECC",
                                           "삼안"="삼안",
                                           "건화"="건화",
                                           "이산"="이산",
                                           "동명"="동명",
                                           "유신"="유신",
                                           "동부"="동부",
                                           "경호"="경호",
                                           "선진"="선진",
                                           "수성"="수성",
                                           "서영"="서영",
                                           "제일"="제일",
                                           "경동"="경동",
                                           "평화"="평화",
                                           "동일"="동일",
                                           "범한"="범한",
                                           "건일"="건일",
                                           "천일"="천일",
                                           "KG"="KG"
                                           
                                         ),selected=NULL)
            )
            
            
            
        ),
    dashboardBody(
        tags$style(HTML(".sidebar-menu .treeview-menu{     padding: 0 0 0 50px;}")),
        tabItems(
            tabItem(tabName="View1",
                        fileInput("selFile","File Choose"),
                        verbatimTextOutput("txt"),
                        verbatimTextOutput("txt2")
                        
                    ),
            
            tabItem(tabName = "View2",
                    # tags$table(
                    # tags$tr(
                    #     tags$th(
                    # selectInput("input_company","기업 선택",
                    #             c("도화"="도화",
                    #               "KECC"="KECC",
                    #               "삼안"="삼안",
                    #               "건화"="건화",
                    #               "이산"="이산",
                    #               "동명"="동명",
                    #               "유신"="유신",
                    #               "동부"="동부",
                    #               "경호"="경호",
                    #               "선진"="선진",
                    #               "수성"="수성",
                    #               "서영"="서영",
                    #               "제일"="제일",
                    #               "경동"="경동",
                    #               "평화"="평화",
                    #               "동일"="동일",
                    #               "범한"="범한",
                    #               "건일"="건일",
                    #               "천일"="천일",
                    #               "KG"="KG"
                    #               
                    #               ))),
                    # tags$th(
                    # selectInput("competition_company","비교 기업1 선택",
                    #             c(" "="선택전",
                    #               "도화"="도화",
                    #               "KECC"="KECC",
                    #               "삼안"="삼안",
                    #               "건화"="건화",
                    #               "이산"="이산",
                    #               "동명"="동명",
                    #               "유신"="유신",
                    #               "동부"="동부",
                    #               "경호"="경호",
                    #               "선진"="선진",
                    #               "수성"="수성",
                    #               "서영"="서영",
                    #               "제일"="제일",
                    #               "경동"="경동",
                    #               "평화"="평화",
                    #               "동일"="동일",
                    #               "범한"="범한",
                    #               "건일"="건일",
                    #               "천일"="천일",
                    #               "KG"="KG"
                    #               
                    #             ),selected=NULL)),
                    # tags$th(
                    #     selectInput("competition_company2","비교 기업2 선택",
                    #                 c(" "="선택전",
                    #                   "도화"="도화",
                    #                   "KECC"="KECC",
                    #                   "삼안"="삼안",
                    #                   "건화"="건화",
                    #                   "이산"="이산",
                    #                   "동명"="동명",
                    #                   "유신"="유신",
                    #                   "동부"="동부",
                    #                   "경호"="경호",
                    #                   "선진"="선진",
                    #                   "수성"="수성",
                    #                   "서영"="서영",
                    #                   "제일"="제일",
                    #                   "경동"="경동",
                    #                   "평화"="평화",
                    #                   "동일"="동일",
                    #                   "범한"="범한",
                    #                   "건일"="건일",
                    #                   "천일"="천일",
                    #                   "KG"="KG"
                    #                   
                    #                 ),selected=NULL))
                    # 
                    # 
                    # 
                    # )),
                    
                    DT::dataTableOutput("s_company"),
                    
                    
                    
                    fluidRow(
                        box(plotOutput("rate"), background = "black"),
                        box(plotOutput("yega"), background = "black")
                    )
                    
                    #selectInput("input_project_name","프로젝트 이름 선택",result_list)
                    
                    
                    
                    
            ),
            
            tabItem(tabName = "View3",
                        
                        tags$table(
                            tags$tr(
                                tags$th(selectInput("input_project_name","프로젝트 이름 선택",result_list,width="100%",selected=NULL)),
                                tags$th(downloadButton("downloadData", "Download")))),
                        
                        tags$table(
                            tags$tr(
                                
                                
                                #tags$th(numericInput("g_num","기술점수",50,0,100)),
                                #tags$th(numericInput("p_num","가격점수",50,0,100)),
                                #tags$th(numericInput("s_num","적격심사점수",90,0,100)),
                                tags$th(tags$h4("기술점수 : ")),
                                tags$th(verbatimTextOutput("g_num")),
                                tags$th(tags$h4("가격점수 : ")),
                                tags$th(verbatimTextOutput("p_num")),
                                tags$th(tags$h4("적격심사점수 : ")),
                                tags$th(verbatimTextOutput("s_num")),
                                tags$th(radioButtons("updown","상/하한율",choices = c("102.00~98.00","103.00~97.00")))
                            )),
                    
                    DT::dataTableOutput("calc")
                    
                        
                       
                    
            ),
            
            tabItem(tabName = "View4",
                    h2("Up Down 시뮬레이션 - TBD")
                    
            )
        )
        
    )
)

server <- function(input, output, session) {
    calc_ori_df <- reactiveValues()
    calc_ori_df=structure(list(character()), class = "data.frame")
    
    output$tb1<-renderDataTable({
        

        
        
        #req(input$selFile)
        #readxl::read_xls(input$selFile$datapath)
    })
    
    ############################ meta ############################
    output$txt<-renderText({
        req(input$selFile)
        #aa<-read.xlsx2(input$selFile$datapath,sheetIndex=1)
        
        aa<-read.csv(input$selFile$datapath,header = TRUE)
        #aa<-read.csv("C:/2010-2020.1_v1.csv",header = TRUE)
        
        aa$기초금액<-as.numeric(gsub(",","",aa$기초금액))
        aa$예정금액<-as.numeric(gsub(",","",aa$예정금액))
        aa$낙찰금액<-as.numeric(gsub(",","",aa$낙찰금액))
        
        #as.Date(aa$공고일[1],'YYYY/MM/DD')
        
        #readxl::read_xls(input$selFile$datapath)
        #aa<-read.xlsx2('C:/2010-2020.1.xls',sheetIndex=1)
        
        #bb<-readxl::read_xls("C:/2018년도 일찰결과(개발업체).xls")
        
        #aa$지역[100]
        
        #aa$기술점수[2]
        
        cc<-dim(aa)[1]
        cc<-as.numeric(cc)
        for (i in 1:cc){
            C_COMPANY<-aa$발주처[i]
            P_NAME<-aa$사업명[i]
            P_NAME<-gsub("'","",P_NAME)
            O_PRICE<-aa$기초금액[i]
            I_DATE<-aa$공고일[i]
            S_DATE<-aa$제출일[i]
            R_DATE<-aa$입찰일[i]
            P_PRICE<-aa$예정금액[i]
            
            ## 신규추가 20200214
            P_G_NUM<-aa$기술점수[i]
            P_P_NUM<-aa$적격심사[i]
            P_REGION<-aa$지역[i]
            
            W_COMPANY<-aa$낙찰사[i]
            W_PRICE<-aa$낙찰금액[i]
            W_JUMSU<-aa$낙찰기술점수[i]
            W_YEGA<-aa$낙찰예가율[i]
        
            # query<-paste0("insert into meta VALUES('",C_COMPANY,"','",
            #          P_NAME,"',",O_PRICE,",'",I_DATE,"','",S_DATE,"','",
            #          R_DATE,"',",P_PRICE,",",P_G_NUM,",'",P_REGION,"','",W_COMPANY,"',",
            #          W_PRICE,",",W_JUMSU,",",W_YEGA,")")
            
            query<-paste0("insert into meta VALUES('",C_COMPANY,"','",
                          P_NAME,"',",O_PRICE,",'",I_DATE,"','",S_DATE,"','",
                          R_DATE,"',",P_PRICE,",",P_G_NUM,",",P_P_NUM,",'",P_REGION,"','",W_COMPANY,"',",
                          W_PRICE,",",W_JUMSU,",",W_YEGA,")")
            
            dbSendUpdate(con,query[1])
            
            query<-""
            
        }
        
        
    })
    
    
    ######################### input INFO_DETAIL ###########################
    
    output$txt2<-renderText({
        req(input$selFile)
        aa<-read.xlsx2(input$selFile$datapath,sheetIndex=1)
        #aa<-read.xlsx2("C:/2010-2020.1.xls",sheetIndex=1)
        #aa<-read.csv("C:/2010-2020.1_v1.csv",header = TRUE)
        cc<-dim(aa)[1]
        cc<-as.numeric(cc)
        
        #as.character(aa[1,"X10순위"])==""
        
        
        #is.nan(aa[1,"X1순위"])
        
        for (i in 1:cc){
            P_NAME<-aa$사업명[i]
            P_NAME<-gsub("'","",P_NAME)
            for(j in 1:30){
                    tmp_M_COMPANY<-paste0("X",j,"순위")
                    tmp_JUMSU<-paste0('점수',j)
                    tmp_R_PRICE<-paste0('투찰',j)
                    tmp_P_YEGA<-paste0('추정예가',j)
                    if(isTRUE(as.character(aa[i,tmp_M_COMPANY])=="")==FALSE){  
                        M_COMPNAY<-as.character(aa[i,tmp_M_COMPANY])
                        RATE<-j
                        JUMSU<-aa[i,tmp_JUMSU]
                        R_PRICE<-as.numeric(gsub(",","",aa[i,tmp_R_PRICE]))
                        P_YEGA<-aa[i,tmp_P_YEGA]
                        
                        
                        
                        query2<-paste0("insert into info_detail VALUES('",P_NAME,"','",
                                   M_COMPNAY,"',",RATE,",",JUMSU,",",R_PRICE,",",
                                   P_YEGA,")")
                    
                        dbSendUpdate(con,query2[1])
                    
                        query2<-""
                }
            }
        }
    })
    
    
    ###################### INPUT COMPANY #####################
    
    
    output$s_company <- renderDataTable({
            i_c<-input$input_company
            #i_c<-"도화"
            #paste0("select C_COMPANY,P_NAME,O_PRICE,I_DATE,S_DATE,R_DATE,P_PRICE,W_COMPANY,W_PRICE,W_JUMSU,W_YEGA from meta where W_COMPANY = '",i_c,"'") -> query
            
            paste0("select C_COMPANY,P_NAME,to_char(O_PRICE,'FM999,999,999,999') as O_PRICE,I_DATE,S_DATE,R_DATE,to_char(P_PRICE,'FM999,999,999,999') as P_PRICE,W_COMPANY,to_char(W_PRICE,'FM999,999,999,999') as W_PRICE,W_JUMSU,W_YEGA from meta where W_COMPANY = '",i_c,"'") -> query
            result=dbGetQuery(con,query)
            
            colnames(result)=c("발주처","사업명","기초금액","공고일","제출일","입찰일","예정금액","낙찰사","낙찰금액","낙찰기술점수","낙찰예가율")
            
            result        
    })
    
    
    
    ################### 막대그래프 - 기업별 기술점수 순위 분포 ###################
    
    
    output$rate = renderPlot({
            #### 메인기업 ####
            i_d<-input$input_company
            #i_d<-"도화"
            #c_d<-"KECC"
            query_first = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE = 1")
            query_second = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE = 2")
            query_third = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE = 3")
            query_four = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE = 4")
            query_five = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE = 5")
            query_etc = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",i_d,"' and M_COMPANY = '",i_d,"' and RATE NOT IN (1,2,3,4,5)")
            
            
            
            result_first = dbGetQuery(con,query_first)
            result_second = dbGetQuery(con,query_second)
            result_third = dbGetQuery(con,query_third)
            result_four = dbGetQuery(con,query_four)
            result_five = dbGetQuery(con,query_five)
            result_etc = dbGetQuery(con,query_etc)
            ##################
            
            
            #### 경쟁업체 ####
            
            c_d<-input$competition_company
            
            query_first_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE = 1")
            query_second_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE = 2")
            query_third_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE = 3")
            query_four_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE = 4")
            query_five_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE = 5")
            query_etc_c = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",c_d,"' and M_COMPANY = '",c_d,"' and RATE NOT IN (1,2,3,4,5)")
            
            
            
            result_first_c = dbGetQuery(con,query_first_c)
            result_second_c = dbGetQuery(con,query_second_c)
            result_third_c = dbGetQuery(con,query_third_c)
            result_four_c = dbGetQuery(con,query_four_c)
            result_five_c = dbGetQuery(con,query_five_c)
            result_etc_c = dbGetQuery(con,query_etc_c)
            
            
            #### 경쟁업체2 ####
            
            t_d<-input$competition_company2
            
            query_first_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE = 1")
            query_second_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE = 2")
            query_third_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE = 3")
            query_four_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE = 4")
            query_five_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE = 5")
            query_etc_t = paste0("select count(*) from info_detail t1, meta t2 where t1.p_name = t2.p_name and W_COMPANY = '",t_d,"' and M_COMPANY = '",t_d,"' and RATE NOT IN (1,2,3,4,5)")
            
            
            
            result_first_t = dbGetQuery(con,query_first_t)
            result_second_t = dbGetQuery(con,query_second_t)
            result_third_t = dbGetQuery(con,query_third_t)
            result_four_t = dbGetQuery(con,query_four_t)
            result_five_t = dbGetQuery(con,query_five_t)
            result_etc_t = dbGetQuery(con,query_etc_t)
            
            
            ##################
            
            if(c_d=="선택전" && t_d=="선택전" ){
                group<-c(i_d,i_d,i_d,i_d,i_d,i_d)
                count<-c(as.numeric(result_first),as.numeric(result_second),as.numeric(result_third),as.numeric(result_four),as.numeric(result_five),as.numeric(result_etc))
                info<-factor(c("1위","2위","3위","4위","5위","나머지"), levels = c("1위","2위","3위","4위","5위","나머지"))
                result_frame = data.frame(group,count,info)
                ggplot(data=result_frame,aes(x=info,y=count,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=info,y=count,label=count),position = position_dodge(width = 1),
                                                                                                                                       vjust = -0.5, size = 5)+xlab("순위")+ylab("낙찰 횟수")+ggtitle(paste0(i_d ,"[[ 낙찰 시 기술점수 순위 분포 ]]"))
            }else if(c_d!="선택전" && t_d=="선택전"){
                group<-c(i_d,i_d,i_d,i_d,i_d,i_d,c_d,c_d,c_d,c_d,c_d,c_d)
                count<-c(as.numeric(result_first),as.numeric(result_second),as.numeric(result_third),as.numeric(result_four),as.numeric(result_five),as.numeric(result_etc),
                         as.numeric(result_first_c),as.numeric(result_second_c),as.numeric(result_third_c),as.numeric(result_four_c),as.numeric(result_five_c),as.numeric(result_etc_c))
                info<-factor(c("1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지"), levels = c("1위","2위","3위","4위","5위","나머지"))
                result_frame = data.frame(group,count,info)   
                ggplot(data=result_frame,aes(x=info,y=count,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=info,y=count,label=count),position = position_dodge(width = 1),
                                                                                                                                       vjust = -0.5, size = 5)+xlab("순위")+ylab("낙찰 횟수")+ggtitle(paste0(i_d," vs ",c_d," [[ 낙찰 시 기술점수 순위 분포 ]]"))
            }else if(c_d=="선택전" && t_d!="선택전"){
                group<-c(i_d,i_d,i_d,i_d,i_d,i_d,t_d,t_d,t_d,t_d,t_d,t_d)
                count<-c(as.numeric(result_first),as.numeric(result_second),as.numeric(result_third),as.numeric(result_four),as.numeric(result_five),as.numeric(result_etc),
                         as.numeric(result_first_t),as.numeric(result_second_t),as.numeric(result_third_t),as.numeric(result_four_t),as.numeric(result_five_t),as.numeric(result_etc_t))
                info<-factor(c("1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지"), levels = c("1위","2위","3위","4위","5위","나머지"))
                result_frame = data.frame(group,count,info)
                ggplot(data=result_frame,aes(x=info,y=count,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=info,y=count,label=count),position = position_dodge(width = 1),
                                                                                                                                       vjust = -0.5, size = 5)+xlab("순위")+ylab("낙찰 횟수")+ggtitle(paste0(i_d," vs ",t_d," [[ 낙찰 시 기술점수 순위 분포 ]]"))
            }else{
                group<-c(i_d,i_d,i_d,i_d,i_d,i_d,c_d,c_d,c_d,c_d,c_d,c_d,t_d,t_d,t_d,t_d,t_d,t_d)
                count<-c(as.numeric(result_first),as.numeric(result_second),as.numeric(result_third),as.numeric(result_four),as.numeric(result_five),as.numeric(result_etc),
                         as.numeric(result_first_c),as.numeric(result_second_c),as.numeric(result_third_c),as.numeric(result_four_c),as.numeric(result_five_c),as.numeric(result_etc_c),
                         as.numeric(result_first_t),as.numeric(result_second_t),as.numeric(result_third_t),as.numeric(result_four_t),as.numeric(result_five_t),as.numeric(result_etc_t))
                info<-factor(c("1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지"), levels = c("1위","2위","3위","4위","5위","나머지"))
                result_frame = data.frame(group,count,info)    
                ggplot(data=result_frame,aes(x=info,y=count,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=info,y=count,label=count),position = position_dodge(width = 1),
                                                                                                                                       vjust = -0.5, size = 5)+xlab("순위")+ylab("낙찰 횟수")+ggtitle(paste0(i_d," vs ",c_d," vs", t_d,"[[ 낙찰 시 기술점수 순위 분포 ]]"))
                }
            #group<-c(i_d,i_d,i_d,i_d,i_d,i_d,c_d,c_d,c_d,c_d,c_d,c_d)
            #count<-c(as.numeric(result_first),as.numeric(result_second),as.numeric(result_third),as.numeric(result_four),as.numeric(result_five),as.numeric(result_etc),
            #         as.numeric(result_first_c),as.numeric(result_second_c),as.numeric(result_third_c),as.numeric(result_four_c),as.numeric(result_five_c),as.numeric(result_etc_c),
            #         as.numeric(result_first_t),as.numeric(result_second_t),as.numeric(result_third_t),as.numeric(result_four_t),as.numeric(result_five_t),as.numeric(result_etc_t))
            #info<-factor(c("1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지","1위","2위","3위","4위","5위","나머지"), levels = c("1위","2위","3위","4위","5위","나머지"))
            
            #result_frame = data.frame(group,count,info)   
            
            
            #ggplot(data=result_frame,aes(x=info,y=count,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=info,y=count,label=count),position = position_dodge(width = 1),
            #                                                                                                                       vjust = -0.5, size = 5)+xlab("순위")+ylab("낙찰 횟수")+ggtitle(paste0(i_d," vs ",c_d," [[ 낙찰 시 기술점수 순위 분포 ]]"))
            
            
            })
    
    ####### CALC EXEL SHEET ###########
    
    output$calc = renderDataTable({
        
        p_name = input$input_project_name
        
        #p_name = '득량도,첨도 도서지역 식수원 개발사업 기본 및 실시설계용역'
        
        query_calc <- paste0("select M_COMPANY,JUMSU from info_detail
                             where P_NAME = '",p_name,"'")
        
        calc_ori = dbGetQuery(con,query_calc)
        
        calc_ori_p_name = calc_ori$M_COMPANY
        calc_ori_g_num = calc_ori$JUMSU
        
        
        ### 2020-02-17 신규 g_num, p_num, s_num
        
        
        i_p_name = as.character(input$input_project_name)
        query = paste0("select P_G_NUM from meta where P_NAME like '%",i_p_name,"%'")
        
        query2 = paste0("select P_P_NUM from meta where P_NAME like '%",i_p_name,"%'")
        
        # 신규 g_num, p_num, s_num
        g_num=as.numeric(dbGetQuery(con,query))
        p_num=100-g_num
        s_num=as.numeric(dbGetQuery(con,query2))
        
        
        
        
        
        #g_num = as.numeric(input$g_num)
        
        #g_num = 70
        
        #p_num = 100-g_num
        
        #s_num = 95
        #s_num = as.numeric(input$s_num)
        
        g_num_percent = g_num/100
        #g_num = 70
        
        calc_ori_h_g_num = calc_ori$JUMSU*g_num/100
        calc_ori_p_num = s_num - calc_ori_h_g_num
        
        ## 기초금액 
        query_gicho <- paste0("select O_PRICE from meta
                             where P_NAME = '",p_name,"'")
        query_gicho_dbget <- dbGetQuery(con,query_gicho)
        
        calc_ori_row_price <- as.numeric(query_gicho_dbget)
        
        
        
        if(calc_ori_row_price >= 3000000000){
            calc_ori_row_percent <- (88-(p_num-(s_num-calc_ori_g_num*g_num_percent)))-0.005
        }else if(calc_ori_row_price <= 3000000000){
            calc_ori_row_percent <- (88-(p_num-(s_num-calc_ori_g_num*g_num_percent)))-0.005
        }else if(calc_ori_row_price <= 1000000000){
            calc_ori_row_percent <- (88-(p_num-(s_num-calc_ori_g_num*g_num_percent))/2)-0.005
        }else{
            calc_ori_row_percent <- (88-(p_num-(s_num-calc_ori_g_num*g_num_percent))/4)-0.005
        }
        
        
        calc_range <- (88-calc_ori_row_percent)*2
        
        calc_ori_price_1<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_2<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_3<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_4<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_5<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_6<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_7<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_8<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_9<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_10<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_11<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_12<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_13<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_14<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_15<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_16<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_17<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_18<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_19<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_20<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_21<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_22<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_23<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_24<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_25<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_26<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_27<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_28<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_29<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_30<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_31<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_32<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_33<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_34<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_35<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_36<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_37<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_38<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_39<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_40<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_41<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_42<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_43<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_44<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_45<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_46<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_47<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_48<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_49<-c(0,0,0,0,0,0,0,0,0,0)
        calc_ori_price_50<-c(0,0,0,0,0,0,0,0,0,0)
        
        
        
        
        if(input$updown == "102.00~98.00"){
            
            #calc_ori_price_17<-formatC(calc_ori_row_price*0.01*calc_ori_row_percent,format="d",big.mark = ",")
            calc_ori_price_17<-calc_ori_row_price*0.01*calc_ori_row_percent
            calc_ori_price_1=formatC(calc_ori_price_17*98.4/100,format="d",big.mark = ",") 
            calc_ori_price_2=formatC(calc_ori_price_17*98.5/100,format="d",big.mark = ",")
            calc_ori_price_3=formatC(calc_ori_price_17*98.6/100,format="d",big.mark = ",")
            calc_ori_price_4=formatC(calc_ori_price_17*98.7/100,format="d",big.mark = ",")
            calc_ori_price_5=formatC(calc_ori_price_17*98.8/100,format="d",big.mark = ",") 
            calc_ori_price_6=formatC(calc_ori_price_17*98.9/100,format="d",big.mark = ",")
            calc_ori_price_7=formatC(calc_ori_price_17*99/100,format="d",big.mark = ",")
            calc_ori_price_8=formatC(calc_ori_price_17*99.1/100,format="d",big.mark = ",")
            calc_ori_price_9=formatC(calc_ori_price_17*99.2/100,format="d",big.mark = ",")
            calc_ori_price_10=formatC(calc_ori_price_17*99.3/100,format="d",big.mark = ",")
            calc_ori_price_11=formatC(calc_ori_price_17*99.4/100,format="d",big.mark = ",")
            calc_ori_price_12=formatC(calc_ori_price_17*99.5/100,format="d",big.mark = ",")
            calc_ori_price_13=formatC(calc_ori_price_17*99.6/100,format="d",big.mark = ",")
            calc_ori_price_14=formatC(calc_ori_price_17*99.7/100,format="d",big.mark = ",")
            calc_ori_price_15=formatC(calc_ori_price_17*99.8/100,format="d",big.mark = ",")
            calc_ori_price_16=formatC(calc_ori_price_17*99.9/100,format="d",big.mark = ",")
            #calc_ori_price_17=tmp*calc_ori_row_percent
            calc_ori_price_18=formatC(calc_ori_price_17*100.1/100,format="d",big.mark = ",")
            calc_ori_price_19=formatC(calc_ori_price_17*100.2/100,format="d",big.mark = ",")
            calc_ori_price_20=formatC(calc_ori_price_17*100.3/100,format="d",big.mark = ",")
            calc_ori_price_21=formatC(calc_ori_price_17*100.4/100,format="d",big.mark = ",")
            calc_ori_price_22=formatC(calc_ori_price_17*100.5/100,format="d",big.mark = ",")
            calc_ori_price_23=formatC(calc_ori_price_17*100.6/100,format="d",big.mark = ",")
            calc_ori_price_24=formatC(calc_ori_price_17*100.7/100,format="d",big.mark = ",")
            calc_ori_price_25=formatC(calc_ori_price_17*100.8/100,format="d",big.mark = ",")
            calc_ori_price_26=formatC(calc_ori_price_17*100.9/100,format="d",big.mark = ",")
            calc_ori_price_27=formatC(calc_ori_price_17*101.0/100,format="d",big.mark = ",")
            calc_ori_price_28=formatC(calc_ori_price_17*101.1/100,format="d",big.mark = ",")
            calc_ori_price_29=formatC(calc_ori_price_17*101.2/100,format="d",big.mark = ",")
            calc_ori_price_30=formatC(calc_ori_price_17*101.3/100,format="d",big.mark = ",")
            calc_ori_price_31=formatC(calc_ori_price_17*101.4/100,format="d",big.mark = ",")
            calc_ori_price_32=formatC(calc_ori_price_17*101.5/100,format="d",big.mark = ",")
            calc_ori_price_33=formatC(calc_ori_price_17*101.6/100,format="d",big.mark = ",")
            calc_ori_price_34=formatC(calc_ori_price_17*101.7/100,format="d",big.mark = ",")
            calc_ori_price_17<-formatC(calc_ori_price_17,format="d",big.mark = ",")
        
                
        calc_ori_df <<- t(data.frame(calc_ori_p_name,calc_ori_g_num,calc_ori_h_g_num,calc_ori_p_num,calc_ori_row_percent,calc_range,
                                   calc_ori_price_34,
                                   calc_ori_price_33,
                                   calc_ori_price_32,
                                   calc_ori_price_31,
                                   calc_ori_price_30,
                                   calc_ori_price_29,
                                   calc_ori_price_28,
                                   calc_ori_price_27,
                                   calc_ori_price_26,
                                   calc_ori_price_25,
                                   calc_ori_price_24,
                                   calc_ori_price_23,
                                   calc_ori_price_22,
                                   calc_ori_price_21,
                                   calc_ori_price_20,
                                   calc_ori_price_19,
                                   calc_ori_price_18,
                                   calc_ori_price_17,
                                   calc_ori_price_16,
                                   calc_ori_price_15,
                                   calc_ori_price_14,
                                   calc_ori_price_13,
                                   calc_ori_price_12,
                                   calc_ori_price_11,
                                   calc_ori_price_10,
                                   calc_ori_price_9,
                                   calc_ori_price_8,
                                   calc_ori_price_7,
                                   calc_ori_price_6,
                                   calc_ori_price_5,
                                   calc_ori_price_4,
                                   calc_ori_price_3,
                                   calc_ori_price_2,
                                   calc_ori_price_1
                                   
                                       
                                       ))
        colnames(calc_ori_df)<<-c(rep("기업별",dim(calc_ori_df)[2]))
        
        row.names(calc_ori_df)<<- c("입찰사","기술점수","기술환산점수","가격점수","최저투찰율","적격심사점수만족범위(추정예가아래)",
                                   "101.7",
                                   "101.6",
                                   "101.5",
                                   "101.4",
                                   "101.3",
                                   "101.2",
                                   "101.1",
                                   "101.0",
                                   "100.9",
                                   "100.8",
                                   "100.7",
                                   "100.6",
                                   "100.5",
                                   "100.4",
                                   "100.3",
                                   "100.2",
                                   "100.1",
                                   "100.0",
                                   "99.9",
                                   "99.8",
                                   "99.7",
                                   "99.6",
                                   "99.5",
                                   "99.4",
                                   "99.3",
                                   "99.2",
                                   "99.1",
                                   "99.0",
                                   "98.9",
                                   "98.8",
                                   "98.7",
                                   "98.6",
                                   "98.5",
                                   "98.4"
                                   )
            
        }else{
            calc_ori_price_26<-calc_ori_row_price*0.01*calc_ori_row_percent
            calc_ori_price_1=formatC(calc_ori_price_26*97.5/100,format="d",big.mark = ",")
            calc_ori_price_2=formatC(calc_ori_price_26*97.6/100,format="d",big.mark = ",") 
            calc_ori_price_3=formatC(calc_ori_price_26*97.7/100,format="d",big.mark = ",")
            calc_ori_price_4=formatC(calc_ori_price_26*97.8/100,format="d",big.mark = ",")
            calc_ori_price_5=formatC(calc_ori_price_26*97.9/100,format="d",big.mark = ",") 
            calc_ori_price_6=formatC(calc_ori_price_26*98.0/100,format="d",big.mark = ",")
            calc_ori_price_7=formatC(calc_ori_price_26*98.1/100,format="d",big.mark = ",")
            calc_ori_price_8=formatC(calc_ori_price_26*98.2/100,format="d",big.mark = ",")
            calc_ori_price_9=formatC(calc_ori_price_26*98.3/100,format="d",big.mark = ",")
            calc_ori_price_10=formatC(calc_ori_price_26*98.4/100,format="d",big.mark = ",")
            calc_ori_price_11=formatC(calc_ori_price_26*98.5/100,format="d",big.mark = ",")
            calc_ori_price_12=formatC(calc_ori_price_26*98.6/100,format="d",big.mark = ",")
            calc_ori_price_13=formatC(calc_ori_price_26*98.7/100,format="d",big.mark = ",")
            calc_ori_price_14=formatC(calc_ori_price_26*98.8/100,format="d",big.mark = ",")
            calc_ori_price_15=formatC(calc_ori_price_26*98.9/100,format="d",big.mark = ",")
            calc_ori_price_16=formatC(calc_ori_price_26*99.0/100,format="d",big.mark = ",")
            calc_ori_price_17=formatC(calc_ori_price_26*99.1/100,format="d",big.mark = ",")
            calc_ori_price_18=formatC(calc_ori_price_26*99.2/100,format="d",big.mark = ",")
            calc_ori_price_19=formatC(calc_ori_price_26*99.3/100,format="d",big.mark = ",")
            calc_ori_price_20=formatC(calc_ori_price_26*99.4/100,format="d",big.mark = ",")
            calc_ori_price_21=formatC(calc_ori_price_26*99.5/100,format="d",big.mark = ",")
            calc_ori_price_22=formatC(calc_ori_price_26*99.6/100,format="d",big.mark = ",")
            calc_ori_price_23=formatC(calc_ori_price_26*99.7/100,format="d",big.mark = ",")
            calc_ori_price_24=formatC(calc_ori_price_26*99.8/100,format="d",big.mark = ",")
            calc_ori_price_25=formatC(calc_ori_price_26*99.9/100,format="d",big.mark = ",")
            #calc_ori_price_26=calc_ori_price_17*100.0/100
            calc_ori_price_27=formatC(calc_ori_price_26*100.1/100,format="d",big.mark = ",")
            calc_ori_price_28=formatC(calc_ori_price_26*100.2/100,format="d",big.mark = ",")
            calc_ori_price_29=formatC(calc_ori_price_26*100.3/100,format="d",big.mark = ",")
            calc_ori_price_30=formatC(calc_ori_price_26*100.4/100,format="d",big.mark = ",")
            calc_ori_price_31=formatC(calc_ori_price_26*100.5/100,format="d",big.mark = ",")
            calc_ori_price_32=formatC(calc_ori_price_26*100.6/100,format="d",big.mark = ",")
            calc_ori_price_33=formatC(calc_ori_price_26*100.7/100,format="d",big.mark = ",")
            calc_ori_price_34=formatC(calc_ori_price_26*100.8/100,format="d",big.mark = ",")
            calc_ori_price_35=formatC(calc_ori_price_26*100.9/100,format="d",big.mark = ",")
            calc_ori_price_36=formatC(calc_ori_price_26*101.0/100,format="d",big.mark = ",")
            calc_ori_price_37=formatC(calc_ori_price_26*101.1/100,format="d",big.mark = ",")
            calc_ori_price_38=formatC(calc_ori_price_26*101.2/100,format="d",big.mark = ",")
            calc_ori_price_39=formatC(calc_ori_price_26*101.3/100,format="d",big.mark = ",")
            calc_ori_price_40=formatC(calc_ori_price_26*101.4/100,format="d",big.mark = ",")
            calc_ori_price_41=formatC(calc_ori_price_26*101.5/100,format="d",big.mark = ",")
            calc_ori_price_42=formatC(calc_ori_price_26*101.6/100,format="d",big.mark = ",")
            calc_ori_price_43=formatC(calc_ori_price_26*101.7/100,format="d",big.mark = ",")
            calc_ori_price_44=formatC(calc_ori_price_26*101.8/100,format="d",big.mark = ",")
            calc_ori_price_45=formatC(calc_ori_price_26*101.9/100,format="d",big.mark = ",")
            calc_ori_price_46=formatC(calc_ori_price_26*102.0/100,format="d",big.mark = ",")
            calc_ori_price_47=formatC(calc_ori_price_26*102.1/100,format="d",big.mark = ",")
            calc_ori_price_48=formatC(calc_ori_price_26*102.2/100,format="d",big.mark = ",")
            calc_ori_price_49=formatC(calc_ori_price_26*102.3/100,format="d",big.mark = ",")
            calc_ori_price_50=formatC(calc_ori_price_26*102.4/100,format="d",big.mark = ",")
            calc_ori_price_26<-formatC(calc_ori_price_26,format="d",big.mark = ",")
            
            
            calc_ori_df <<- t(data.frame(calc_ori_p_name,calc_ori_g_num,calc_ori_h_g_num,calc_ori_p_num,calc_ori_row_percent,calc_range,
                                       calc_ori_price_50,
                                       calc_ori_price_49,
                                       calc_ori_price_48,
                                       calc_ori_price_47,
                                       calc_ori_price_46,
                                       calc_ori_price_45,
                                       calc_ori_price_44,
                                       calc_ori_price_43,
                                       calc_ori_price_42,
                                       calc_ori_price_41,
                                       calc_ori_price_40,
                                       calc_ori_price_39,
                                       calc_ori_price_38,
                                       calc_ori_price_37,
                                       calc_ori_price_36,
                                       calc_ori_price_35,
                                       calc_ori_price_34,
                                       calc_ori_price_33,
                                       calc_ori_price_32,
                                       calc_ori_price_31,
                                       calc_ori_price_30,
                                       calc_ori_price_29,
                                       calc_ori_price_28,
                                       calc_ori_price_27,
                                       calc_ori_price_26,
                                       calc_ori_price_25,
                                       calc_ori_price_24,
                                       calc_ori_price_23,
                                       calc_ori_price_22,
                                       calc_ori_price_21,
                                       calc_ori_price_20,
                                       calc_ori_price_19,
                                       calc_ori_price_18,
                                       calc_ori_price_17,
                                       calc_ori_price_16,
                                       calc_ori_price_15,
                                       calc_ori_price_14,
                                       calc_ori_price_13,
                                       calc_ori_price_12,
                                       calc_ori_price_11,
                                       calc_ori_price_10,
                                       calc_ori_price_9,
                                       calc_ori_price_8,
                                       calc_ori_price_7,
                                       calc_ori_price_6,
                                       calc_ori_price_5,
                                       calc_ori_price_4,
                                       calc_ori_price_3,
                                       calc_ori_price_2,
                                       calc_ori_price_1
                                       
            ))
            
            colnames(calc_ori_df)<<-c(rep("기업별",dim(calc_ori_df)[2]))
            
            
            row.names(calc_ori_df)<<- c("입찰사","기술점수","기술환산점수","가격점수","최저투찰율","적격심사점수만족범위(추정예가아래)",
                                       "102.4",
                                       "102.3",
                                       "102.2",
                                       "102.1",
                                       "102.0",
                                       "101.9",
                                       "101.8",
                                       "101.7",
                                       "101.6",
                                       "101.5",
                                       "101.4",
                                       "101.3",
                                       "101.2",
                                       "101.1",
                                       "101.0",
                                       "100.9",
                                       "100.8",
                                       "100.7",
                                       "100.6",
                                       "100.5",
                                       "100.4",
                                       "100.3",
                                       "100.2",
                                       "100.1",
                                       "100.0",
                                       "99.9",
                                       "99.8",
                                       "99.7",
                                       "99.6",
                                       "99.5",
                                       "99.4",
                                       "99.3",
                                       "99.2",
                                       "99.1",
                                       "99.0",
                                       "98.9",
                                       "98.8",
                                       "98.7",
                                       "98.6",
                                       "98.5",
                                       "98.4",
                                       "98.3",
                                       "98.2",
                                       "98.1",
                                       "98.0",
                                       "97.9",
                                       "97.8",
                                       "97.7",
                                       "97.6",
                                       "97.5"
            )
            
            }
            
            
        
        
        DT::datatable(calc_ori_df, options = list(pageLength = 60))
        
        
        
    })
    
    
    ### 기술점수  //   가격점수   // 적격심사점수
    output$g_num = renderText({
        i_p_name = as.character(input$input_project_name)
        
        
        
        query = paste0("select P_G_NUM from meta where P_NAME like '%",i_p_name,"%'")
        
        print(as.numeric(dbGetQuery(con,query)))
        
        
    })
    
    output$p_num = renderText({
        i_p_name = as.character(input$input_project_name)
                        
        
        
        query = paste0("select P_G_NUM from meta where P_NAME like '%",i_p_name,"%'")
            
        print(100-as.numeric(dbGetQuery(con,query)))
            
    })
    
    output$s_num = renderText({
        i_p_name = as.character(input$input_project_name)
        
        query = paste0("select P_P_NUM from meta where P_NAME like '%",i_p_name,"%'")
        
        print(as.numeric(dbGetQuery(con,query)))
    })
    
    ################## 기업별 추정예가 분포 비교 ###################
    
    output$yega = renderPlot({
        
        #### 메인기업 ####
        i_d<-input$input_company
        c_d<-input$competition_company
        t_d<-input$competition_company2
        #i_d<-'도화'
        #c_d<-'KECC'
        
        
        query_yega_c1 = paste0(
            "select NVL(LEGEND,0) as LEGEND,count(*) as COUNT from (
                select CASE WHEN
                P_YEGA>=110 THEN '110 이상'
                WHEN P_YEGA >=105 THEN '105~110'
                WHEN P_YEGA >=100 THEN '100~105'
                WHEN P_YEGA >=95 THEN '95~100'
                WHEN P_YEGA >=90 THEN '90~95'
                WHEN P_YEGA<90 THEN '90 미만' ELSE '기타' END LEGEND
                from info_detail
                where M_COMPANY='",i_d,"' )
                group by LEGEND")         
        
         query_yega_c2 = paste0(
             "select NVL(LEGEND,0) as LEGEND,count(*) as COUNT from (
                select CASE WHEN
                P_YEGA>=110 THEN '110 이상'
                WHEN P_YEGA >=105 THEN '105~110'
                WHEN P_YEGA >=100 THEN '100~105'
                WHEN P_YEGA >=95 THEN '95~100'
                WHEN P_YEGA >=90 THEN '90~95'
                WHEN P_YEGA<90 THEN '90 미만' ELSE '기타' END LEGEND
                from info_detail
                where M_COMPANY='",c_d,"' )
                group by LEGEND")
         
         query_yega_c3 = paste0(
             "select NVL(LEGEND,0) as LEGEND,count(*) as COUNT from (
                select CASE WHEN
                P_YEGA>=110 THEN '110 이상'
                WHEN P_YEGA >=105 THEN '105~110'
                WHEN P_YEGA >=100 THEN '100~105'
                WHEN P_YEGA >=95 THEN '95~100'
                WHEN P_YEGA >=90 THEN '90~95'
                WHEN P_YEGA<90 THEN '90 미만' ELSE '기타' END LEGEND
                from info_detail
                where M_COMPANY='",t_d,"' )
                group by LEGEND")
        
        
        result_yega_c1 = dbGetQuery(con,query_yega_c1)
        result_yega_c2 = dbGetQuery(con,query_yega_c2)
        result_yega_c3 = dbGetQuery(con,query_yega_c3)
        
        as.numeric(dim(result_yega_c1)[1]) -> length1
        as.numeric(dim(result_yega_c2)[1]) -> length2
        as.numeric(dim(result_yega_c3)[1]) -> length3
        
        result_yega<-rbind(result_yega_c1,result_yega_c2,result_yega_c3)
        
        ##################
        #rep(i_d,length1)
        
        if(c_d=="선택전" && t_d=="선택전"){
            group<-c(rep(i_d,length1))
            result_frame_yega = data.frame(group,result_yega)
            result_frame_yega$LEGEND <- factor(result_frame_yega$LEGEND, levels = c("90 미만","90~95","95~100","100~105","105~110","110 이상"))
        
            ggplot(data=result_frame_yega,aes(x=LEGEND,y=COUNT,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=LEGEND,y=COUNT,label=COUNT),position = position_dodge(width = 1),
                                                                                                                                          vjust = -0.5, size = 5)+xlab("범주")+ylab("추정예가")+ggtitle(paste0(i_d," [[ 추정예가 분포 ]]"))
            
            
        }else if(c_d!="선택전" && t_d=="선택전"){
            group<-c(rep(i_d,length1),rep(c_d,length2))
            result_frame_yega = data.frame(group,result_yega)
            result_frame_yega$LEGEND <- factor(result_frame_yega$LEGEND, levels = c("90 미만","90~95","95~100","100~105","105~110","110 이상"))
            
            ggplot(data=result_frame_yega,aes(x=LEGEND,y=COUNT,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=LEGEND,y=COUNT,label=COUNT),position = position_dodge(width = 1),
                                                                                                                                          vjust = -0.5, size = 5)+xlab("범주")+ylab("추정예가")+ggtitle(paste0(i_d,"vs",c_d," [[ 추정예가 분포 ]]"))
        }else if(c_d=="선택전" && t_d!="선택전"){
            group<-c(rep(i_d,length1),rep(t_d,length3))
            result_frame_yega = data.frame(group,result_yega)
            result_frame_yega$LEGEND <- factor(result_frame_yega$LEGEND, levels = c("90 미만","90~95","95~100","100~105","105~110","110 이상"))
            
            ggplot(data=result_frame_yega,aes(x=LEGEND,y=COUNT,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=LEGEND,y=COUNT,label=COUNT),position = position_dodge(width = 1),
                                                                                                                                          vjust = -0.5, size = 5)+xlab("범주")+ylab("추정예가")+ggtitle(paste0(i_d,"vs",t_d," [[ 추정예가 분포 ]]"))
        }else{     
            group<-c(rep(i_d,length1),rep(c_d,length2),rep(t_d,length3))
            result_frame_yega = data.frame(group,result_yega)
            result_frame_yega$LEGEND <- factor(result_frame_yega$LEGEND, levels = c("90 미만","90~95","95~100","100~105","105~110","110 이상"))
        
            ggplot(data=result_frame_yega,aes(x=LEGEND,y=COUNT,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=LEGEND,y=COUNT,label=COUNT),position = position_dodge(width = 1),
                                                                                                                                          vjust = -0.5, size = 5)+xlab("범주")+ylab("추정예가")+ggtitle(paste0(i_d,"vs",c_d,"vs",t_d," [[ 추정예가 분포 ]]"))
        }
        
        
        #result_frame_yega = data.frame(group,result_yega)
        #result_frame_yega$LEGEND <- factor(result_frame_yega$LEGEND, levels = c("90 미만","90~95","95~100","100~105","105~110","110 이상"))
        #ggplot(data=result_frame_yega,aes(x=LEGEND,y=COUNT,fill=group))+geom_bar(stat="identity",position=position_dodge())+geom_text(aes(x=LEGEND,y=COUNT,label=COUNT),position = position_dodge(width = 1),
        #                                                                                                                              vjust = -0.5, size = 5)+xlab("범주")+ylab("추정예가")+ggtitle(paste0(i_d," vs ",c_d," [[ 추정예가 분포 ]]"))
        
    })
    
    
    
    ### make new Menu
    
    output$menu <- renderMenu({
        sidebarMenu(
            
            menuItem("Data INPUT", tabName = "View1", icon = icon("th")),
            #menuItem("분석 시스템 (과거 데이터)", tabName = "View2", icon = icon("th")),
            menuItem("분석 시스템 (과거 데이터)",icon = icon("dashboard"),
                     menuSubItem("추정예가 / 기술순위 분포",icon=icon("dashboard"), tabName = "View2"),
                     menuSubItem("분석 화면 2",icon=icon("dashboard")),
                     menuSubItem("분석 화면 3",icon=icon("dashboard")),
                     menuSubItem("분석 화면 4",icon=icon("dashboard"))),
            menuItem("참여기업별 예정가 계산", tabName = "View3", icon = icon("th")),
            menuItem("UP & DOWN 예측 시뮬레이션", tabName = "View4", icon = icon("dashboard"))
        )
    })
    
    
    ### DOWNLOAD ###
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$input_project_name, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(calc_ori_df, file, row.names = TRUE)
        }
    )
    
}

shinyApp(ui, server)
