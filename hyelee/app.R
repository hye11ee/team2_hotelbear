# ----------------------------------- library
library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(thematic)
library(tidyverse)
library(bslib)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggcorrplot)
library(colorspace)
library(shinyjs)
library(plotly)
library(gganimate)
library(av)
library(nord)
library(hexbin)
library(gifski)



# ----------------------------------- data preprocessing
df <- read.csv('hotel_bookings.csv')
table(is.na(df))
df <- df[complete.cases(df),]

df <- df[df$country!='NULL',]
df <- df[df$adr>0,]

df<-df%>%
  mutate(
    hotel=as.factor(hotel),      
    is_canceled=as.factor(is_canceled),
    meal=as.factor(meal),
    country=as.factor(country),
    market_segment=as.factor(market_segment),
    distribution_channel=as.factor(distribution_channel),
    is_repeated_guest=as.factor(is_repeated_guest),
    reserved_room_type=as.factor(reserved_room_type),
    assigned_room_type=as.factor(assigned_room_type),
    deposit_type=as.factor(deposit_type),
    customer_type=as.factor(customer_type),
    reservation_status=as.factor(reservation_status),
    agent=as.factor(agent),
    company=as.factor(company),
    arrival_date_day_of_month=as.factor(arrival_date_day_of_month),
    arrival_date_month=as.factor(arrival_date_month),
    arrival_date_year=as.factor(arrival_date_year)
    
  )


df%>%
  group_by(country)%>%
  summarise(num=n())%>%
  arrange(desc(num))

df%>%
  filter(adr>1000)

df = df%>%
  mutate(adr = replace(adr, adr>1000, mean(adr)))

linebreaks <- function(n){HTML(strrep(br(), n))}


# ----------------------------------- home image css
backgroundImageCSS <- "/* background-color: #cccccc; */
                       height: 91vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url('%s');"


# ----------------------------------- UI

ui <- dashboardPage(
  dashboardHeader(title = "Hotel Booking demand"),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "sumarry", "Home", icon = icon("home")),
      menuItem(tabName = "intro", "DataFame", icon = icon("table")),
      menuItem(tabName = "main", "Chart", icon = icon("bar-chart-o"),
                menuSubItem(tabName = "R0", "Corrplot", icon = icon('bolt')),
                menuSubItem(tabName = "R1", "Barchart", icon = icon('bolt')),
                menuSubItem(tabName = "R2", "Histgram", icon = icon('bolt')),
                menuSubItem(tabName = "R3", "Animate", icon = icon("bolt"))
               ),
               useShinyjs(),
      menuItem(tabName = "conclusion", "Information", icon = icon("user circle"))
    )
  ),
  dashboardBody(
      # 소개 ------------------
      tabItem(
        tabName = 'conclusion',
              fluidPage(
                h1('Visualize Team Project'),
                column(width = 8,
                       h2('Team member'),
                       fluidRow(
                         column(width = 8,offset = 1,
                                h3('곽영효'),
                                fluidRow(
                                  column(width = 8,offset = 1,
                                         div('호텔 예약 취소율 감소 및 이용객 증가 방안 도출')
                                  )
                                ),
                                
                                h3('강혜리'),
                                fluidRow(
                                  column(width = 8,offset = 1,
                                         div('호텔 예약 취소 관련 변수 분석')
                                  )
                                ),
                                
                                h3('신나령'),
                                fluidRow(
                                  column(width = 8,offset = 1,
                                         div('호텔 예약자/취소자 현황 분석'),
                                  )
                                ),
                                h3('우남광'),
                                fluidRow(
                                  column(width = 8,offset = 1,
                                         div('재방문 관련 변수 분석'),
                                  )
                                )
                         )
                       )    
                ),
                column(width = 4,
                       fluidRow(
                         br(),br(),br(),br()
                       ),
                       fluidRow(
                         h4('GitHub'),
                         a(href = "https://github.com/hye11ee/team2_hotelbear", 'GitHub Link'),
                         h4('Data'),
                         a(href = "https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand",
                           '[kaggle] hotel booking demand'),
                         h4('Shiny URL'),
                         a(href = "https://legendaryhero.shinyapps.io/younghyo/", '곽영효'),
                         a(href = "https://isna.shinyapps.io/ProjectWeek9/", '신나령'),
                         a(href = "https://kkonji.shinyapps.io/9th_project_namgwang/", '우남광')
                       )
                )
              )
      ),
      
    
    tabItem(
      selected = 1,
      tabName = "sumarry",
        center = TRUE,
        style = sprintf(backgroundImageCSS, 
                        "https://seoul.intercontinental.com/upload/image/room/21/10949.jpg")
      ),
    
    tabItem(
        tabName = "R1",
        fluidRow(
          box(width = 8,
              title = "Graph 1",
              color = "blue", ribbon = TRUE, title_side = "top right",
              column(width = 8,
                     plotOutput("plot1", height = 450)
              )
          ),
          
          box(width = 8,
              title = "Graph 2",
              color = "red", ribbon = TRUE, title_side = "top right",
              column(width = 8,
                     plotOutput("plot2", height = 450)
              )
          )
        )
      ),
    
    
      tabItem(
        tabName = "intro",
        fluidRow(
          dataTableOutput("hoteltable")
        )
      ),
    
    
    tabItem(
        
        tabName = "R0",
        fluidRow(
            box(width = 16,
                title = "Graph 3",
                color = "black", ribbon = TRUE, title_side = "top right",
                column(width = 16,
                       plotOutput("plot3", height = 450,width = 1000)
                )
            )
        )

    ),
    
    
    tabItem(
        tabName = "R2",
        fluidRow(
            box(width = 16,
                title = "Graph 4",
                color = "green", ribbon = TRUE, title_side = "top right",
                column(width = 16,
                       plotlyOutput("plot4", height = 450,width = 800)))
            )
    ),
  tabItem(
    tabName = "R3",
    fluidRow(
      box(width = 16,
          title = "Graph 5",
          color = "green", ribbon = TRUE, title_side = "top right",
          column(width = 16,
                 plotOutput("plot5", height = 450,width = 900)))
    )
  ),
  
  
  tabItem(
    selected = 1,
    tabName = "conclusion1",
    center = TRUE,
    style = sprintf(backgroundImageCSS, 
                    "https://www.fourseasons.com/alt/img-opt/~80.930.0,0000-188,7500-3000,0000-1687,5000/publish/content/dam/fourseasons/images/web/SKO/SKO_725_original.jpg")
  ), theme = "simplex")
)



# ----------------------------------- SERVER

server <- shinyServer(function(input, output, session) {
  colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])

  output$hoteltable <- renderDataTable(df)
  
  # bar chart (box1 ~ box2)
  output$plot1 <- renderPlot({
      df%>% 
          ggplot(aes(x=distribution_channel,fill=is_canceled)) + 
          geom_bar(alpha=.6, width=.4) +
          scale_fill_manual(values = c('skyblue 3','orange'))+
          scale_colour_manual(values = colscale) +
          theme_ridges()
  })
  
  output$plot2 <- renderPlot({
      df%>%
          ggplot(aes(x=assigned_room_type,fill=is_canceled))+
          geom_bar(alpha=.6, width=.4)+
          scale_fill_manual(values = c('skyblue 3','orange'))+
          scale_colour_manual(values = colscale) +
          theme_ridges()
  })
  
  # corr chart
  output$plot3 <- renderPlot({
      df$canceled <- as.numeric(df$is_canceled)
      corr <- cor(df[, sapply(df, is.numeric)])
      
      ggcorrplot(corr,
                 hc.order=TRUE,
                 type='lower',
                 lab=TRUE,
                 outline.color='white',
                 colors=diverge_hcl(3, palette='Tropic'))
  })  
  
  # histogram
  output$plot4 <- renderPlotly({
    #ggplot(data=df)+
      #geom_hex(mapping=aes(x=adr, 
                           #y=lead_time))+
      #theme_ridges()
    
    ggplot(data=df, x = adr, y = lead_time) +
      geom_hex(mapping=aes(x = adr,
                           y = lead_time))+ 
      scale_fill_viridis()

  })
  
  #animate chart (*** error)
  output$plot5 <- renderPlot({
    df_us <-  df[df$country == 'USA',]
    df_uk <- df[df$country == 'GBR',]
    df_po <- df[df$data1.country == 'PRT',]
    df_ger <- df[df$country == 'DEU',]
    df_sp <- df[df$country == 'ESP',]
    
    df_merge <- rbind(df_us, df_uk, df_po, df_ger, df_sp)
    
    ggplot(df_merge, aes(stays_in_weekend_nights, stays_in_week_nights, fill=country))+
      geom_violin(scale='area',
                  color=NA,
                  alpha=0.5) +
      scale_fill_nord('aurora')+
      ylim(0, 15) +
      #transition_states(is_canceled,
                        #transition_length=0.5,
                        #state_length=0.5)+
      theme_minimal()
    #anim_save("outfile.gif")
    
    # image save but not view...why......why...why.......
    #animate(an, renderer=av_renderer())
    #anim_save("outfile.gif")
    
    #list(src = "outfile.gif", contentType = "image/gif")
  }#,
  #deleteFile = TRUE
  )
  
})


shinyApp(ui, server)
