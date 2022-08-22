#    http://shiny.rstudio.com/
# install.packages('devtools')
# devtools::install_github("timelyportfolio/d3treeR")
# install.packages('treemap')

# 라이브러리 로딩
library(RColorBrewer)
library(ggplot2)
library(shiny)
library(treemap)
library(d3treeR)
library(tidyverse)
# library(leaflet)


# 데이터 로딩
df <- read.csv('hotel_bookings.csv')
df <- df[complete.cases(df),]

# 사용할 함수 만들기 ->  input: vector(양수 값), output: 색깔
color_ft <- function(vec) {
  d <- c()
  for (i in vec) {
    d <- c(d,brewer.pal(9, 'OrRd')[3+ round(6*i / max(vec))])
  }
  return (d)
}

# 문자형 -> 범주형 변경
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

# country group

df%>%
  group_by(country)%>%
  summarise(num=n())%>%
  arrange(desc(num))

df%>%
  filter(adr>1000)

df = df%>%
  mutate(adr = replace(adr, adr>1000, mean(adr)))

df <- df[df$adr> 0, ]

df <- df[df$country != 'NULL',]
df[rownames(subset(df, country== 'CN')),'country'] <- 'CHN'



# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel('1st', 
             sidebarLayout(
               sidebarPanel(),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel('treemaps', h2('재방문고객의 국적별로 차지하는 비율 -> treemaps 사용'),
                            d3tree2Output("wng_tree"),
                            ),
                   
                   tabPanel('bargraph', h2('첫방문, 재방문 별로 분류해서 범주형 데이터 분석 -> geom_bar 사용'),
                            selectInput("wng_count", 
                                        label = "첫방문,재방문 비교(범주형)",
                                        choices = c("식사" ="meal", 
                                                    "손님 유형" = "customer_type",
                                                    "예약 상태"= "reservation_status", 
                                                    "방"="reserved_room_type"),
                                        selected = "meal")
                            , 
                            # 첫방문, 재방문 별로 비교
                            plotOutput("wng_bar")
                            ),
                   tabPanel('violin', h2('첫방문, 재방문 별로 분류해서 수치형 데이터 분석'), 
                            selectInput("wng_fac", 
                                        label = "첫방문,재방문 비교(수치형)",
                                        choices = c("평균숙박비"="adr", 
                                                    "도착일-예약일"= "lead_time"),
                                        selected = "adr"),
                            plotOutput("wng_violin")
                            ),
                   
                  
                   tabPanel('rank', h2('준비중'),
                            plotOutput("wng_line")
                            ),
                   tabPanel('map', h2('첫방문 대비 재방문이 차지하는 비율'),
                            plotOutput("wng_map")
                            
                            
                            )
                 )
                 ,
                 
                 
                 
               )
             )
             ),
    tabPanel('2nd')
  ),

    # Application title
    titlePanel("재방문 관련 변수 분석"),

    # Sidebar with a slider input for number of bins 
    
)
?mutate
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wng_tree <- renderD3tree2({
      df_agg <- read.csv('hotel_rep_agg.csv')
      # basic treemap
      p <- treemap(df_agg,
                   index=c("is_prt","continent", "country" ),
                   vSize="value",
                   type="index",
                   palette = "Set2",
                   align.labels=list(
                     c("center", "center"), 
                     c("right", "bottom")
                   )  
      )              
      # bg.labels=c("white")
      # make it interactive ("rootname" becomes the title of the plot):
      inter <- d3tree2( p ,  rootname = "General" )
      inter
    })
    
    output$wng_bar <- renderPlot({
      
      # formulaText <- reactive({
      #   paste('x=', input$wng_count, 'fill=', input$wng_count )
      # })
      p <- ggplot(data=df, mapping = aes(x=get(input$wng_count), fill=get(input$wng_count)))
      q <- p + geom_bar() +facet_wrap(~is_repeated_guest, nrow=2, scales = 'free', labeller ='label_both')
      q
    })
    
    output$wng_violin <- renderPlot({
      p <- ggplot(data=df, mapping=aes(x=is_repeated_guest, y=get(input$wng_fac), fill=is_repeated_guest)) + geom_violin() +coord_flip()+ facet_wrap(~arrival_date_year, nrow=2, labeller ='label_both')
      p
  
    })

    output$wng_map <- renderPlot({
      
      df_merge <- read.csv('hotel_merge')
      df_color <- df_merge$mv
      color_map <- ggplot(df_merge, aes(long, lat, group=group)) + 
        geom_polygon(fill=color_ft(df_color), color='gray')
      color_map
    })
    
    # output$map_table <- renderTable({
    #   req(input$map_event)
    #   df_merge <- read.csv('hotel_merge')
    #   
    #   nearPoints(df_merge, input$plot_click, panelvar1 ='long', panelvar2 ='lat', allRows = F)
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
