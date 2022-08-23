library(shiny)
library(ggplot2)
library(thematic)
library(tidyverse)
library(bslib)
library(gganimate)
library(gifski)


df <- read.csv('./hotel_bookings.csv')
table(is.na(df))
df <- df[complete.cases(df),]
df <- df[df$country!='NULL',]
df <- df[df$adr>0,]

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


thematic_shiny(font = "auto")

backgroundImageCSS <- "/* background-color: #cccccc; */
                       height: 91vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url('%s');"

linebreaks <- function(n){HTML(strrep(br(), n))}


ui <- fluidPage(h1("Hotel Booking demand", style = 'font-weight: bold;'),
                theme = bslib::bs_theme(
                  bg = "#FFFFF8", fg = "#111111", primary = "#111111",
                  base_font = bslib::font_google("Merriweather")
                ),
                
                tabsetPanel(
                  type = "pills",
                  tabPanel("summary",
                           linebreaks(7),
                           center = TRUE,
                           style = sprintf(backgroundImageCSS, 
                                           "https://www.lottehotel.com/content/dam/lotte-hotel/signiel/seoul/accommodation/suite/3291-2-2000-roo-LTSG.jpg.thumb.1920.1920.jpg"),
                           h4("KNU K-Digital Training ", style = 'font-weight: bold;', align = "center"),
                           h1("호텔 예약 취소율 감소 및 이용객 증가 방안 도출", style = 'font-weight: bold;', align = "center"),
                           linebreaks(3),
                           h4(tags$a(href = "https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand", "dataset : kaggle 'hotel booking demand'", target = "_blank"), align = "center"),
                           h4("member : 곽영효, 강혜리, 신나령, 우남광", align = "center")),
                  
                  
                  # 나령파트
                  tabPanel("introduction"),
                  
                  
                  
                  
                  
                  
                  
                  
                  # 혜리파트
                  tabPanel("main.1",
                           h2("호텔 예약 취소 요인 분석", style = 'font-weight: bold;'),
                           linebreaks(1),
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("wells"),
                               uiOutput("analytes"),
                               uiOutput("date_ranges"),
                               checkboxInput("scale_plot", "Scale Free Plot")
                             ),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Plot", plotOutput("plot1")),
                                 tabPanel("Summary", verbatimTextOutput("summary")),
                                 tabPanel("Table", tableOutput("table"))
                               )),
                           )
                           
                  ),
                  
                  # 남광파트
                  tabPanel("main.2"),
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #영효파트
                  tabPanel("conclusion",
                           h2('결론')
                           ),
                  mainPanel(
                    img(src = 'graph_yh_1.gif'),
                    plotOutput('plot_yh_2'),
                    plotOutput('plot_yh_3')
                  )
                )
)

server <- function(input, output) {
  # 나령파트
  
  
  
  
  
  
  
  # 혜리파트
  output$plot1 <- renderPlot({
    d <- df%>% ggplot(aes(x=arrival_date_year,fill=is_canceled))
    print(d + geom_bar())
  })
  
  
  # 남광파트
  
  
  
  
  
  
  
  # 영효파트


  output$plot_yh_1 <- renderPlot({
    
    df_city <- df[df$hotel == 'City Hotel',]
    df_resort <- df[df$hotel == 'Resort Hotel',]
    
    df_city_repeated <- df_city[df_city$is_repeated_guest == 1,]
    df_resort_repeated <- df_resort[df_resort$is_repeated_guest == 1,]
    
    market_segment <- c('Corporate', 'Aviation',
                        'Complementary', 'Direct',
                        'Groups', 'Offline TA/TO',
                        'Online TA')
    
    repeated_city <- c(
      round(nrow(df_city_repeated[df_city_repeated$market_segment== 'Corporate',]) / 
              nrow(df_city[df_city$market_segment == 'Corporate',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Aviation',]) / 
              nrow(df_city[df_city$market_segment == 'Aviation',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Complementary',]) / 
              nrow(df_city[df_city$market_segment == 'Complementary',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Direct',]) / 
              nrow(df_city[df_city$market_segment == 'Direct',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Groups',]) / 
              nrow(df_city[df_city$market_segment == 'Groups',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Offline TA/TO',]) / 
              nrow(df_city[df_city$market_segment == 'Offline TA/TO',])*100,2),
      round(nrow(df_city_repeated[df_city_repeated$market_segment=='Online TA',]) / 
              nrow(df_city[df_city$market_segment == 'Online TA',])*100,2))
    
    repeated_resort <- c(
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment== 'Corporate',]) / 
              nrow(df_resort[df_resort$market_segment == 'Corporate',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Aviation',])/ 
              nrow(df_resort[df_resort$market_segment == 'Aviation',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Complementary',]) / 
              nrow(df_resort[df_resort$market_segment == 'Complementary',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Direct',]) / 
              nrow(df_resort[df_resort$market_segment == 'Direct',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Groups',]) / 
              nrow(df_resort[df_resort$market_segment == 'Groups',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Offline TA/TO',]) / 
              nrow(df_resort[df_resort$market_segment == 'Offline TA/TO',])*100,2),
      round(nrow(df_resort_repeated[df_resort_repeated$market_segment=='Online TA',]) / 
              nrow(df_resort[df_resort$market_segment == 'Online TA',])*100,2))
    
    a <- data.frame(group = market_segment,
                    values = repeated_city, frame = rep('a', 7))
    b <- data.frame(group = market_segment,
                    values = repeated_resort, frame = rep('b', 7))
    data <- rbind(a,b) 
    
    ggplot(data, aes(x=group, y=values, fill=group)) + 
      geom_bar(stat='identity') +
      scale_fill_brewer(palette = "Set3", direction = -1) +
      theme_minimal() +
      transition_states(
        frame,
        transition_length = 2,
        state_length = 1
      ) + ease_aes('sine-in-out')
  })
  # anim_save("graph_yh_1.gif") 
  
  output$plot_yh_2 <- renderPlot({
    df_city_cancel <- df_city[df_city$is_canceled == 1,]
    df_resort_cancel <- df_resort[df_resort$is_canceled == 1,]
    
    unique(df$arrival_date_month)
    
    month <- c(
      'January', 'February', 'March', 'April', 'May',
      'June', 'July', 'August', 'September', 'October',
      'November', 'December'
    )
    
    cancel_city <- data.frame()
    cancel_resort <- data.frame()
    
    for (i in month){
      C1 = i
      C2 = round(nrow(df_city_cancel[df_city_cancel$arrival_date_month== i,]) / 
                   nrow(df_city[df_city$arrival_date_month == i,])*100,2)
      cancel_city = rbind(cancel_city, c(C1, C2))
    }
    names(cancel_city) = c('Month', 'Rate')
    
    for (i in month){
      C1 = i
      C2 = round(nrow(df_resort_cancel[df_resort_cancel$arrival_date_month== i,]) / 
                   nrow(df_resort[df_resort$arrival_date_month == i,])*100,2)
      cancel_resort = rbind(cancel_resort, c(C1, C2))
    }
    names(cancel_resort) = c('Month', 'Rate')
    
    ggplot(cancel_city, aes(Month, Rate)) +
      geom_bar(stat = 'identity') +
      scale_x_discrete(limits = month) +
      theme_minimal() +
      coord_polar() +
      xlab("") + ylab("")+
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_yh_3 <- renderPlot({
    ggplot(cancel_resort, aes(Month, Rate)) +
      geom_bar(stat = 'identity') +
      scale_x_discrete(limits = month) +
      theme_minimal() +
      coord_polar() +
      xlab("") + ylab("")+
      scale_fill_brewer(palette = "Set3") 
  })
}
shinyApp(ui, server)