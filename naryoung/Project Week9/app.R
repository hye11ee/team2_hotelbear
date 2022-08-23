library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)
library(semantic.dashboard)
library(gridExtra)
library(reshape2)
library(gganimate)
library(fmsb)
library(hrbrthemes)

hotelDf <- read.csv('./hotel_bookings.csv')

sum(is.na(hotelDf))
hotelDf <- na.omit(hotelDf)
sum(is.na(hotelDf))

hotelDf <- hotelDf[hotelDf$country!='NULL',]
hotelDf <- hotelDf[hotelDf$adr>0,]

# 나라별 예약/취소 현황
country.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$country), FUN=sum)
country.reserved <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$country), FUN=sum)

countryDf <- inner_join(country.reserved, country.canceled, by="Group.1")
colnames(countryDf) <- c('country', 'reserved', 'canceled')

# 연도별 예약/취소 현황
d1 <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$arrival_date_year, hotelDf$arrival_date_month), FUN=sum)
d2 <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_year, hotelDf$arrival_date_month), FUN=sum)
d1$Group.1 <- factor(d1$Group.1)
d2$Group.1 <- factor(d2$Group.1)
colnames(d1) <- c('year','month','count')
colnames(d2) <- c('year','month','count')

# 호텔별 예약/취소 현황
month.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_month,hotelDf$hotel), FUN=sum)
month.reserved <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$arrival_date_month,hotelDf$hotel), FUN=sum)

city1 <- month.reserved[month.reserved$Group.2=='City Hotel',]
city1$Group.1 <- c(4,8,12,2,1,6,7,3,5,11,10,9)
resort1 <- month.reserved[month.reserved$Group.2=='Resort Hotel',]
resort1$Group.1 <- c(4,8,12,2,1,6,7,3,5,11,10,9)
resort1 <- resort1[order(resort1$Group.1),]
city1 <- city1[order(city1$Group.1),]
data1 <- as.data.frame(rbind(city1$x,resort1$x))
colnames(data1) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July',
                     'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
row.names(data1) <- paste("mister" , letters[1:2] , sep="-")
data1 <- rbind(rep(10000,10) , rep(0,10) , data1)

city2 <- month.canceled[month.canceled$Group.2=='City Hotel',]
city2$Group.1 <- c(4,8,12,2,1,6,7,3,5,11,10,9)
resort2 <- month.canceled[month.canceled$Group.2=='Resort Hotel',]
resort2$Group.1 <- c(4,8,12,2,1,6,7,3,5,11,10,9)
resort2 <- resort2[order(resort2$Group.1),]
city2 <- city2[order(city2$Group.1),]
data2 <- as.data.frame(rbind(city2$x,resort2$x))
colnames(data2) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July',
                     'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
row.names(data2) <- paste("mister" , letters[1:2] , sep="-")
data2 <- rbind(rep(5000,10) , rep(0,10) , data2)

colors_border=c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

# 사람 수별 예약/취소 현황
hotelDf$family <- hotelDf$adults+hotelDf$children+hotelDf$babies
familyDf <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$family), FUN=sum)
familyDf$Group.1 <- factor(familyDf$Group.1)
colnames(familyDf) <- c('family','count')

familyDf2 <- aggregate(hotelDf$is_canceled, by=list(hotelDf$family), FUN=sum)
familyDf2$Group.1 <- factor(familyDf2$Group.1)
colnames(familyDf2) <- c('family','count')


ui <- dashboardPage(
  dashboardHeader(title = "호텔 예약/취소 현황", titleWidth = 50),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "plot1", "호텔별"),
      menuItem(tabName = "plot2", "나라별"),
      menuItem(tabName = "plot3", "연도별"),
      menuItem(tabName = "plot4", "사람수별")
    )
  ),
  dashboardBody(
    tabItem(tabName = "plot1",
            fluidRow(plotOutput('plot1', width = 1500, height = 600))
    ), 
    tabItem(tabName = "plot2",
            fluidRow(plotlyOutput('plot2', width = 900, height = 500))
    ),
    tabItem(tabName = "plot3",
            fluidRow(plotOutput('plot3', width = 1200, height = 500))
    ),
    tabItem(tabName = "plot4",
            fluidRow(plotOutput('plot4', width = 1200, height = 500))
    )
  ))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    par(mfrow=c(1,2))
    radarchart(data1, axistype=1 , 
               pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1, 
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10000,2500), cglwd=0.8,
               vlcex=0.8, title = 'Reservation Status by Month')
    legend(x=0.9, y=1.2, legend = c('City Hotel','Resort Hotel'), bty = "n", pch=20 ,
           col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
    radarchart(data2, axistype=1 , 
               pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1, 
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5000,1250), cglwd=0.8,
               vlcex=0.8, title = 'Cancellation Status by Month')
    legend(x=0.9, y=1.2, legend = c('City Hotel','Resort Hotel'), bty = "n", pch=20 ,
           col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
    par(mfrow=c(1,1))
  })
  output$plot2 <- renderPlotly({
    countryDf
    p <- ggplot(countryDf, aes(reserved, canceled)) +
      ggtitle("Reservation/Cancellation Status by Country") +
      geom_point(aes(color = country, size = reserved), alpha = 0.5) +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      scale_size(range=c(1,15)) +
      theme_bw() +
      ylim(c(0,30000)) +
      xlim(c(0,50000)) +
      theme(legend.position = 'none', plot.title = element_text(face='bold'),
            text = element_text(family = "Consolas", face='bold'))
    
    ggplotly(p)
  })
  output$plot3 <- renderPlot({
    p1 <- ggplot(d1, aes(x=year, y=count, fill=year)) +
      ggtitle('Reservation Status by year') +
      geom_violin(position='dodge', alpha=0.5, scale = 'count', color=NA, trim = F) + 
      theme_ipsum()
    p2 <- ggplot(d2, aes(x=year, y=count, fill=year)) + 
      ggtitle('Cancellation Status by year')+
      geom_violin(position='dodge', alpha=0.5, scale = 'count', color=NA, trim = F) + theme_ipsum()
    grid.arrange(arrangeGrob(p1, p2, ncol=2), nrow = 1) 
  })
  output$plot4 <- renderPlot({
    p1 <- ggplot(familyDf, aes(x=family, y=count)) +
      ggtitle('Reservation Status by family') +
      geom_segment(aes(x=family, xend=family, y=0, yend=count), color=ifelse(familyDf$family=='2','orange','grey'),
                   size = 1) +
      geom_point(color = ifelse(familyDf$family=='2','orange','grey'), 
                 size=ifelse(familyDf$family=='2',4,2), alpha=.6) + 
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )# + transition_time(family)
    p2 <- ggplot(familyDf2, aes(x=family, y=count)) +
      ggtitle('Cancellation Status by family')+
      geom_segment(aes(x=family, xend=family, y=0, yend=count), color=ifelse(familyDf$family=='2','orange','grey'),
                   size = 1) +
      geom_point(color = ifelse(familyDf$family=='2','orange','grey'), 
                 size=ifelse(familyDf$family=='2',4,2), alpha=.6) + 
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
    grid.arrange(arrangeGrob(p1, p2, ncol=2), nrow = 1)
  })
}

shinyApp(ui = ui, server = server)

