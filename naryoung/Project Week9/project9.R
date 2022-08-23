
library(ggplot2)
library(plotly)
library(dplyr)
library(extrafont)
library(plotly)
library(gganimate)

getwd()
hotelDf <- read.csv('./hotel_bookings.csv')

sum(is.na(hotelDf))
hotelDf <- na.omit(hotelDf)
sum(is.na(hotelDf))

hotelDf <- hotelDf[hotelDf$country!='NULL',]
hotelDf <- hotelDf[hotelDf$adr>0,]

# 호텔별 예약/취소 현황
"hotel.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$hotel), FUN=sum)
hotel.reserved <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$hotel), FUN=sum)
hotelnameDf <- rbind(hotel.reserved,hotel.canceled)
colnames(hotelnameDf) <- c('hotel', 'count')
hotelnameDf$status <- c('reserved','reserved','canceled','canceled')

p <- ggplot(hotelnameDf, aes(x=hotel, y=count, fill=status)) + 
  ggtitle('Reservation/Cancellation Status by Hotel') +
  geom_bar(position='stack', stat = 'identity') + 
  scale_fill_manual(values = c('#7BDDFC','#FFBE70')) +
  geom_text(aes(label = sprintf('%d',count)),
            position = position_stack(0.9),
            hjust = 0.5, vjust = -0.5) +
  scale_y_discrete() +
  coord_cartesian(ylim = c(0,82000)) +
  theme(panel.background = element_blank(),
        text = element_text(face='bold'), 
        plot.title = element_text(face='bold'),
        axis.title.x = element_text(size=15, face='bold'),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=15, face='bold'),
        legend.position = 'top') 
p<-ggplot(hotelnameDf, aes(x=hotel, y=count, fill=sort(status,decreasing = F))) +
  geom_bar(position='dodge', stat = 'identity') +
  scale_fill_manual(values = c('#7BDDFC','#FFBE70')) +
  geom_text(aes(label = sprintf('%d',count)),
            position = position_stack(0.9),
            hjust = 0.5, vjust = -0.5) +
  scale_y_discrete() +
  enter_grow() + 
  transition_states(status, transition_length = 4, state_length = 1)
animate(p, duration = 10, 
        fps = 20, 
        width = 400, 
        height = 400, 
        renderer = gifski_renderer())"

# 년도별 예약/취소 현황
year.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_year), FUN=sum)
year.reserved <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$arrival_date_year), FUN=sum)
"
year.canceled$fraction <- year.canceled$x / sum(year.canceled$x)
year.canceled$ymax <- cumsum(year.canceled$fraction)
year.canceled$ymin <- c(0, head(year.canceled$ymax, n=-1))
year.canceled$labelPosition <- (year.canceled$ymax+year.canceled$ymin) / 2
year.canceled$label <- paste0(year.canceled$Group.1, '\n count: ', year.canceled$x)
  
year.reserved$fraction <- year.reserved$x / sum(year.reserved$x)
year.reserved$ymax <- cumsum(year.reserved$fraction)
year.reserved$ymin <- c(0, head(year.reserved$ymax, n=-1))
year.reserved$labelPosition <- (year.reserved$ymax+year.reserved$ymin) / 2
year.reserved$label <- paste0(year.reserved$Group.1, '\n count: ', year.reserved$x)
library(gridExtra)
p1 <- ggplot(year.canceled, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3, fill=Group.1))+
  ggtitle('Cancellation Status by Year') +
  geom_rect() + geom_text(x=2.3, aes(y=labelPosition, label=label), size =4)+ 
  coord_polar(theta='y') +
  scale_fill_viridis(discrete = F) +
  xlim(c(1,4)) + theme_void() +
  theme(legend.position = 'none', plot.title=element_text(face='bold'))

p2 <- ggplot(year.reserved, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3, fill=Group.1))+
  ggtitle('Reservation Status by Year') +
  geom_rect() + geom_text(x=2.3, aes(y=labelPosition, label=label), size =4)+ 
  coord_polar(theta='y') +
  scale_fill_viridis(discrete = F) +
  xlim(c(1,4)) + theme_void() + 
  theme(legend.position = 'none',plot.title=element_text(face='bold'))
"
d1

ggplot(d1, aes(x=year, y=count, fill=year)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(alpha=.5) 

d1 <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$arrival_date_year, hotelDf$arrival_date_month), FUN=sum)
d2 <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_year, hotelDf$arrival_date_month), FUN=sum)
d1$Group.1 <- factor(d1$Group.1)
d2$Group.1 <- factor(d2$Group.1)
colnames(d1) <- c('year','month','count')
colnames(d2) <- c('year','month','count')
p1 <- ggplot(d1, aes(x=year, y=count, fill=year)) +
  ggtitle('Reservation Status by year') +
  geom_violin(position='dodge', alpha=0.5, scale = 'count', color=NA, trim = F) + 
  theme_ipsum()
p2 <- ggplot(d2, aes(x=year, y=count, fill=year)) + 
  ggtitle('Cancellation Status by year')+
  geom_violin(position='dodge', alpha=0.5, scale = 'count', color=NA, trim = F) + theme_ipsum()
grid.arrange(arrangeGrob(p1, p2, ncol=2), nrow = 1) 

# 나라별 예약/취소 현황
country.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$country), FUN=sum)
country.reserved <- aggregate(!hotelDf$is_canceled, by=list(hotelDf$country), FUN=sum)

countryDf <- inner_join(country.reserved, country.canceled, by="Group.1")
colnames(countryDf) <- c('country', 'reserved', 'canceled')
countryDf 

library(viridis)

p <- ggplot(countryDf, aes(reserved, canceled)) +
  ggtitle("Reservation/Cancellation Status by Country") +
  geom_point(aes(color = country, size = reserved+canceled), alpha = 0.5) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  scale_size(range=c(1,15)) +
  theme_bw() +
  theme(legend.position = 'none', plot.title = element_text(face='bold'),
        text = element_text(family = "Consolas", face='bold'))

ggplotly(p)

# 사람 수별 예약/취소 현황

hotelDf$family <- hotelDf$adults+hotelDf$children+hotelDf$babies
rm(familyDf)
familyDf <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$family), FUN=sum)
familyDf$Group.1 <- factor(familyDf$Group.1)
colnames(familyDf) <- c('family','count')

familyDf2 <- aggregate(hotelDf$is_canceled, by=list(hotelDf$family), FUN=sum)
familyDf2$Group.1 <- factor(familyDf2$Group.1)
colnames(familyDf2) <- c('family','count')

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

# 월별 예약/취소 현황
library(dplyr) 
library(reshape2)

month.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_month,hotelDf$hotel), FUN=sum)
month.reserved <- aggregate(hotelDf$is_canceled+!hotelDf$is_canceled, by=list(hotelDf$arrival_date_month,hotelDf$hotel), FUN=sum)

"monthDf <- inner_join(month.reserved, month.canceled, by='Group.1')
colnames(monthDf) <- c('month', 'reserved', 'canceled')
monthDf$month <- c(4,8,12,2,1,6,7,3,5,11,10,9)
monthDf <- monthDf[order(monthDf$month),]
monthDf"

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
data1

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
data2 <- rbind(rep(4000,10) , rep(0,10) , data2)

colors_border=c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

par(mfrow=c(1,2))
radarchart(data1, axistype=1 , 
            pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1, 
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10000,2500), cglwd=0.8,
            vlcex=0.8, title = 'Reservation Status by Month') 
legend(x=0.9, y=1.2, legend = c('City Hotel','Resort Hotel'), bty = "n", pch=20 ,
       col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
radarchart(data2, axistype=1 , 
           pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1, 
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4000,1000), cglwd=0.8,
           vlcex=0.8, title = 'Cancellation Status by Month')
par(mfrow=c(1,1))
