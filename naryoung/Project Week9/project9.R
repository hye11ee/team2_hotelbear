
library(ggplot2)
library(plotly)
library(dplyr)
library(extrafont)

hotelDf <- read.csv('./hotel_bookings.csv')
str(hotelDf)

sum(is.na(hotelDf))
hotelDf <- na.omit(hotelDf)
sum(is.na(hotelDf))

hotelDf <- hotelDf[hotelDf$country!='NULL',]
hotelDf <- hotelDf[hotelDf$adr>0,]

# 호텔별 예약/취소 현황
hotel.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$hotel), FUN=sum)
hotel.reserved <- aggregate(!hotelDf$is_canceled, by=list(hotelDf$hotel), FUN=sum)

hotelnameDf <- rbind(hotel.reserved,hotel.canceled)
colnames(hotelnameDf) <- c('hotel', 'count')
hotelnameDf$status <- c('reserved','reserved','canceled','canceled')

ggplot(hotelnameDf, aes(x=hotel, y=count, fill=status)) + 
  ggtitle("Reservation/Cancellation Status by Hotel") +
  geom_bar(position='dodge', stat = 'identity') + 
  scale_fill_manual(values = c('#7BDDFC','#FFBE70')) +
  geom_text(aes(label = sprintf('%d',count)),
            position = position_dodge(0.9),
            hjust = 0.5, vjust = -0.5) +
  scale_y_discrete() +
  coord_cartesian(ylim = c(0,50000)) +
  theme(panel.background = element_blank(),
        text = element_text(face='bold'), 
        plot.title = element_text(face='bold'),
        axis.title.x = element_text(size=15, face='bold'),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=15, face='bold'),
        legend.position = 'top')

# 년도별 예약/취소 현황
year.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_year), FUN=sum)
year.reserved <- aggregate(!hotelDf$is_canceled, by=list(hotelDf$arrival_date_year), FUN=sum)

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
  ggtitle("Cancellation Status by Year") +
  geom_rect() + geom_text(x=2.3, aes(y=labelPosition, label=label), size =4)+ 
  coord_polar(theta='y') +
  scale_fill_viridis(discrete = F) +
  xlim(c(1,4)) + theme_void() +
  theme(legend.position = 'none', plot.title=element_text(face='bold'))

p2 <- ggplot(year.reserved, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3, fill=Group.1))+
  ggtitle("Reservation Status by Year") +
  geom_rect() + geom_text(x=2.3, aes(y=labelPosition, label=label), size =4)+ 
  coord_polar(theta='y') +
  scale_fill_viridis(discrete = F) +
  xlim(c(1,4)) + theme_void() + 
  theme(legend.position = 'none',plot.title=element_text(face='bold'))

grid.arrange(arrangeGrob(p2, p1, ncol=2), nrow = 1)
"library(maps)
library(mapproj)

world <- map_data(map='world')

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_polygon(color='black', fill='white')+
  coord_quickmap() +
  theme(panel.background = element_rect(fill = '#d8e4ec', colour = 'lightgray'))"


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
  scale_size(range=c(1,12)) +
  theme_bw() +
  theme(legend.position = 'none', plot.title = element_text(face='bold'),
        text = element_text(family = "Consolas", face='bold'))

ggplotly(p)

# 사람 수별 예약/취소 현황
str(hotelDf)

hotelDf$family <- hotelDf$adults+hotelDf$children+hotelDf$babies
familyDf <- aggregate(hotelDf$is_canceled, by=list(hotelDf$family), FUN=sum)

ggplot(familyDf, aes(Group.1, x)) +
  geom_violin(aes(fill=Group.1), alpha=0.5, color=NA) +
  geom_jitter(aes(color=Group.1), size=2)

str(hotelDf)

# 월별 예약/취소 현황
library(dplyr) 
library(reshape2)

month.canceled <- aggregate(hotelDf$is_canceled, by=list(hotelDf$arrival_date_month), FUN=sum)
month.reserved <- aggregate(!hotelDf$is_canceled, by=list(hotelDf$arrival_date_month), FUN=sum)

month.canceled
monthDf <- inner_join(month.reserved, month.canceled, by="Group.1")
colnames(monthDf) <- c('month', 'reserved', 'canceled')
monthDf$month <- c(4,8,12,2,1,6,7,3,5,11,10,9)
monthDf <- monthDf[order(monthDf$month),]
"
a <- select(monthDf,c(month,reserved,canceled))
b <- melt(a,id.vars = 'month')
ggplot(b, aes(month, value))+
  geom_area(aes(col=variable, fill=variable), alpha=0.5,
            position=position_identity()) 

ggplot(monthDf, aes(x=month, y=reserved)) +
  geom_point() + 
  geom_segment( aes(x=month, xend=month, y=0, yend=reserved))"

ggplot(monthDf, aes(x=month, y=reserved)) + 
  geom_area(aes(month,reserved+canceled),col="orangered",fill="orangered",size=1,alpha=0.3)+ 
  geom_area(aes(month,canceled),col="cyan4",fill="cyan4",size=1,alpha=0.3) 
