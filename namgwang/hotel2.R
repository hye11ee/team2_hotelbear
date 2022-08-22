library(RColorBrewer)
library(tidyverse)

brewer.pal(9, 'Blues')
display.brewer.all()
pie(rep(1,9), col=brewer.pal(9, 'OrRd'))
df <- read.csv('hotel_bookings.csv')
df <- df[complete.cases(df),]


# input: vector(양수 값), output: 색깔
color_ft <- function(vec) {
  d <- c()
  for (i in vec) {
    d <- c(d,brewer.pal(9, 'OrRd')[3+ round(6*i / max(vec))])
  }
  return (d)
}

d <- c(1,3,5,7,4,4,0.5)

color_ft(d)
pie(d, col=color_ft(d))
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

unique(df[df$is_repeated_guest == 1,]$country)

df <- df[df$country != 'NULL',]

df[rownames(subset(df, country== 'CN')),'country'] <- 'CHN'
unique(df[df$is_repeated_guest == 1,]$country)  # 38개국


df_wng <- df

df_wng$is_repeated_guest <- as.numeric(df_wng$is_repeated_guest)-1
df_wng$val1 <- c(1)


df_wng2 <- aggregate(df_wng[,c(17,33 )],by=list(country=df_wng$country) ,FUN = sum)
df_wng2 <- df_wng2[df_wng2$is_repeated_guest != 0,]
df_wng2$mv <- 100 * df_wng2$is_repeated_guest / df_wng2$val1
df_wng2[order(df_wng2$mv, decreasing = T),]
df_wng2
world$region %in% df_wng2$country

df_wng2 <- df_wng2[order(df_wng2$mv, decreasing=T),]
df_wng2
# 지도 그리기 -> 뚜렷한 값이 안나옴
world <- map_data('world')

df_wng2$country2 <- c('Puerto Rico', 'Portugal', 'Angola', 'Slovenia', 'Turkey', 'Peru', 'Lebanon', 'Greece', 'Nigiria', 'Algeria', 'Iceland', 'Japan', 'South Africa', 'Luxembourg', 'Italy', 'Sweden', 'Spain', 'Ireland', 'Netherlands', 'Austria', 'USA','GBR','South Korea', 'Belgium', 'France','Brazil', 'Poland', 'China', 'Argentina','Australia',
                      'Finland', 'Israel', 'Morocco', 'Germany', 'Norway', 'Denmark', 'Switzerland', 'Russia')
df_wng2 <- df_wng2[,c(4,5)]
names(df_wng2) <- c('mv','region')
df_wng2



df_merge <- merge(df_wng2, world, all.y=T)
df_merge[is.na(df_merge$mv),c('mv')] <- 0

df_merge <- df_merge[order(df_merge$order, decreasing = F),]

df_merge <- df_merge[df_merge$region != 'Puerto Rico',]
df_color <- df_merge$mv
color_map <- ggplot(df_merge, aes(long, lat, group=group)) + 
  geom_polygon(fill=color_ft(df_color), color='gray')
color_map

write.csv(df_merge, 'hotel_merge')


read.csv('hotel_merge')
# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(tidyverse)


# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")


# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(year)


# violin graph
df$market_segment
df$arrival_date_year
# Most basic violin chart
p <- ggplot(df, aes(x=is_repeated_guest, y=adr, fill=is_repeated_guest)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin() +coord_flip() + facet_wrap(~arrival_date_year, nrow=2, labeller ='label_both')
p




# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)

library(tidyverse)
library(lubridate)
library(gifski)
# devtools::install_github("thomasp85/transformr")
library(transformr)

# dg<- subset(df, is_repeated_guest==1)
# dg$val <- c(1)
# dg <- aggregate(dg[,c('val')],by=list(country=dg$country, year=dg$arrival_date_year, month=dg$arrival_date_month) ,FUN = sum)
# 
# dg$year <- as.numeric(dg$year) + 2014
# ymd('2014 October 1')
# dg$time <-ymd(paste(dg$year,' ', dg$month ,' ', '1'))
# 
# 
# dg <- dg[,c(1,4,5)]
# 
# p <- dg %>%
#   ggplot( aes(x=time, y=x, group=country, color=country)) +
#   geom_line() +
#   geom_point() +
#   ggtitle("Popularity of American names in the previous 30 years") +
#   theme_ipsum() +
#   ylab("Number of babies born")
# length(dg$country) * length(dg$time)
# 
#
# 
# p +   transition_time(time)
# ?transition_time
# 
# animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
# # Save at gif:
# anim_save("287-smooth-animation-with-tweenr.gif")




# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip DATA/world_shape_file.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

# Read this shape file with the rgdal library. 
library(rgdal)
library(leaflet)
?readOGR
?paste0
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/DATA/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Clean the data object
library(dplyr)
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

# -- > Now you have a Spdf object (spatial polygon data frame). You can start doing maps!

world_spdf@data

names(df_wng2) <- c('mv', 'NAME')
df_merge <- merge(df_wng2, world_spdf@data, all.y=T)
df_merge[is.na(df_merge$mv),c('mv')] <- 0
df_merge$mv

# Library
library(leaflet)

# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=df_merge$mv, na.color="transparent")
mypalette(c(45,43))

mypalette(POP2005)
class(world_spdf)
# Basic choropleth with leaflet?
m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

m


# load ggplot2
library(ggplot2)

# Distribution of the population per country?
world_spdf@data %>% 
  ggplot( aes(x=as.numeric(POP2005))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  xlab("Population (M)") + 
  theme_bw()

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet1.html"))

# Color by quantile
m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )
m

# Numeric palette
m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("YlOrRd", POP2005)(POP2005) )
m

# Bin
m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorBin("YlOrRd", POP2005)(POP2005) )
m

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet2.html"))


# Create a color palette with handmade bins.
library(RColorBrewer)
mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Area: ", world_spdf@data$AREA, "<br/>", 
  "Population: ", round(world_spdf@data$POP2005, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map -> 그냥 이게 핵심
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(POP2005), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet5.html"))
