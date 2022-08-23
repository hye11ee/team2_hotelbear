library(RColorBrewer)
library(ggplot2)
library(shiny)
library(treemap)
library(d3treeR)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(rgdal)

merge_df <- read.csv('use_merge', col.names=c('NAME', 'mv5'))

world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/DATA/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
world_spdf2 <- world_spdf
world_spdf2@data <- merge(world_spdf2@data, df_merge, all.x = T, sort=F)
mypalette <- colorNumeric( palette="viridis", domain=world_spdf2@data$mv5, na.color="transparent")


# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf2@data$NAME,"<br/>", 
  "Area: ", world_spdf2@data$AREA, "<br/>", 
  "many_visit /first_visit* 5: ", round(world_spdf2@data$mv5, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map 
m <-leaflet(world_spdf2) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(mv5), 
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
  addLegend( pal=mypalette, values=~mv5, opacity=0.9, title = "many_visit / first_visit ", position = "bottomleft" )

m
