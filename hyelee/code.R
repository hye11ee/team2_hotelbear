library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(colorspace)
library(nord)
library(hexbin)


df <- read.csv('./Desktop/Hotel_booking_demand/hotel_bookings.csv')

str(df)

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

df%>%
  ggplot(aes(x=arrival_date_year,fill=is_canceled))+
  geom_bar(alpha=.6, width=.4)+
  scale_fill_manual(values = c('skyblue 3','yellow'))+
  theme_ridges()

df%>%
  ggplot(aes(x=hotel,fill=is_canceled))+
  geom_bar()


df%>%
  ggplot(aes(x=assigned_room_type,fill=is_canceled))+
  geom_bar()


df%>%
  ggplot(aes(x=distribution_channel,fill=is_canceled))+
  geom_bar()


df%>%
  filter(days_in_waiting_list>1)%>%
  ggplot(aes(x=days_in_waiting_list,fill=is_canceled))+
  geom_histogram(binwidth = 10)


df%>%
  ggplot(aes(x=deposit_type,fill=is_canceled))+
  geom_bar()

df%>%
  ggplot(aes(x=lead_time,fill=is_canceled))+
  geom_histogram(binwidth=10,position="stack")

str(df)

df$canceled <- as.numeric(df$is_canceled)
corr <- cor(df[, sapply(df, is.numeric)])

df_sample <- df[sample(c(1:length(rownames(df))), 5000),]
#corr <- cor(df_sample[, sapply(df_sample,is.numeric)])

ggcorrplot(corr,
           hc.order=TRUE,
           type='lower',
           lab=TRUE,
           outline.color='white',
           colors=diverge_hcl(3, palette='Tropic'))

library(GGally)
ggpairs(df_sample[, sapply(df_sample, is.numeric)], title="correlogram with ggpairs()") 


monthPlot <-
  df %>% 
  mutate(arrival_date_month = factor(
    arrival_date_month,
    levels = c(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ),
    ordered = TRUE
  ))

ggplot(monthPlot, aes(arrival_date_month, fill = is_canceled)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(
    name = "Reservation Status",
    breaks = c("0", "1"),
    label = c("Not Cancelled", "Cancelled")
  ) +
  labs(title = "Reservation Status by Month", x = "Month", y = "Count") +
  scale_x_discrete(labels = month.abb)

df$adr_pp <- df$adr / (df$adults + df$children)
actual_guests <- df[df$is_canceled == '0',]
actual_guests$price <- actual_guests$adr * (actual_guests$stays_in_weekend_nights +actual_guests$stays_in_week_nights)
as.numeric(actual_guests$arrival_date_month)
actual_guests$hotel
str(actual_guests)

ggplot(actual_guests, aes(x=as.numeric(arrival_date_month), y=price))+
  geom_segment(aes(xend=as.numeric(arrival_date_month), yend=0, color=as.numeric(hotel)), size=0.5)+
  geom_point(aes(color=as.numeric(hotel)), size=2)+
  scale_color_continuous_sequential('SunsetDark', rev=TRUE)+
  theme_minimal()
str(df)
df$is_canceled

library(plotly)
plot_ly(df, x=~arrival_date_month) %>% 
  #add_lines(y=~lead_time, color=I('#4B4B4B'), name='DAX') %>% 
  add_lines(y=~stays_in_weekend_nights, color=I('#967D4B'), name='SMI') %>% 
  add_lines(y=~stays_in_week_nights, color=I('#AFAF7D'), name='CAC') %>% 
  add_lines(y=~booking_changes, color=I('#C89632'), name='FTSE')

plot_ly(data=df, type='scatter', mode='lines',
        x=~arrival_date_month, y=~lead_time)

df_us <-  df[df$country == 'USA',]
df_uk <- df[df$country == 'GBR',]
df_po <- df[df$data1.country == 'PRT',]
df_ger <- df[df$country == 'DEU',]
df_sp <- df[df$country == 'ESP',]

df_merge <- rbind(df_us, df_uk, df_po, df_ger, df_sp)

library(gganimate)
library(av)
library(viridis)

ggplot(df_merge, aes(stays_in_weekend_nights, stays_in_week_nights, fill=country))+
  geom_violin(scale='area',
              color=NA,
              alpha=0.5) +
  scale_fill_nord('aurora')+
  ylim(0, 15) +
  transition_states(is_canceled,
                    transition_length=0.5,
                    state_length=0.5)+
  theme_minimal()

ggplot(data=df)+
  geom_hex(mapping=aes(x=adr, 
                       y=lead_time))


getwd()