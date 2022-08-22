

# 데이터 로딩
df <- read.csv('hotel_bookings.csv')

# 정보 확인
str(df)
dim(df)
names(df)

# unique value 확인
head(sort(table(df[[14]]), decreasing = T), 50)
prop.table(table(df[['is_repeated_guest']]))
unique(df[['customer_type']])

# 결측치 확인 및 제거
library(VIM)
library(ggplot2)
aggr(df)
sum(is.na(df$children)) # 결측치 4개 확인 
df <- df[complete.cases(df),]

# 재방문 데이터셋 생성
df_rep <- subset(df,is_repeated_guest == 1)
table(df_rep$country) # 43개(NULL값 포함)
# NULL 값 제거 및 CN -> CHN
df_rep <- df_rep[df_rep$country != 'NULL',]
dim(df_rep)
df_rep[rownames(subset(df_rep, country== 'CN')),'country'] <- 'CHN'
table(df_rep$country) # 41개 
# write.csv(df_rep, file= 'hotel_rep.csv')

# 컬럼추가(PRT 유무 - PRT , NOT PRT)
df_rep$is_prt <- ifelse(df_rep$country == 'PRT', 'PRT', 'NOT PRT')
prop.table(table(df_rep$is_prt))

# 그룹 연산
df_temp <-df_rep[,c('country', 'is_prt')]

df_agg <- aggregate(x=df_temp, list(group=df_temp$is_prt, subgroup =df_temp$country), length)
names(df_agg) <- c('is_prt', 'country', 'value', 'value2')
df_agg <- df_agg[,c(1,2,3)]
df_agg$continent <- c('Africa', 'Asia', 'America', 'Asia', 'Europe', 'Europe', 'America', 'Europe', 'Asia', 'Europe', 'Europe', 'Africa', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Asia', 'Europe', 'Asia', 'Asia', 'Asia', 'Europe', 'Africa', 'Africa', 'Africa', 'Europe', 'Europe', 'America', 'Europe', 'America', 'Europe', 'Europe', 'Europe', 'Europe', 'Africa', 'Asia', 'America', 'Africa' )
df_agg
write.csv(df_agg, 'hotel_rep_agg.csv')


# library
library(treemap)
library(d3treeR)
# 필요한 패키지 설치
# install.packages('devtools')
# devtools::install_github("timelyportfolio/d3treeR")


# dataset
df_agg <- read.csv('hotel_rep_agg.csv')
df_agg
# basic treemap
?treemap
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
# save the widget
# library(htmlwidgets)
# saveWidget(inter, file=paste0( getwd(), "/HtmlWidget/interactiveTreemap.html"))

# heatmap 그릴려고 했는데 값도 좋지않고 잘 안됨 (포기)
c(1:4)
rownames(df)
length(rownames(df))
set.seed(300)
df_sample <- df[sample(c(1:length(rownames(df))), 5000),]
df_sample

library(ggplot2)

ggplot(df) +
 aes(x = is_repeated_guest_f, y = adr) +
 geom_boxplot(fill = "#FF8C00") +
 theme_minimal()
,17)]))
cor(as.matrix(df_sample[,c(2, 3, 8, 9, 10, 11, 12 ,17)]))
cor(as.matrix(df_sample[,c(2,17)]))

# 첫 고객과 재방문 고객과 평균 숙박비 차이 비교
library(ggplot2)
df$is_repeated_guest_f <- factor(df$is_repeated_guest)
names(df)
hist(df[,c(28)])
p <- ggplot(data=df[,c(28,33)], aes(x=is_repeated_guest_f))
df[,c(28,17)]

p + geom_boxplot(outlier.colour = 'red')

install.packages('esquisse')

library(tidyverse)
library(ggplot2)

ggplot(df) +
  aes(x =is_repeated_guest_f, y = adr) +
  geom_boxplot(fill = "s#FF8C00") + 
  theme_minimal() + scale_y_log10()

ggplot(df) +
  aes(x =is_repeated_guest_f, y = lead_time) +
  geom_boxplot(fill = "#FF8C00") + 
  theme_minimal()   #scale_y_log10()

ggplot(df) +
  aes(x =is_repeated_guest_f, y = arrival_date_month) +
  geom_boxplot(fill = "#FF8C00") + 
  theme_minimal()   #scale_y_log10()

# dumbbell plot (x축: )
df_year <- data.frame(table(df$arrival_date_year) / c(6, 12, 8))
names(df_year)
p <- ggplot(data=df_year, mapping = aes(x=Var1, y=Freq, fill=Var1))
p +  geom_bar(stat='identity', alpha=.6, width=.4)+
  scale_fill_manual(values=c('skyblue 3', 'yellow 3', 'coral 3'))

# meal, customer_type, reservation_status, reserved_room_type, deposit_type, hotel
p <- ggplot(data=df, mapping = aes(x=df$hotel, fill=df$hotel))
p + geom_bar() +facet_wrap(~is_repeated_guest, nrow=2, scales = 'free', labeller ='label_both')
?facet_wrap
names(df)

df

library(tidyverse)
?mutate
sw <- data.frame(starwars)
sw %>%
  select(name, mass) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )


df <- df[df$adr> 0, ]
?replace
df
df[df$adr>0, ] 
# facet 예시

# library(tidyverse)
# library(datasauRus)
# 
# data(package = 'datasauRus')
# 
# dd <- datasaurus_dozen
# str(dd)
# unique(dd$dataset)
# 
# plot(y~x, data=subset(dd, dataset== 'dino'), pch=19, col='tomato')
# 
# p <- ggplot(data=dd, mapping = aes(x=x, y=y))
# p + geom_point(mapping=aes(color=dataset)) + facet_wrap(~dataset, nrow=4)


