library(readr)
library(purrr)
library(naniar)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/USER/Desktop/chuseok")
df <- read.csv("covid9_22.csv", head=T)

View(df)
str(df)
View(df)

# 결측치 제거
vis_miss(df)
gg_miss_var(df, show_pct = TRUE)
table(is.na(df))
df <- na.omit(df)

# 성별 시각화
df %>%
  group_by(gender) %>%
  summarise(L = length(gender)) %>%
  ggplot() +
  geom_bar(aes(x = gender, y = L, fill = gender), stat='identity',
           position = position_dodge() ,alpha = 0.7) +
  ggtitle("Patient Gender") +
  geom_text(aes(x = gender, y = L, label = L), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  xlab("Gender") +
  ylab("Patient Count") +
  scale_y_continuous(breaks=seq(0, 2000, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

# 월별 확진자수
df$date_m <- substr(df$date,7,7)
df$date_m

df %>%
  group_by(date_m) %>%
  summarise(N = length(date_m)) %>%
  ggplot() +
  geom_bar(aes(x = date_m, y = N, fill=date_m), stat='identity',
           position = position_dodge() ,alpha = 0.7) +
  geom_text(aes(x = date_m, y =N, label = N), vjust=-0.1, color="black",
            position = position_dodge(0.9), size=3.5) +
  ggtitle("Monthly Patient") +
  xlab("Month") +
  ylab("Patient Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  labs(fill = "Month")

# 현재까지 확진자수(사랑제일교회 8.12 첫 확진부터)
glimpse(df)

df$date <- as.character(df$date)
table(df$date)
df$date <- as.Date(df$date, "%Y. %m. %d")
df$date
  
date <- subset(df, df$date >= "2020-08-12")

date_md <- format(date$date, "%m/%d")
table(date_md)

date %>%
  group_by(date) %>%
  summarise(M = length(date)) %>%
  ggplot() +
  geom_bar(aes(x = date, y = M, fill = date), stat='identity',
           position = position_dodge() ,alpha = 0.7) +
  geom_text(aes(x = date, y = M, label = M), vjust=-0.1, color="black",
            position = position_dodge(0.9), size=3) +
  ggtitle("Daily Patient from 8/12") +
  xlab("Day") +
  ylab("Patient Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))
  #labs(fill = "Month")





# 연령별 확진자수
df %>%
  group_by(age_group) %>%
  summarise(L = length(age_group)) %>%
  ggplot() +
  geom_bar(aes(x = age_group, y = L, fill = age_group), stat='identity',
           position = position_dodge() ,alpha = 0.7) +
  ggtitle("Patient Age Group") +
  geom_text(aes(x = age_group, y = L, label = L), vjust=0.1, color="black",
            position = position_dodge(0.9), size=3.5) +
  xlab("Age Group") +
  ylab("Patient Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))


# 지역별 확진자수

## 1. 세부지역
df %>%
  group_by(area) %>%
  summarise(L = length(area)) %>%
  ggplot() +
  geom_bar(aes(x = area, y = L, fill = area), stat='identity',
           position = position_dodge() ,alpha = 0.9, width = 0.3) +
  ggtitle("Patient Area (1)") +
  geom_text(aes(x = area, y = L, label = L), vjust=0.2, color="black",
            position = position_dodge(0.9), size=3) +
  xlab("Area (1)") +
  ylab("Patient Count") +
  scale_y_continuous(breaks=seq(0, 2000, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size = 3),legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=45, hjust=1))



## 2. 시별 구분
name_split <-data.frame(df$number,df$area_num)
name_split2<-data.frame(do.call('rbind', strsplit(as.character(name_split$df.area_num), split='-', fixed=T))) 
df$area_num2<-name_split2$X1

df %>%
  group_by(area_num2) %>%
  summarise(L = length(area_num2)) %>%
  ggplot() +
  geom_bar(aes(x = area_num2, y = L, fill = area_num2), stat='identity',
           position = position_dodge() ,alpha = 0.9, width=0.5) +
  ggtitle("Patient Area (2)") +
  geom_text(aes(x = area_num2, y = L, label = L), vjust=0.2, color="black",
            position = position_dodge(0.9), size=3) +
  xlab("Area (2)") +
  ylab("Patient Count") +
  scale_y_continuous(breaks=seq(0, 2000, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size =3),legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## 경기 북부, 남부로 비교
df$area_num3<-ifelse(df$area_num2=='고양'|df$area_num2=='남양주'|df$area_num2=='파주'|df$area_num2=='의정부'|df$area_num2=='양주'|df$area_num2=='구리'|df$area_num2=='포천'|df$area_num2=='동두천'|df$area_num2=='가평'|df$area_num2=='연천','경기북부','경기남부')

df %>%
  group_by(area_num3) %>%
  summarise(N = length(area_num3)) %>%
  ggplot() +
  geom_bar(aes(x = area_num3, y = N, fill=area_num3), stat='identity',
           position = position_dodge() ,alpha = 0.7) +
  geom_text(aes(x = area_num3, y =N, label = N), vjust=-0.1, color="black",
            position = position_dodge(0.9), size=3.5) +
  ggtitle("Patient Area (3)") +
  xlab("Area(3)") +
  ylab("Patient Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# 감염경로별 확진자수

df %>%
  group_by(infect_route) %>%
  summarise(L = length(infect_route)) %>%
  ggplot() +
  geom_bar(aes(x =infect_route, y = L, fill = infect_route), stat='identity',
           position = position_dodge() ,alpha = 0.9, width=0.5) +
  ggtitle("Patient infect_route") +
  geom_text(aes(x = infect_route, y = L, label = L), vjust=0.2, color="black",
            position = position_dodge(0.9), size=3) +
  xlab("infect_route") +
  ylab("Patient Count") +
  scale_y_continuous(breaks=seq(0, 2000, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),legend.position = "bottom")

