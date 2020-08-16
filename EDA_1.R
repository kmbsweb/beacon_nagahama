install.packages("tictoc")
library(tictoc)

##packages
library(dplyr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)

setwd("~/Desktop/beacon_test/contact cnt")
##可視化
file_name <- list.files(full.names =F)
OutData <- data.frame()

for(i in 1:length(file_name)) {
  ##データの読み込み
  rdata <-read.csv(file_name[i],header=F, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)
  rdata$V5 <- as.POSIXct(rdata$V5, origin="1970-01-01")
  rdata$V6 <- str_sub(file_name[i], start=1, end=8)
  OutData <- rbind(OutData,rdata)
}



table <-read.csv("table.csv",header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

OutData　%>% 
  mutate(date = date(V5),
         hour = hour(V5)) %>%
  group_by(V6,hour) %>% 
  summarise(cnt = n()) %>% 
  rename(デバイス_ID = V6) %>% 
  inner_join(table, by="デバイス_ID") %>% 
  ggplot() +  
  geom_line(aes(x = hour, y = cnt)) +
  facet_wrap(~ 設置場所) + 
  theme_bw(base_family = "HiraKakuProN-W3") 

#weekdays:曜日のmerge

#日本標準時=JST
##スポット毎での出現回数
Freq <- OutData　%>%
  group_by(V1,V6) %>%
  tally() %>%
  spread(V6, n)

test <- Freq %>%
  mutate_all(funs(ifelse( .>=1,1,0))) %>%
  mutate_all(funs(ifelse(is.na(.),0,.))) %>%
  mutate(total = rowSums(across(is.numeric)))
  
##滞在時間
duration <- OutData %>%
  select(V1,V5) %>%
  rename(person_ID = V1, time = V5) %>%
  group_by(person_ID) %>%
  summarize(duration = max(time)-min(time))  

#secs to minute
#duration$duration <- seconds_to_period(duration$duration)

DF_2 <- test %>%
  rename(person_ID = V1) %>%
  inner_join(duration, by="person_ID")

DF_2 %>%
  mutate(minute = as.numeric(duration)/60) %>%
  filter(total >= 2 && minute >= 20 && minute <= 300) %>%
  ggplot(aes(minute)) + 
  theme_minimal() +
  geom_histogram(fill="darkgrey")

##interval 
# 説明用にintervalを準備
x_i1 <- interval(ymd("2018-01-01"), ymd("2018-01-31"))

# %within%演算子のテスト
# れは含まれるのでTRUE
ymd("2018-01-10") %within% x_i1

## 20200411-20　課題
## x:接触回数 y:接触日数
setwd("~/Desktop/beacon_test/contact cnt")

file_name <- list.files(full.names =F)
OutData <- data.frame()

for(i in 1:length(file_name)) {
  ##データの読み込み
  rdata <-read.csv(file_name[i],header=F, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)
  rdata$V5 <- as.POSIXct(rdata$V5, origin="1970-01-01")
  rdata$V6 <- str_sub(file_name[i], start=1, end=8)
  rdata$V7 <- str_sub(file_name[i], start=10, end=17)
  OutData <- rbind(OutData,rdata)
}

PCON <- OutData %>%
  select(V1,V5,V6,V7) %>%
  rename(person_ID = V1, time_stamp = V5, device = V6 , date = V7) %>%
  group_by(person_ID, date) %>%
  summarize(contact = n())%>%
　#arrange(desc(contact))
  group_by(person_ID) %>%
  summarize(day = n(),
            contact_mean = mean(contact)) 


library(fancycut)

PCON$class <- fancycut(x = PCON$contact_mean,
                       `10^0`   ='[0,1]',
                       `10^0.5` ='(1, 3.162278]',
                       `10^1`   ='(3.162278, 10]',
                       `10^1.5` ='(10, 31.62278]',
                       `10^2`   ='(31.62278, 100]',
                       `10^2.5` ='(100, 316.2278]',
                       `10^3`   ='(316.2278, 1000]',
                       `10^3.5` ='(1000, 3162.278]',
                       `10^4`   ='(3162.278, 10000]')


remotes::install_github("inbo/INBOtheme")
library(INBOtheme)

PCON %>%
  group_by(day,class) %>%
  summarize(cnt = n()) %>%
ggplot(aes(as.factor(day), class)) +
       geom_tile(aes(fill = cnt)) + 
       geom_text(aes(fill = cnt, label = cnt), size = 2.0, color="black") +
       scale_fill_gradient2()  +
       theme_minimal() 



setwd("~/Desktop/beacon_test")
table <-read.csv("table.csv",header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

OutData %>%
  group_by(V6,V7) %>%
  summarize(cnt = n()) %>%
  rename(デバイス_ID = V6) %>% 
  inner_join(table, by="デバイス_ID") %>% 
ggplot(aes(V7, 設置場所)) +
  geom_tile(aes(fill = cnt)) + 
  geom_text(aes(fill = cnt, label = cnt), size = 2.0, color="black") +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal(base_family = "HiraKakuPro-W3") 


##同行者の抽出
Small <- OutData[1:200,]
Small$V8 <- str_sub(Small$V5, start=1, end=16)
duplicated(Small$V6, Small$V8)
Small2 <- Small[,8][!duplicated(Small[c("V6","V8")]),]


##define tourist
##only 1 day, 3 times over

tourist_A <- PCON %>%
  filter(day==1, contact_mean > 10 )

tourist_A1 <- OutData %>%
  filter(V1 %in% tourist_A$person_ID)

##滞在時間
tourist_A1 %>%
  select(V1,V5) %>%
  rename(person_ID = V1, time = V5) %>%
  group_by(person_ID) %>%
  summarize(duration = max(time)-min(time))  %>%
  mutate(minute = as.numeric(duration)/60) %>%
  #filter(total >= 2 && minute >= 20 && minute <= 300) %>%
  ggplot(aes(minute)) + 
  theme_minimal() +
  geom_histogram(fill="darkgrey")

##開始時間
tourist_A1 %>%
  select(V1,V5) %>%
  rename(person_ID = V1, time = V5) %>%
  group_by(person_ID) %>%
  summarize(duration = max(time)-min(time),
            start = min(time),
            end = max(time) ,
            start_h = hour(start),
            end_h = hour(end)) %>%
  ggplot(aes(end_h)) + 
  theme_minimal() +
  geom_histogram(binwidth = 1, fill="darkgrey")

## 20200505　課題
## 2日滞在の抽出
file_name <- list.files(full.names =F)
OutData <- data.frame()

for(i in 1:length(file_name)) {
  ##データの読み込み
  rdata <-read.csv(file_name[i],header=F, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)
  rdata$V5 <- as.POSIXct(rdata$V5, origin="1970-01-01")
  rdata$V6 <- str_sub(file_name[i], start=1, end=8)
  rdata$V7 <- str_sub(file_name[i], start=10, end=17)
  OutData <- rbind(OutData,rdata)
}

OutData <- OutData %>%
  select(V1,V5,V6,V7) %>%
  rename(person_ID = V1, time_stamp = V5, device = V6 , date = V7)

##接触日数×観測スポット数

two_day <- OutData %>%
  group_by(person_ID, date) %>%
  summarize(contact = n()) %>%
  group_by(person_ID) %>%
  summarize(day = n()) %>%
  filter(day==2)
  
t <- filter(OutData, person_ID %in% two_day$person_ID) %>%
  group_by(person_ID,date) %>%
  summarize(day = n()) %>%
  group_by(person_ID) %>%
  summarize(seq = as.integer(max(date))-as.integer(min(date)),
            sum_cnt = sum(day)) %>%
  filter(seq==1)

tt <- filter(OutData, person_ID %in% t$person_ID)  %>%
  group_by(person_ID) 

save(tt, file = "2days.rda")


t$class <- fancycut(x = t$sum_cnt,
                       `10^0`   ='[0,1]',
                       `10^0.5` ='(1, 3.162278]',
                       `10^1`   ='(3.162278, 10]',
                       `10^1.5` ='(10, 31.62278]',
                       `10^2`   ='(31.62278, 100]',
                       `10^2.5` ='(100, 316.2278]',
                       `10^3`   ='(316.2278, 1000]',
                       `10^3.5` ='(1000, 3162.278]',
                       `10^4`   ='(3162.278, 10000]')


t_2 <- filter(OutData, person_ID %in% t$person_ID) %>%
  group_by(person_ID) %>%
  summarise(Unique_device = n_distinct(device)) 

t_all <- inner_join(t, t_2, by="person_ID")
head(t_all)
h <- filter(t_all, Unique_device >=  7 ) 

colnames(t_all)
t_all %>%
  group_by(Unique_device, class) %>%
  summarize(cnt = n()) %>%
ggplot(aes(as.factor(Unique_device), class)) +
  geom_tile(aes(fill = cnt)) + 
  geom_text(aes(fill = cnt, label = cnt), size = 2.0, color="black") +
  scale_fill_gradient2()  +
  theme_minimal() 

##開始時刻×終了時刻
filter(OutData, person_ID %in% h$person_ID) %>%
  group_by(person_ID) %>%
  summarise(start = min(time_stamp),
            end = max(time_stamp) ,
            start_h = hour(start),
            end_h = hour(end)) %>%
group_by(start_h, end_h) %>%
  summarize(cnt = n()) %>%
  ggplot(aes(as.factor(start_h), as.factor(end_h))) +
  geom_tile(aes(fill = cnt)) + 
  geom_text(aes(fill = cnt, label = cnt), size = 2.0, color="black") +
  scale_fill_gradient2()  +
  theme_minimal() 

##開始場所×終了場所
t_3 <- filter(OutData, person_ID %in% t$person_ID) %>%
  group_by(person_ID)　%>%
  summarise(start = min(time_stamp),
            end = max(time_stamp) ,
            start_h = hour(start),
            end_h = hour(end))　%>%
　mutate(ID_start = paste(!!!rlang::syms(c("person_ID", "start")), sep="-"))　%>%
  mutate(ID_end = paste(!!!rlang::syms(c("person_ID", "end")), sep="-"))

OutData <- OutData　%>%
  mutate(ID_time = paste(!!!rlang::syms(c("person_ID", "time_stamp")), sep="-"))

#開始場所
t_4 <- filter(OutData,ID_time %in% t_3$ID_start) %>%
  select(person_ID, device) %>%
  rename(first_spot = device)

#終了場所
t_5 <- filter(OutData,ID_time %in% t_3$ID_end) %>%
  select(person_ID, device) %>%
  rename(last_spot = device)

t_all <- inner_join(t_3, t_4, by="person_ID") %>%
         inner_join(t_5, by="person_ID")

t_all  %>%
group_by(first_spot, last_spot) %>%
  summarize(cnt = n()) %>%
  ggplot(aes(first_spot, last_spot)) +
  geom_tile(aes(fill = cnt)) + 
  geom_text(aes(fill = cnt, label = cnt), size = 2.0, color="black") +
  scale_fill_gradient2()  +
  theme_minimal() 

head(t_all)
t <- t_all  %>%
  mutate(start_ymd_hm = str_sub(start, start=1, end=16)) %>%
  mutate(end_ymd_hm = str_sub(end, start=1, end=16))

result <- t %>% group_by(first_spot, last_spot, start_ymd_hm,end_ymd_hm) %>%
  filter(n() == 3)

