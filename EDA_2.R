
setwd("~/Desktop/beacon_test")
load("2days.rda")
library(dplyr)
library(tidyverse)
##hotel ID
##100014A5:北びわこホテルグラッツェ
##100014A4:ロイヤル

hotel_person <- filter(tt, device %in% c("100014A4","100014A5")) %>%
  distinct(person_ID, .keep_all = FALSE) 

hotel <- filter(tt, person_ID %in% hotel_person$person_ID)  %>%
  group_by(person_ID,device) %>%
  summarize(cnt = n()) %>%
  spread(key = device,value = cnt) %>%
  mutate_all(funs(ifelse(is.na(.),0,.))) 

write.csv(hotel,"2days.csv")

#{arules}パッケージのロード
install.packages("arules")
library(arules)
#データの読み込み
df <- read.csv("tourist850_1.csv",fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

#トランザクションデータに変換
tran <- as(as.matrix(df[,2:ncol(df)]),"transactions")

itemFrequencyPlot(tran,type = "absolute")

#アプリオリアルゴリズム
data.ap <- apriori(tran, parameter = list(supp =0.0001,maxlen=5,confidence=0.01))
print(data.ap)
AA <- as.data.frame(inspect(data.ap, by = "count", n=20))


##duratrion distribution
filter(tt, person_ID %in% df$person_ID) %>%
  group_by(person_ID) %>%
  summarize(duration = max(time_stamp)-min(time_stamp))  %>%
  mutate(hour = as.numeric(duration)/60/60) %>%
  ggplot(aes(hour)) + 
  theme_minimal() +
  geom_histogram(fill="darkgrey")


##開始時刻×終了時刻
filter(tt, person_ID %in% df$person_ID) %>%
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

TT <- filter(tt, person_ID %in% df$person_ID) 
write.csv(TT,"2days_2.csv")         
