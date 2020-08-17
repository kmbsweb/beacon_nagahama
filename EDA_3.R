
library(dplyr)
library(lubridate)

load("2day_850.rda")

#2days in a row: remove hotel only
change_pt <- TT %>%
  arrange(time_stamp) %>%
  arrange(person_ID) %>%
  mutate(lag = lag(device, 1)) %>%
  mutate(flg = ifelse(device==lag,0,1))  %>%
  mutate(flg_2 = replace_na(flg, 1))　%>%
  mutate(cumsum = cumsum(flg_2))%>%
  select (-c(lag, flg, flg_2)) %>%
  group_by(person_ID, cumsum) %>%
  summarize(duration =  round((max(time_stamp)-min(time_stamp))/60,3),
            first_obs = min(time_stamp),
            last_obs = max(time_stamp),
            device = device,
            cnt = n()) %>%
  distinct(.keep_all = FALSE) 
  

table <-read.csv("https://raw.githubusercontent.com/kmbsweb/beacon_nagahama/master/table.csv",
                 header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

class(change_pt$duration)
change_pt  %>%
  select(duration,device) %>%
  mutate(hour = duration/60)%>%
  left_join(table, by="device") %>%
  ungroup(person_ID)%>%
  select(hour,設置場所) %>%
  group_by(設置場所) %>%
  summarize(mean   =  mean(hour),
            median = median(hour),
            max    = max(hour),
            min    = min(hour),
            stdev  = sd(hour),
            cnt = n()) %>%
  ggplot(aes(x = 設置場所, y = cnt)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw(base_family = "HiraKakuProN-W3")

tt %>%
  ggplot(aes(hour)) + 
  geom_histogram(fill="darkgrey") +
  facet_wrap(~ 設置場所) 
 

