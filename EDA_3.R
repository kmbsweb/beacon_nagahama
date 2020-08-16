
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
  
#summarize(cnt = n()) %>%
  #spread(key = device,value = cnt) %>%



table <-read.csv("table.csv",header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

ggplot(aes(hour)) + 
  theme_minimal() +
  geom_histogram(fill="darkgrey")