
#home listing - age of inventory (days)
# https://www.zillow.com/research/data/

# library
library(startingDataScience)
library(tidyverse)

#data
# set working directory
setwd("~/Documents/DailyInterchange/data_sets")

zillow <- read.csv("MedianAgeOfInventory_NSA_AllHomes_Metro.csv", stringsAsFactors = F)
head(zillow)

######### city related ########
# remove first 2 columns
zillow2 <- select(zillow, RegionName:X2019.01)

# filter to atlanta
atl <- filter(zillow2, RegionName == "Atlanta, GA")
atl <- select(atl, X2013.01:X2019.01 )

# tidy 
atl <- gather(atl, date, month) 

# removed 'x' from date
atl <- atl %>%
  mutate(date = str_replace(date, "X", ""))

# what is the date class? - character
class(atl$date)
atl$date <- as.numeric(atl$date)

# date format?
# ???

# visualize the data - no values at x?
ggplot(atl, aes(x=date, y=month)) +
  geom_point() + 
  scale_x_discrete(breaks = c(2010.01, 2011.01, 2012.01, 2013.01, 2014.01, 2015.01, 2016.01, 2017.01, 2018.01, 2019.01))

######### city related END ########



### start over - just look at this by state

#data
# set working directory
setwd("~/Documents/DailyInterchange/data_sets")

days_zillow <- read.csv("DaysOnZillow_State.csv", stringsAsFactors = F)
head(days_zillow)

# clean data and remove columns - minimize date range to 2016-2019
days_zillow_clean <- days_zillow %>%
  select(RegionName:X2019.01) %>%
  select(-RegionType) %>%
  select(-starts_with("X2010"), -starts_with("X2011"), -starts_with("X2012"), -starts_with("X2013"), -starts_with("X2014"), -starts_with("X2015"))


# repeat for a few other states - 
states3 <- days_zillow_clean %>%
  filter(RegionName == "Florida" | RegionName == "Tennessee" | RegionName == "Oregon") 

states3 <- gather(states3, date, days, -RegionName)

states3 <- states3 %>%
  mutate(date = str_replace(date, "X", ""))

# change days character to number 
class(states3$days)

states3$days <- as.numeric(states3y$days)

ggplot(states3, aes(x=date, y = days, color = RegionName)) +
  geom_point() +
  scale_x_discrete(breaks = c(2016.01, 2017.01, 2018.01, 2019.01)) +
  theme(panel.grid.minor = element_blank())


# min, max and median for three states  
state_avg_day <- states3 %>%
group_by(RegionName) %>% 
  summarize(mean(days))

state_avg_max <- states3 %>%
  group_by(RegionName) %>% 
  summarize(max(days))

state_avg_min <- states3 %>%
  group_by(RegionName) %>% 
  summarize(min(days))

# join the summary stats
avg_states3 <- left_join(state_avg_day, state_avg_max)
avg_states3 <- left_join(avg_states3, state_avg_min)

# rename columns
colnames(avg_states3)[colnames(avg_states3) =="mean(days)"] <- "mean"
colnames(avg_states3)[colnames(avg_states3) =="max(days)"] <- "max"
colnames(avg_states3)[colnames(avg_states3) =="min(days)"] <- "min"

avg_states3 %>% 
  ggplot(aes(x=RegionName, y=mean)) +
  geom_bar()

# plot mean
avg_states3 %>% 
  mutate(highlight_flag = ifelse(RegionName == "Oregon", T, F)) %>% 
  ggplot(aes(RegionName, y=mean)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  coord_flip() +
  labs(x = "") +
  scale_fill_manual(values = c('grey', 'light blue')) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none")



# plot max
avg_states3 %>% 
  mutate(highlight_flag = ifelse(RegionName == "Oregon", T, F)) %>% 
  ggplot(aes(RegionName, y=max)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  coord_flip() +
  labs(x = "") +
  scale_fill_manual(values = c('grey', 'light blue')) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  


# plot min
avg_states3 %>% 
  mutate(highlight_flag = ifelse(RegionName == "Oregon", T, F)) %>% 
  ggplot(aes(RegionName, y=min)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  coord_flip() +
  labs(x = "") +
  scale_fill_manual(values = c('grey', 'light blue')) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none")



# best month to sell
oregon <- states3 %>% 
  filter(RegionName == "Oregon") %>% 
  arrange(days) %>% 
  separate(date, c('year', 'month'))

# checkout new df - year and month are characters - need to change to numbers
glimpse(oregon)
oregon$year <- as.numeric(oregon$year)
oregon$month <- as.numeric(oregon$month)

# group by month and get average for each month
oregon %>% 
  group_by(month) %>% 
  summarize(mean(days)) %>% 
  ggplot(aes(x=month, y=`mean(days)`)) +
  geom_bar(stat = 'identity') +
  labs(y='days') +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))

# same work for Tennessee
tennessee <- states3 %>% 
  filter(RegionName == "Tennessee") %>% 
  arrange(days) %>% 
  separate(date, c('year', 'month'))

# checkout new df - year and month are characters - need to change to numbers
glimpse(tennessee)
tennessee$year <- as.numeric(tennessee$year)
tennessee$month <- as.numeric(tennessee$month)


# group by month and get average for each month
tennessee %>% 
  group_by(month) %>% 
  summarize(mean(days)) %>% 
  ggplot(aes(x=month, y=`mean(days)`)) +
  geom_bar(stat = 'identity') +
  labs(y='days') +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))

# same work for Florida
florida <- states3 %>% 
  filter(RegionName == "Florida") %>% 
  arrange(days) %>% 
  separate(date, c('year', 'month'))

# checkout new df - year and month are characters - need to change to numbers
glimpse(florida)
florida$year <- as.numeric(florida$year)
florida$month <- as.numeric(florida$month)


# group by month and get average for each month
florida %>% 
  group_by(month) %>% 
  summarize(mean(days)) %>% 
  ggplot(aes(x=month, y=`mean(days)`)) +
  geom_bar(stat = 'identity') +
  labs(y='days') +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))
