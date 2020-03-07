### devtools::install_github("tidyverse/readxl")
library(tidyverse)

# HubeiEng: hard code dataset with English city names and column names
HubeiEng_path = "/Users/apple/Desktop/XuenCov/Coronavirus-Disease-2019/HubeiEng.xlsx"
HubeiEng = readxl::read_excel(HubeiEng_path, col_names = TRUE)

### [Mar.4th] Strategy: encapsulate in a function to realize the following function: input a city name, returns the desired subset and the plot.


#######################
###  Wuhan example

# data set [Wuhan]: simple filter

Wuhan <- 
  HubeiEng %>%
  filter(City == "Wuhan")


######################################
### confirmed infected number vs. time


######################################
### create the new column "Day" as the x-axis variable

# original raw time not presentable.

library(lubridate)
Wuhan_Day <- 
  Wuhan %>%
  mutate(yearDay = yday(Time), Day = strftime(Time, "%m/%d"))  # yday: year day


######################## 
# scatterplot 1: 

# Confirmed vs. Day
ggplot(data = Wuhan_Day) +
  geom_point(mapping = aes(x = Day, y = Confirmed))


# Death vs. Day
ggplot(data = Wuhan_Day) +
  geom_point(mapping = aes(x = Day, y = Death))


##################################################
### Battle #2: I only interest in the last measurement of each day

# https://www.stat.cmu.edu/~ryantibs/statcomp/lectures/tidyverse2.html

### Strategy: long --> Wide: Each Day as columns



### Long -> Wide in a sense
# not ideal in extracting row numbers, 
# but a great dataset in terms of human eye-examination1
Wuhan_Day_Confirmed_Wide <- Wuhan_Day %>% 
  pivot_wider(names_from = yearDay,
              values_from = "Confirmed")


Wuhan_Day_Death_Wide <- Wuhan_Day %>% 
  pivot_wider(names_from = yearDay,
              values_from = "Death")



######################
### Wuhan_Day_Xue: filter the last measurement of each day

dayRange = 25:44  # further generalize, easy
j = 1
lastMeasureIndex = c()

for (i in dayRange) {
  lastMeasureIndex[j] = max(which(Wuhan_Day$yearDay == i)) + 1  # day descent --> asscent
  j = j + 1
}

lastMeasureIndex <- c(lastMeasureIndex, 1)  # add the latest measurement (the first row)

Wuhan_Day_Xue <- Wuhan_Day[lastMeasureIndex,]


#############

ggplot(data = Wuhan_Day_Xue) +
  geom_point(mapping = aes(x = Day, y = Confirmed))


# Death vs. Day
ggplot(data = Wuhan_Day_Xue) +
  geom_point(mapping = aes(x = Day, y = Death))


##############################################3
library(highcharter)

series = list(
  list(
    name = 'Death Number',
    color = '#1E90FF', # <http://cloford.com/resources/colours/500col.htm>
    data = Wuhan_Day_Xue %>% select(Death) %>% .[[1]]
  )
)

highchart() %>%
  hc_add_series_list(series) %>% 
  hc_xAxis(categories = Wuhan_Day_Xue %>% select(Day) %>% .[[1]] )%>%
  hc_yAxis(plotLines = list(
    list(value = 0, width = 5, color = 'black')
  ))



series = list(
  list(
    name = 'Death Number',
    color = '#1E90FF', # <http://cloford.com/resources/colours/500col.htm>
    data = Wuhan_Day_Xue %>% select(Confirmed) %>% .[[1]]
  )
)

highchart() %>%
  hc_add_series_list(series) %>% 
  hc_xAxis(categories = Wuhan_Day_Xue %>% select(Day) %>% .[[1]] )%>%
  hc_yAxis(plotLines = list(
    list(value = 0, width = 5, color = 'black')
  ))

