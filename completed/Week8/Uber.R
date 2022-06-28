# k-means clustering tutorial
# https://www.datacamp.com/community/tutorials/k-means-clustering-r

# install.packages("VIM")
# install.packages("ggmap")
# install.packages("DT")
# bind data files with 'dplyr'
library(dplyr)
# VIM library for using 'aggr'
library(VIM)
# lubridate library to separate date & time
library(lubridate)
# plots map data
library(ggmap)
# Google Maps API key: AIzaSyCcAW7rqFOq4WOsCTfPnVvRGYFHTU0H9Xg
register_google(key = "AIzaSyCcAW7rqFOq4WOsCTfPnVvRGYFHTU0H9Xg")
# data tables
library(DT)

setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520")

# apr14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv")
# may14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv")
# jun14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv")
# jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
# aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
# sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")

apr14 <- read.csv("completed/Week8/uber-raw-data-apr14.csv")
may14 <- read.csv("completed/Week8/uber-raw-data-may14.csv")
jun14 <- read.csv("completed/Week8/uber-raw-data-jun14.csv")
jul14 <- read.csv("completed/Week8/uber-raw-data-jul14.csv")
aug14 <- read.csv("completed/Week8/uber-raw-data-aug14.csv")
sep14 <- read.csv("completed/Week8/uber-raw-data-sep14.csv")

data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)
summary(data14)

# 'aggr' plots the amount of missing/imputed values in each column (VIM)
# aggr(data14)

# Separate or mutate the Date/Time columns (lubridate)
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

head(data14, n=10)


# implement the algorithm and see the results!
set.seed(20)
clusters <- kmeans(data14[,2:3], 5)
# Save the cluster number in the dataset as column 'Borough'
data14$Borough <- as.factor(clusters$cluster)
# Inspect 'clusters'
str(clusters)

# Let's plot some graphs to visualize the data as well as the results of the k-means clustering well. (ggmap)
NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")


# use the borough information to check out Uber's growth within the boroughs for each month (DT)
data14$Month <- as.double(data14$Month)
# month_borough_14 <- count(data14, vars = c('Month', 'Borough'), sort = TRUE) %>% arrange(Month, Borough)
month_borough_14 <- data14[order(data14[,6], data14[,12]),]

month_borough_14 <- data14 %>% count(Month, Borough, sort = TRUE)
datatable(month_borough_14)

# Let's get a graphical view of the same... (dplyr)
monthly_growth <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth



# since the "growth" section didn't work, trying a smaller data set:
ind <- sample(2, nrow(data14), replace=TRUE, prob=c(0.1, 0.9))
# create subset
data14.subset <- data14[ind==1, 1:12]

data14.subset$Month <- as.double(data14.subset$Month)
month_borough_14.s <- data14.subset[order(data14.subset[,6], data14.subset[,12]),]
# head(datatable(month_borough_14.s))



month_borough_14.s <- data14.subset %>% count(Month, Borough, sort = TRUE)
monthly_growth.s <- month_borough_14.s %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth.s
