setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")

library(readr)




#### Importing Police Data

### Arrests and Citations
arrests_citations <- read_csv("completed/FinalProject/data/LPD_2013_2020_Arrests_and_Citations_De_Coded.csv")
a_c.13_20 <- arrests_citations[c("VDATE", "VTIM", "CHARGED")]
names(a_c.13_20)[1:3] <- c("Date", "Time", "Charge")
# Parse dates & times
a_c.13_20$Date <- parse_date(a_c.13_20$Date, "%Y/%m/%d %H:%M:%S+00")
a_c.13_20$Time <- str_pad(a_c.13_20$Time, 4, pad = "0")
a_c.13_20$Time <- parse_time(a_c.13_20$Time, "%H%M")
# Remove all but Sep, Oct, Nov
a_c.13_20 <- a_c.13_20[month(a_c.13_20$Date) >= 9 & month(a_c.13_20$Date) <= 11, ]
# Remove dates before 2013 or after 2019
a_c.13_19 <- a_c.13_20[year(a_c.13_20$Date) >= 2013 & year(a_c.13_20$Date) <= 2019, ]
# Remove NAs
a_c.13_19 <- na.omit(a_c.13_19)
# Day of week column
a_c.13_19$Day <- wday(a_c.13_19$Date, label = TRUE)

 head(a_c.13_19)


### Incidents
## 2017-2020 data set
incidents_2017_2020 <- read_csv("completed/FinalProject/data/LPD_2017_2020_Incident_Reports.csv")
# Remove / Rename columns
inc.17_20 <- incidents_2017_2020[c("CALL_TYPE", "From_Date", "From_Time")]
names(inc.17_20)[1:3] <- c("Type", "Date", "Time")
# Parse dates
inc.17_20$Date <- ymd(inc.17_20$Date)
## 2015 & 2016 data sets
incidents_2016 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2016.csv")
incidents_2015 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2015.csv")
inc.15_16 <- rbind(incidents_2015, incidents_2016)
# Remove / Rename columns
inc.15_16 <- inc.15_16[c("CALL_TYPE", "DATE_FROM", "TIME_FROM")]
names(inc.15_16)[1:3] <- c("Type", "Date", "Time")
# Parse dates
inc.15_16$Date <- parse_date(inc.15_16$Date, "%Y/%m/%d %H:%M:%S+00")
## 2013 & 2014 data sets
incidents_2014 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2014.csv")
incidents_2013 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2013.csv")
inc.13_14 <- rbind(incidents_2013, incidents_2014)
# Remove / Rename columns
inc.13_14 <- inc.13_14[c("CALL_TYPE", "DATE_FROM", "TIME_FROM")]
names(inc.13_14)[1:3] <- c("Type", "Date", "Time")
# Parse dates
inc.13_14$Date <- mdy(inc.13_14$Date)
## Combine data sets (2013-2020)
inc.13_20 <- rbind(inc.13_14, inc.15_16, inc.17_20)
# Parse times
inc.13_20$Time <- str_pad(inc.13_20$Time, 4, pad = "0")
inc.13_20$Time <- parse_time(inc.13_20$Time, "%H%M")
# Remove all but Sep, Oct, Nov
inc.13_20 <- inc.13_20[month(inc.13_20$Date) >= 9 & month(inc.13_20$Date) <= 11, ]
# Remove dates before 2013 or after 2019
inc.13_19 <- inc.13_20[year(inc.13_20$Date) >= 2013 & year(inc.13_20$Date) <= 2019, ]
# Remove NAs
inc.13_19 <- na.omit(inc.13_19)
# Day of week column
inc.13_19$Day <- wday(inc.13_19$Date, label = TRUE)

 head(inc.13_19)
 str(inc.13_19)
 summary(inc.13_19)

### Cleanup
rm(arrests_citations, a_c.13_20)
rm(incidents_2017_2020, incidents_2016, incidents_2015, incidents_2014, incidents_2013)
rm(inc.17_20, inc.15_16, inc.13_14, inc.13_20)



### Husker Time
test.data <- Husker_games
Husker_games <- test.data
Husker_games$Time <- parse_time(Husker_games$Time, "%H:%M:%S") # cant because not <chr>
Husker_games$Time <- hms(Husker_games$Time)
# Husker_games$DateTime <- ymd_hms(Husker_games$Date + Husker_games$Time)
head(Husker_games)
summary(Husker_games)
str(Husker_games)
# Husker_games$Time <- as.numeric(Husker_games$Time) # changed 20:00:00 to 72000
Husker_games$Time <- as.character(Husker_games$Time)
Husker_games$Time <- parse_time(Husker_games$Time, "%H:%M:%S")
rm(test.data)



### TRAFFIC CRASHES
{
Trf_Crash_13 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2013.csv")
Trf_Crash_14 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2014.csv")
Trf_Crash_15 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2015.csv")
Trf_Crash_16 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2016.csv")
Trf_Crash_17 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2017.csv")
Trf_Crash_18 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2018.csv")
Trf_Crash_19 <- read_csv("completed/FinalProject/data/Traffic_Crashes_2019.csv")
}

names(Trf_Crash_18)[16] <- "FID"
names(Trf_Crash_19)[16] <- "FID"
t_c.13_18 <- rbind(Trf_Crash_13, Trf_Crash_14, Trf_Crash_15, Trf_Crash_16, Trf_Crash_17, Trf_Crash_18)

t_c.13_18$DOA <- parse_date(t_c.13_18$DOA, "%Y/%m/%d %H:%M:%S+00")

Trf_Crash_19$DOA <- as.POSIXct(Trf_Crash_19$DOA/1000, origin = "1970-01-01")
Trf_Crash_19$DOA <- as.character(Trf_Crash_19$DOA)
Trf_Crash_19$DOA <- parse_date(Trf_Crash_19$DOA, "%Y-%m-%d %H:%M:%S")

t_c.13_19 <- rbind(t_c.13_18, Trf_Crash_19)

t_c.13_19 <- t_c.13_19[c("TYPE", "ACTION", "PED", "BIKE", "MC", "MOPED", "TRAIN", "TRUCK", "BUS", "DOA", "TOA")]
names(t_c.13_19)[1:11] <- c("Type", "Action", "Pedestrian", "Bike", "Motorcycle", "Moped", "Train", "Truck", "Bus", "Date", "Time")

t_c.13_19 <- t_c.13_19[month(t_c.13_19$Date) >= 9 & month(t_c.13_19$Date) <= 11, ]
t_c.13_19 <- t_c.13_19[year(t_c.13_19$Date) >= 2013 & year(t_c.13_19$Date) <= 2019, ]

t_c.13_19$Time <- str_pad(t_c.13_19$Time, 4, pad = "0")
t_c.13_19$Time <- parse_time(t_c.13_19$Time, "%H%M")

t_c.13_19 <- na.omit(t_c.13_19)
t_c.13_19$Day <- wday(t_c.13_19$Date, label = TRUE)


head(t_c.13_19)

head(Trf_Crash_13)
head(Trf_Crash_14)
head(Trf_Crash_15)
head(Trf_Crash_16)
head(Trf_Crash_17)
head(Trf_Crash_18)
head(Trf_Crash_19)

rm(Trf_Crash_13, Trf_Crash_14, Trf_Crash_15, Trf_Crash_16, Trf_Crash_17, Trf_Crash_18, Trf_Crash_19)
rm(t_c.13_18)



### TRAFFIC STOPS
{
Trf_Stop_13 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2013.csv")
Trf_Stop_14 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2014.csv")
Trf_Stop_15 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2015.csv")
Trf_Stop_16 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2016.csv")
Trf_Stop_17 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2017.csv")
Trf_Stop_18 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2018.csv")
Trf_Stop_19 <- read_csv("completed/FinalProject/data/LPD_traffic_stops_2019.csv")
}

names(Trf_Stop_14)[4] <- "SEX"
names(Trf_Stop_18)[8] <- "FID"
t_s.13.16 <- rbind(Trf_Stop_13, Trf_Stop_16)
t_s.14_15 <- rbind(Trf_Stop_14, Trf_Stop_15)
t_s.17_19 <- rbind(Trf_Stop_17, Trf_Stop_18, Trf_Stop_19)

t_s.13.16$TIME <- parse_time(t_s.13.16$TIME)
t_s.14_15$TIME <- parse_time(t_s.14_15$TIME, "%Y/%M/%D %H:%M:%S+00")
t_s.17_19$TIME <- gsub(":XX", "", t_s.17_19$TIME)
t_s.17_19$TIME <- parse_time(t_s.17_19$TIME, "%H:%M")

t_s.13_19 <- rbind(t_s.13.16, t_s.14_15, t_s.17_19)

t_s.13_19 <- t_s.13_19[c("REASON", "DATE", "TIME")]
names(t_s.13_19)[1:3] <- c("Reason", "Date", "Time")
t_s.13_19$Date <- parse_date(t_s.13_19$Date, "%Y/%m/%d %H:%M:%S+00")

t_s.13_19 <- t_s.13_19[month(t_s.13_19$Date) >= 9 & month(t_s.13_19$Date) <= 11, ]
t_s.13_19 <- t_s.13_19[year(t_s.13_19$Date) >= 2013 & year(t_s.13_19$Date) <= 2019, ]

t_s.13_19$Day <- wday(t_s.13_19$Date, label = TRUE)

rm(Trf_Stop_13, Trf_Stop_14, Trf_Stop_15, Trf_Stop_16, Trf_Stop_17, Trf_Stop_18, Trf_Stop_19)
rm(t_s.13.16, t_s.14_15, t_s.17_19)


head(t_s.13_19)


test.data <- t_s.17_19
test.data$TIME <- gsub(":XX", "", test.data$TIME)
test.data$TIME <- parse_time(test.data$TIME, "%H:%M")
head(test.data)
parse_time(t_s.13.16$TIME, "%H:%M")

class(test.data$TIME)
class(t_s.13.16$TIME)

head(t_s.13.16)
head(t_s.14_15)
head(t_s.17_19)
head(a_c.13_19)

head(Trf_Stop_13)
head(Trf_Stop_14)
head(Trf_Stop_15)
head(Trf_Stop_16)
head(Trf_Stop_17)
head(Trf_Stop_18)
head(Trf_Stop_19)








