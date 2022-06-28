setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")
# Read data files
library(readr)
# Plot data
library(ggplot2)
# Work with dates
library(lubridate)
# date?
library(dplyr)


## HUSKER DATA

# Load data
Huskers_2019 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2019.csv")
Huskers_2018 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2018.csv")
Huskers_2017 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2017.csv")
Huskers_2016 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2016.csv")
Huskers_2015 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2015.csv")
Huskers_2014 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2014.csv")
Huskers_2013 <- read_csv("GitHub/DSC520/completed/FinalProject/data/Huskers_2013.csv")

Husker_games <- rbind(Huskers_2013, Huskers_2014, Huskers_2015, Huskers_2016, Huskers_2017, Huskers_2018, Huskers_2019)

# Remove ranking from school names
head(Husker_games)
Husker_games$School <- gsub("[[:digit:]]", "", Husker_games$School)
Husker_games$School <- gsub("[[:punct:]]+[[:punct:]] ", "", Husker_games$School)
head(Husker_games)
Husker_games$Opponent <- gsub("[[:digit:]]", "", Husker_games$Opponent)
Husker_games$Opponent <- gsub("[[:punct:]]+[[:punct:]] ", "", Husker_games$Opponent)
head(Husker_games)

# Convert Date column to dates
# Husker_games$Date <- as.Date(Husker_games$Date)
Husker_games$Date <- mdy(Husker_games$Date)
Husker_games$Day <- wday(Husker_games$Date, label = TRUE)
Husker_games$Time <- hms(Husker_games$Time)
Husker_games$DateTime <- ymd_hms(Husker_games$Date + Husker_games$Time)  

# Plot data
ggplot(data = Husker_games) + geom_histogram(aes(Husker_games$Time))
ggplot(data = Husker_games, aes(x = Husker_games$Day)) + geom_bar()
                                                           
# Remove columns
Husker_games <- Husker_games[c("Date", "Time", "Day", "School", "X6", "Opponent", "X9")]


testgames <- Husker_games
as.numeric(as.factor(testgames$Win))-1
Husker_games$Win <- as.numeric(as.factor(testgames$Win))-1
testgames$Home <- as.numeric(as.factor(Husker_games$Home))-1
testgames$Home[is.na(testgames$Home)] <- 1


## POLICE DATA
incidents_2017_2020 <- read_csv("completed/FinalProject/data/LPD_2017_2020_Incident_Reports.csv")
head(incidents_2017_2020)
inc.17_20 <- incidents_2017_2020[c("CALL_TYPE", "From_Date", "From_Time")]
head(inc.17_20)
names(inc.17_20)[1:3] <- c("Call.Type", "Date", "Time")
head(inc.17_20)


incidents_2016 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2016.csv")
head(incidents_2016)

# Append incidents
incidents <- rbind(inc.13_16, inc.17_20)

# Fix dates
# incidents$Date <- mdy(incidents$Date)
inc.13_16$Date <- mdy(inc.13_16$Date)
head(inc.13_16)
head(incidents_2013)
head(incidents_2014)
head(incidents_2015)
head(incidents_2016)
head(inc.13_14)

# Time
head(incidents)
incidents$Date.Time <- as_datetime(incidents$Date + incidents$Time)
inc.13_14$TIME <- hms(inc.13_14$TIME)
inc.15_16$TIME <- hm(inc.15_16$TIME)
inc.17_20$Time <- hm(inc.17_20$Time)
incidents_2017_2020$From_Time <- hms(incidents_2017_2020$From_Time)

test.17_20 <- inc.17_20
test.17_20$Numeric <- is.numeric(incidents_2017_2020$From_Time)

# More time...
## Incident Reports
incidents_2017_2020 <- read_csv("completed/FinalProject/data/LPD_2017_2020_Incident_Reports.csv")
head(incidents_2017_2020)
incidents_2017_2020$From_Date <- ymd(incidents_2017_2020$From_Date)
inc.17_20 <- incidents_2017_2020[c("CALL_TYPE", "From_Date", "From_Time")]
# inc.17_20$From_Time <- as.numeric(inc.17_20$From_Time)
names(inc.17_20)[1:3] <- c("Call.Type", "Date", "Time")

incidents_2016 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2016.csv")
head(incidents_2016)
incidents_2015 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2015.csv")
inc.15_16 <- rbind(incidents_2015, incidents_2016)
inc.15_16$DATE_FROM <- ymd_hms(inc.15_16$DATE_FROM)
# inc.15_16$TIME_FROM <- as.numeric(inc.15_16$TIME_FROM)
head(inc.15_16)
head(incidents_2015)

incidents_2014 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2014.csv")
incidents_2013 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2013.csv")
inc.13_14 <- rbind(incidents_2013, incidents_2014)
inc.13_14$DATE_FROM <- mdy(inc.13_14$DATE_FROM)
# inc.13_14$TIME_FROM <- as.numeric(inc.13_14$TIME_FROM)
# head(inc.13_14)

inc.13_16 <- rbind(inc.13_14, inc.15_16)
inc.13_16 <- inc.13_16[c("CALL_TYPE", "DATE_FROM", "TIME_FROM")]
names(inc.13_16)[1:3] <- c("Call.Type", "Date", "Time")

incidents <- rbind(inc.13_16, inc.17_20)
incidents$Time <- str_pad(incidents$Time, 4, pad = "0")
incidents$Time <- as.numeric(incidents$Time)
incidents$Time <- hm(incidents$Time)
# head(inc.17_20)
# head(inc.13_16)
head(incidents)


incidents_2016 <- read_csv("completed/FinalProject/data/LPD_Incident_Reports_2016.csv")
# head(incidents_2016)
incidents_2016 <- incidents_2016[c("CALL_TYPE", "DATE_FROM", "TIME_FROM")]
# head(incidents_2016)
names(incidents_2016)[1:3] <- c("Call.Type", "Date", "Time")
# head(incidents_2016)
# remove rows w/NA
incidents_2016 <- na.omit(incidents_2016)
# dates & times
incidents_2016$Date <- ymd_hms(incidents_2016$Date)
head(incidents_2016)

incidents_2016$Time <- str_pad(incidents_2016$Time, 4, pad = "0")
incidents_2016$Time <- format(strptime(incidents_2016$Time, format='%H%M'), format = '%H:%M')
head(incidents_2016)

incidents_2016$Time <- as.numeric(incidents_2016$Time)
incidents_2016$Time <- substr(as.POSIXct(sprintf("%04.0f", incidents_2016$Time), format='%H%M'), 12, 16)
head(incidents_2016)


test.incidents <- incidents_2016
incidents_2016 <- test.incidents

hm(incidents_2016$Time)
hms(incidents_2016$Time)
test.incidents$Time <- is.na(incidents_2016$Time)

as.numeric(incidents_2016$Time)

# as_datetime(incidents_2016$Time)
as.POSIXlt(incidents_2016$Time, tryFormats = c("%H:%M"))
# as.difftime(incidents_2016$Time)
# format(strptime(incidents_2016$Time, format='%H%M'))
# format(strptime(incidents_2016$Time, format='%H%M'), format = '%H:%M')
# as.POSIXlt('incidents_2016$Time')
# ISOdatetime(hour = incidents_2016$Time)

parse_time(incidents_2017_2020$From_Time, "%H%M")  ## I THINK THIS IS IT

parse_time(incidents_2016$Time, "%H%M")
test.data <- parse_time(incidents_2016$Time, "%H%M")
# Pad first
incidents_2016$Time <- parse_time(incidents_2016$Time, "%H%M")
head(test.data)
incidents_2016$Date <- parse_date(incidents_2016$Date, "%Y/%m/%d %H:%M:%S+00")

## filter out months =/= 9, 10, 11 [NOTE: ALSO REMOVE 2020 AND PRE-2013 DATA]
head(Husker_games)
# test.data <- filter(Husker_games, month == 9)
# month(Husker_games$Date)
# filter(Husker_games$Date, month(Husker_games$Date == 9))                    
# month(Husker_games$Date) >= 9 & month(Husker_games$Date) <= 11
# Husker_games[month(Husker_games$Date) >= 9 & month(Husker_games$Date) <= 11, ]
# test.data <- Husker_games[month(Husker_games$Date) >= 9 & month(Husker_games$Date) <= 11, ]
# test.data

test.data <- inc.13_16
test.data <- test.data[month(test.data$Date) >= 9 & month(test.data$Date) <= 11, ]
test.data


# Day of week column
Husker_games$Day <- wday(Husker_games$Date, label = TRUE)

# Remove NAs
incidents_2016 <- na.omit(incidents_2016)


inc.17_20[1526:1528,]

# Cleanup
rm(test.17_20, test.data, test.incidents, testdf, testgames)
