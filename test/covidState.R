
library(ggplot2)

# setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")

uscovid_df <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
# head(uscovid_df)

uscovid_df$date <- as.Date(uscovid_df$date)

iowa_df <- uscovid_df[ which( uscovid_df$state == "Iowa"), ]
nebraska_df <- uscovid_df[ which( uscovid_df$state == "Nebraska"), ]
texas_df <- uscovid_df[ which( uscovid_df$state == "Texas"), ]

# plot number of cases over time
ggplot(data=iowa_df, aes(x=date, group=1)) +
  geom_line(aes(y = cases, colour = "Iowa")) +
  geom_line(data=nebraska_df, aes(y = cases,colour="Nebraska")) +
  geom_line(data=texas_df, aes(y = cases, colour="Texas")) +
  scale_colour_manual("",
                      breaks = c("Iowa", "Nebraska", "Texas"),
                      values = c("darkred", "darkgreen", "steelblue")) +
  xlab(" ") + ylab("Cases")

## Scale the y axis using `scale_y_log10()`
ggplot(data=iowa_df, aes(x=date, group=1)) +
  geom_line(aes(y = cases, colour = "Iowa")) +
  geom_line(data=nebraska_df, aes(y = cases,colour="Nebraska")) +
  geom_line(data=texas_df, aes(y = cases, colour="Texas")) +
  scale_colour_manual("",
                      breaks = c("Iowa", "Nebraska", "Texas"),
                      values = c("darkred", "darkgreen", "steelblue")) +
  xlab(" ") + ylab("Cases") + scale_y_log10()
