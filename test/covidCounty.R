
library(ggplot2)

# setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")

ctycovid_df <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# head(ctycovid_df)

ctycovid_df$date <- as.Date(ctycovid_df$date)

neCtys_df <- ctycovid_df[ which( ctycovid_df$state == "Nebraska"), ]
lncstr_df <- neCtys_df[ which( neCtys_df$county == "Lancaster"), ]
iaCtys_df <- ctycovid_df[ which( ctycovid_df$state == "Iowa"), ]
blkhwk_df <- iaCtys_df[ which( iaCtys_df$county == "Black Hawk"), ]

# plot number of cases over time
ggplot(data=lncstr_df, aes(x=date, group=1)) +
  geom_line(aes(y = cases, colour = "Lancaster")) +
  geom_line(data=blkhwk_df, aes(y = cases,colour="Black Hawk")) +
  scale_colour_manual("",
                      breaks = c("Lancaster", "Black Hawk"),
                      values = c("darkred", "darkgreen")) +
  xlab(" ") + ylab("Cases")

## Scale the y axis using `scale_y_log10()`
ggplot(data=lncstr_df, aes(x=date, group=1)) +
  geom_line(aes(y = cases, colour = "Lancaster")) +
  geom_line(data=blkhwk_df, aes(y = cases,colour="Black Hawk")) +
  scale_colour_manual("",
                      breaks = c("Lancaster", "Black Hawk"),
                      values = c("darkred", "darkgreen")) +
  xlab(" ") + ylab("Cases") + scale_y_log10()
