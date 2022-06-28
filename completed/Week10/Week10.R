setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")

# creating dummy columns
install.packages("fastDummies")
library(fastDummies)
# count() instances
library(dplyr)
# correlation comparison
library(GGally)
# make a long data set
library(reshape2)
# correlation analysis
# library(boot)
# pcor() and pcor.test()
library(ggm)
# library(Hmisc)
# library(polycor)

head(Husker_games)
# { # DONE
# Husker_games$Day <- as.factor(Husker_games$Day)
# Husker_games$Location <- as.factor(Husker_games$Location)
# Husker_games$Opponent <- as.factor(Husker_games$Opponent)
# Husker_games$Win <- as.factor(Husker_games$Win)
# }
# Husker_games$Year <- year(Husker_games$Date)
# Husker_games$Week <- isoweek(Husker_games$Date)
# 
# # test.data <- Husker_games
# # # # test.data$week <- week(Husker_games$Date)
# # # test.data$Week <- isoweek(Husker_games$Date)
# # # # test.data$epiweek <- epiweek(Husker_games$Date)
# # # test.data$Year <- year(Husker_games$Date)
# # # 
# # # test.data$Yr_Wk <- paste(test.data$Year, test.data$Week, sep = "-")
# #  # in one step:
# # test.data$Yr_Wk <- paste(year(Husker_games$Date), isoweek(Husker_games$Date), sep = "-")
# 
# # get week number
# { # DONE
# Husker_games$Yr_Wk <- paste(year(Husker_games$Date), isoweek(Husker_games$Date), sep = "-")
# a_c.13_19$Yr_Wk <- paste(year(a_c.13_19$Date), isoweek(a_c.13_19$Date), sep = "-")
# inc.13_19$Yr_Wk <- paste(year(inc.13_19$Date), isoweek(inc.13_19$Date), sep = "-")
# t_c.13_19$Yr_Wk <- paste(year(t_c.13_19$Date), isoweek(t_c.13_19$Date), sep = "-")
# t_s.13_19$Yr_Wk <- paste(year(t_s.13_19$Date), isoweek(t_s.13_19$Date), sep = "-")
# }
# { # DONE
# # Count occurences
# a_c.occur <- table(unlist(a_c.13_19$Yr_Wk))
# Husker_games$A_C <- a_c.occur[Husker_games$Yr_Wk]
# Inc.occur <- table(unlist(inc.13_19$Yr_Wk))
# Husker_games$Inc <- Inc.occur[Husker_games$Yr_Wk]
# t_c.occur <- table(unlist(t_c.13_19$Yr_Wk))
# Husker_games$T_C <- t_c.occur[Husker_games$Yr_Wk]
# t_s.occur <- table(unlist(t_s.13_19$Yr_Wk))
# Husker_games$T_S <- t_s.occur[Husker_games$Yr_Wk]
# 
# rm(a_c.occur, Inc.occur, t_c.occur, t_s.occur)
# }
# head(Husker_games)
# { # DONE
# Husker_games$A_C <- as.numeric(Husker_games$A_C)
# Husker_games$Inc <- as.numeric(Husker_games$Inc)
# Husker_games$T_C <- as.numeric(Husker_games$T_C)
# Husker_games$T_S <- as.numeric(Husker_games$T_S)
# }
# # rm(test.data, data.test)
# # {
# # Husker_games$Win <- factor(Husker_games$Win)
# # Husker_games$Home <- factor(Husker_games$Home)
# # }

ggplot(Husker_games, aes(x = Date, y = A_C, color = Win)) + geom_point() + geom_smooth(method = lm)
ggplot(H_G.long, aes(x = Week, y = value, color = variable)) + geom_point() + geom_smooth(method = lm) + facet_grid(Location ~ Win)
ggplot(H_G.long, aes(x = Date, y = value, color = variable)) + geom_point() + geom_smooth(method = lm) + facet_grid(Location ~ Win)
## ^^^ Good to show decrease over time, regardless of W/L, Home/Away
# ggplot(H_G.long, aes(x = Win, y = value, color = Location)) + geom_line() + geom_smooth(method = lm) + facet_grid(~ variable)
## ^^^ better in a box plot
ggplot(Husker_games, aes(x = Date, y = A_C, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(Husker_games, aes(x = Date, y = Inc, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(Husker_games, aes(x = Date, y = T_C, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(Husker_games, aes(x = Date, y = T_S, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(H_G.long, aes(x = Date, y = value, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_grid(variable ~ Location)
ggplot(H_G.long, aes(x = Date, y = value, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_grid(Location ~ variable)
#
# ggplot(Husker_games, aes(x = Date, y = A_C, color = Location)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Win)
# ggplot(Husker_games, aes(x = Date, y = Inc, color = Location)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Win)
# ggplot(Husker_games, aes(x = Date, y = T_C, color = Location)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Win)
# ggplot(Husker_games, aes(x = Date, y = T_S, color = Location)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Win)

# { # DONE
# # Get total number of incidents
# Husker_games$Tot_Inc <- rowSums(Husker_games[c("A_C", "Inc", "T_C", "T_S")])
# }

ggplot(Husker_games, aes(x = Date, y = Tot_Inc, color = Win)) + geom_point() + geom_smooth(method = lm)
ggplot(Husker_games, aes(x = Date, y = Tot_Inc, color = Location)) + geom_point() + geom_smooth(method = lm)

# NOTE: NEED GGPAIRS from GGally ... maybe this isn't so useful
ggpairs(Husker_games, columns = c("Year", "Day", "Location", "Opponent", "Win", "A_C", "Inc", "T_C", "T_S"), cardinality_threshold = 28)
# ^^ Lots of information, but not very useful

ggplot(Husker_games, aes(Win, Tot_Inc, color = Win)) + geom_boxplot() + facet_wrap(~ Location)
ggplot(H_G.long, aes(variable, value, color = Win)) + geom_boxplot()
## ^^ Win
ggplot(Husker_games, aes(Location, Tot_Inc, color = Win)) + geom_boxplot()
ggplot(H_G.long, aes(variable, value, color = Location)) + geom_boxplot()
## ^^ Location
ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + geom_boxplot(data = subset(Husker_games, Location=="Home")) + theme(axis.text.x = element_text(angle = 90))
ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + geom_boxplot(data = subset(Husker_games, Location=="Away")) + theme(axis.text.x = element_text(angle = 90))

ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + geom_boxplot(data = subset(Husker_games, Location=="Home")) + geom_point(data = subset(Husker_games, Location=="Home")) + theme(axis.text.x = element_text(angle = 90))

# ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + stat_summary(fun = mean, geom = "line", lwd = 1, aes(group = 1, y = mean(Tot_Inc)))  + geom_boxplot() + geom_point() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~ Location)
# ^^ mean line same for both :|
ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + geom_boxplot() + geom_point(aes(shape = Win)) + theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size = 8)) + facet_wrap(~ Location) #+ theme(legend.position = "bottom")
## ^^^ home and away by Opponent <-- Do I just need Home info? [also: need to relocate legend]
ggplot(Husker_games, aes(Opponent, Tot_Inc, color = Opponent)) + geom_boxplot() + geom_point() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~ Win)
## ^^^ Win and Lose by Opponent
ggplot(Husker_games, aes(x = Location, color = Win)) + geom_boxplot(aes(y = A_C))
ggplot(H_G.long, aes(x = Location, color = Win)) + geom_boxplot(aes(y = value)) + facet_grid(~ variable)
## ^^^


ggplot(Husker_games, aes(Time, Tot_Inc, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
## ^^^ This could be interesting (see also individual A_C below vvv)
# ggplot(Husker_games, aes(Time, A_C)) + geom_bar(stat = "identity")
ggplot(Husker_games, aes(Time, A_C, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(Husker_games, aes(Time, T_S, color = Win)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ Location)
ggplot(H_G.long, aes(Time, value, color = Win)) + geom_point(position = "jitter") + geom_smooth(method = lm) + facet_grid(variable ~ Location)
ggplot(H_G.long, aes(Time, value, color = Win)) + geom_point(position = "jitter") + geom_smooth(method = lm) + facet_grid(Location ~ variable)

# ggplot(Husker_games, aes(Yr_Wk, Tot_Inc)) + geom_col(aes(fill = A_C))

ggplot(Husker_games, aes(A_C, Tot_Inc)) + geom_point()
# ggplot(Husker_games, aes(Location, Tot_Inc, Win)) + geom_tile()
ggplot(Husker_games, aes(Location, Win)) + geom_jitter()
ggplot(Husker_games, aes(Location, Win)) + geom_jitter(aes(size = Tot_Inc, color = Opponent, shape = Day))
ggplot(Husker_games, aes(Location, A_C)) + geom_bar(stat = "identity") # shows total A_C home vs away
ggplot(H_G.long, aes(variable, value, fill = variable)) + geom_bar(stat = "identity") + facet_grid(Location ~ Win)
## ^^^ bar chart comparing home vs away and W vs L for each Police variable
head(Husker_games)

# 
# ## Fixing Arrests / Citations
# rm(arrests_citations)
# head(arr_cit_13)
# head(arr_cit_14)
# head(arr_cit_15)
# head(arr_cit_16)
# head(arr_cit_17)
# head(arr_cit_18)
# head(arr_cit_19)
# 
# # Remove / rename columns and merge
# names(arr_cit_18)[15] <- "FID"
# a_c.13_18 <- rbind(arr_cit_13, arr_cit_14, arr_cit_15, arr_cit_16, arr_cit_17, arr_cit_18)
# a_c.13_18 <- a_c.13_18[c("CHARGED", "VDAT", "VTIM")]
# arr_cit_19 <- arr_cit_19[c("CHARGED", "VDAT", "VTIM")]
# a_c.13_19 <- rbind(a_c.13_18, arr_cit_19)
# names(a_c.13_19)[1:3] <- c("Charge","Date", "Time")
# # Parse dates & times
# a_c.13_19$Date <- parse_date(a_c.13_19$Date, "%Y/%m/%d %H:%M:%S+00")
# a_c.13_19$Time <- str_pad(a_c.13_19$Time, 4, pad = "0")
# a_c.13_19$Time <- parse_time(a_c.13_19$Time, "%H%M")
# # Remove all but Sep, Oct, Nov
# a_c.13_19 <- a_c.13_19[month(a_c.13_19$Date) >= 9 & month(a_c.13_19$Date) <= 11, ]
# # Remove dates before 2013 or after 2019
# a_c.13_19 <- a_c.13_19[year(a_c.13_19$Date) >= 2013 & year(a_c.13_19$Date) <= 2019, ]
# # Remove NAs
# a_c.13_19 <- na.omit(a_c.13_19)
# # Day of week column
# a_c.13_19$Day <- wday(a_c.13_19$Date, label = TRUE)


## MAKE LONG FORMAT DATA SET: stack() unstack()
# H_G.long <- stack(Husker_games, select = c("A_C", "Inc", "T_C", "T_S"))
# H_G.undo <- unstack(H_G.long, values ~ ind)
# Well that didn't really work. Let's try  using package "reshape"
## MAKE LONG FORMAT DATA SET: melt() cast()
H_G.long.2 <- melt(Husker_games, id = c("Date", "Year", "Week", "Day", "Time", "School", "Location", "Opponent", "Win", "Yr_Wk"), measured = c("A_C", "Inc", "T_C", "T_S"))
head(H_G.long.2)
tail(H_G.long.2)
H_G.long <- melt(Husker_games, id = c("Date", "Year", "Week", "Day", "Time", "School", "Location", "Opponent", "Win", "Yr_Wk", "Tot_Inc"), measured = c("A_C", "Inc", "T_C", "T_S"))
head(H_G.long)
class(H_G.long.2)

rm(H_G.long)
rm(H_G.long.2)

## HISTOGRAMS
ggplot(H_G.long, aes(value, fill = Win)) + geom_histogram() + facet_grid(Win ~ variable)
ggplot(H_G.long, aes(value, fill = variable)) + geom_histogram(position = "dodge", binwidth = 200) + facet_grid(Win ~ Location)
ggplot(H_G.long, aes(value, fill = Win)) + geom_histogram(position = "dodge") + facet_wrap(~ variable)
# ^^
ggplot(H_G.long, aes(value, fill = Location)) + geom_histogram(position = "dodge") + facet_wrap(~ variable)
ggplot(H_G.long, aes(value, fill = Win)) + geom_histogram(position = "dodge") + facet_grid(Location ~ variable)
# ^^
ggplot (Husker_games, aes(A_C)) + geom_histogram() + facet_wrap(~ Win)
ggplot (H_G.long, aes(value, fill = variable)) + geom_histogram(position = "dodge") + facet_wrap(~ Win)
ggplot (H_G.long, aes(value, fill = variable)) + geom_histogram(position = "dodge") + facet_grid(Location ~ Win)
# ^^
ggplot(H_G.coded, aes(Win)) + geom_histogram(position = "dodge") + facet_wrap(~ Location)
ggplot(Husker_games, aes(Win)) + geom_histogram(position = "dodge", stat = "count") + facet_wrap(~ Location)

## Husker wins vs losses, faceted by location
ggplot(Husker_games, aes(Win, fill = Win)) + geom_histogram(position = "dodge", stat = "count") + facet_wrap(~ Location)
ggplot(Husker_games, aes(Win, fill = Win)) + geom_histogram(position = "dodge", stat = "count") + facet_grid(Location ~ Opponent)
ggplot(Husker_games, aes(Win, fill = Win)) + geom_histogram(position = "dodge", stat = "count") + facet_wrap(~ Opponent)
ggplot(Husker_games, aes(Location, fill = Win)) + geom_histogram(position = "dodge", stat = "count") + facet_wrap(~ Opponent)
## ^^^ just for fun (no crime data, just W/L by Home/Away by Opponent)

ggplot(Husker_games, aes(A_C)) + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(Husker_games$A_C), 
                                         sd(Husker_games$A_C)))
ggplot(Husker_games, aes(Inc)) + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(Husker_games$Inc), 
                                         sd(Husker_games$Inc)))
ggplot(Husker_games, aes(T_C)) + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(Husker_games$T_C), 
                                         sd(Husker_games$T_C)))
ggplot(Husker_games, aes(T_S)) + geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(Husker_games$T_S), 
                                         sd(Husker_games$T_S)))
## ^^^ histograms with normal curve for each


## BOXPLOTS
ggplot(H_G.long, aes(variable, value)) + geom_boxplot(aes(color = Win))
ggplot(H_G.long, aes(variable, value)) + geom_boxplot(aes(color = Location))
ggplot(H_G.long, aes(variable, value)) + geom_boxplot(aes(color = variable)) + facet_grid(Win ~ Location)
# ^ nope, the others are better

## BARCHARTS
ggplot(H_G.long, aes(variable, value, fill = Location)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2)
# ^^ not too bad
ggplot(H_G.long, aes(variable, value, fill = Win)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2)

ggplot(H_G.long, aes(variable, value, fill = Location)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) +
  facet_wrap(~ Win)
# ^^ not too bad
ggplot(H_G.long, aes(Win, value, fill = Location)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) +
  facet_wrap(~ variable)
## ^^^ this one is pretty nice
{
ggplot(H_G.long, aes(Win, value, fill = Location)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  facet_grid(variable ~ Opponent)
ggplot(H_G.long, aes(Location["Home"], value, fill = Win)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  facet_grid(variable ~ Opponent)
} # ^^ by Opponent - this is pretty busy

## LINE PLOTS
ggplot(Husker_games, aes(Location, A_C)) + 
         stat_summary(fun = mean, geom = "point") +
         stat_summary(fun = mean, geom = "line", aes(group = 1))
# ^ nope
ggplot(H_G.long, aes(Location, value, color = variable)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", 
               aes(group = variable))   

ggplot(H_G.long, aes(Location, value, color = variable)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", 
               aes(group = variable)) +
  facet_wrap(~ Win)
## ^^ this one is pretty good to compare all against home vs away, but doesn't really tell anything new

## SCATTERPLOTS - all these have better options above
ggplot(H_G.long, aes(variable, value, color = Location)) + geom_point(position = "jitter")
# different PSIs over time (by year)
ggplot(H_G.long, aes(Year, value, color = variable)) + geom_point() + geom_smooth(method = lm)
# different PSIs over time (by week)
ggplot(H_G.long, aes(Week, value, color = variable)) + geom_point() + geom_smooth(method = lm)


## EXPLORING ASSUMPTIONS
shapiro.test(Husker_games$A_C) # 0.8052
shapiro.test(Husker_games$Inc) # 0.001708 <- not normal
shapiro.test(Husker_games$T_C) # 0.005984 <- not normal
shapiro.test(Husker_games$T_S) # 0.8249
by(Husker_games$A_C, Husker_games$Location, shapiro.test)
by(H_G.long$value, H_G.long$Location, shapiro.test)
# Check for normalcy w/ qq plot
{
qplot(sample = Husker_games$A_C)
qplot(sample = Husker_games$Inc)
qplot(sample = Husker_games$T_C)
qplot(sample = Husker_games$T_S)
} # Q-Q plots for each

## CORRELATION
# Husker_games[c("Day", "Location")] # test
# cor(Husker_games$A_C, Husker_games$Inc)
# LOL oops, see below
cor.test(H_G.coded$A_C, H_G.coded$Location)
cor.test(H_G.coded$Inc, H_G.coded$Location)
cor.test(H_G.coded$T_C, H_G.coded$Location)
cor.test(H_G.coded$T_S, H_G.coded$Location)

cor.test(H_G.coded$A_C, H_G.coded$Win)
cor.test(H_G.coded$Inc, H_G.coded$Win)
cor.test(H_G.coded$T_C, H_G.coded$Win)
cor.test(H_G.coded$T_S, H_G.coded$Win)

# 1s/0s for Home/Away & Win/Lose
# Convert W/L and NA/@ to 1/0

H_G.coded <- Husker_games
H_G.coded$Location <- as.numeric(as.factor(H_G.coded$Location))-1
H_G.coded$Win <- as.numeric(as.factor(H_G.coded$Win))-1
# H_G.coded <- dummy_cols(H_G.coded, select_columns = "Opponent")
H_G.coded$Opponent <- as.factor(H_G.coded$Opponent)
 # DONE (?)
head(H_G.coded)

# point-biserial correlations: 

cor.test(H_G.coded$A_C, H_G.coded$Location) # p 0.03558
# cor.test(H_G.coded$A_C, H_G.coded$Location)$estimate^2 * 100
cor.test(H_G.coded$Inc, H_G.coded$Location) # p 0.5723
cor.test(H_G.coded$T_C, H_G.coded$Location) # p 0.5048
cor.test(H_G.coded$T_S, H_G.coded$Location) # p 0.4838
cor.test(H_G.coded$Tot_Inc, H_G.coded$Location) # p 0.09613
cor.test(H_G.coded$A_C, H_G.coded$Win) # p 0.1085
cor.test(H_G.coded$Inc, H_G.coded$Win) # p 0.3060
cor.test(H_G.coded$T_C, H_G.coded$Win) # p 0.4557
cor.test(H_G.coded$T_S, H_G.coded$Win) # p 0.2604
cor.test(H_G.coded$Tot_Inc, H_G.coded$Win) # p 0.1227
 # see below vvv
# Point-biserial correlation coefficient
cor(H_G.coded[c("Location", "Win", "A_C", "Inc", "T_C", "T_S", "Tot_Inc")])
cor(H_G.coded[c("Location", "Win", "A_C", "Inc", "T_C", "T_S", "Tot_Inc")])^2 * 100
## ^^^ p222 this tells how much of the variability is shared between variables
# "the variance in y accounted for by x" (percent)

# #Partial correlation ## I guess this doesn't work :(
# pc <- pcor(c("A_C", "Inc", "T_C", "T_S", "Tot_Inc", "Location", "Win"), var(H_G.coded))

# LINEAR REGRESSION
# Multiple
loc.lm <- lm(H_G.coded$Location, ~ A_C + Inc + T_C + T_S, data = H_G.coded)


H_G.lm <- lm(A_C ~ Opponent, data = H_G.coded)
summary(H_G.lm)
# ^^^ not sure how to explain, but probably should include (maybe a make subset of home games?)
round(tapply(H_G.coded$A_C, H_G.coded$Opponent, mean, na.rm = TRUE), 3)
# ^ tapply by Opponent
H_G.lm.2 <- lm(A_C ~ Opponent + Location, data = H_G.coded)
summary(H_G.lm.2)
round(tapply(H_G.coded$A_C, H_G.coded[c("Opponent", "Location")], mean, na.rm = TRUE), 2)
# ^^^ again, unsure, but looks good lol

rm(test.set)

# # test.set <- as.numeric(as.factor(H_G.coded$Opponent))-1
# test.set <- H_G.coded[c("Location", "Win", "A_C", "Inc", "T_C", "T_S", "Tot_Inc", "Opponent")]
# head(test.set)
# 
# # DUMMY VARIABLES (don't actually need to do this :| if factors)
# test.data <- dummy_cols(test.set, select_columns = "Opponent")
# head(test.data)
