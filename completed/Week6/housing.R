library(readxl)
library(ggplot2)

week_6_housing <- read_excel("completed/Week6/week-6-housing.xlsx")
week_7_housing <- read_excel("completed/Week6/week-7-housing.xlsx")
# head(week_6_housing)
testDF <- week_6_housing[, c()]


all.equal(week_6_housing, week_7_housing)
# TRUE

"Plots"
ggplot(week_6_housing, aes(x = `Sale Date`, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
# ggplot(week_6_housing, aes(x = sale_reason, y = `Sale Price`)) + geom_point() + geom_smooth()
# ggplot(week_6_housing, aes(x = sale_instrument, y = `Sale Price`)) + geom_point() + geom_smooth()
# ggplot(week_6_housing, aes(x = sale_warning, y = `Sale Price`)) + geom_point() + geom_smooth()
# ggplot(week_6_housing, aes(x = sitetype, y = `Sale Price`)) + geom_point() + geom_smooth()
ggplot(week_6_housing, aes(x = zip5, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = lon, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = lat, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = building_grade, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = square_feet_total_living, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = square_feet_total_living, y = `Sale Price`)) + geom_point() + geom_smooth()
ggplot(week_6_housing, aes(x = bedrooms, y = `Sale Price`)) + geom_point() + geom_smooth()
ggplot(week_6_housing, aes(x = bath_full_count, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = bath_half_count, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = bath_3qtr_count, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = year_built, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
ggplot(week_6_housing, aes(x = year_renovated, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
# ggplot(week_6_housing, aes(x = current_zoning, y = `Sale Price`)) + geom_point() + geom_smooth()
ggplot(week_6_housing, aes(x = sq_ft_lot, y = `Sale Price`)) + geom_point() + geom_smooth(method = "lm")
# ggplot(week_6_housing, aes(x = prop_type, y = `Sale Price`)) + geom_point() + geom_smooth()
# ggplot(week_6_housing, aes(x = present_use, y = `Sale Price`)) + geom_point() + geom_smooth()
ggplot(week_6_housing, aes(x = log(sq_ft_lot), y = `Sale Price`, fill = factor(zip5))) + geom_point(aes(color = factor(building_grade))) + geom_smooth(method = "lm")


"Boxplots"
boxplot(week_6_housing$`Sale Price`, xlab = "Sale Price") 
boxplot(week_6_housing$`Sale Date`, xlab = "Sale Date")
boxplot(week_6_housing$lat, xlab = "Latitude")$out
boxplot(week_6_housing$building_grade, xlab = "Building Grade")
boxplot(week_6_housing$square_feet_total_living, xlab = "Square Feet Living")
boxplot(week_6_housing$bedrooms, xlab = "Bedrooms")
boxplot(week_6_housing$bath_full_count, xlab = "Full Baths")
boxplot(week_6_housing$bath_half_count, xlab = "Half Baths")
boxplot(week_6_housing$bath_3qtr_count, xlab = "3/4 Baths")
boxplot(week_6_housing$year_built, xlab = "Year Built")
boxplot(week_6_housing$year_renovated, xlab = "Year Renovated")
boxplot(week_6_housing$sq_ft_lot, xlab = "Lot Size")

# "Removing Outliers"  # Well this didn't work :( 
# Q <- quantile(week_6_housing$`Sale Price`, probs = c(.25, .75), na.rm = FALSE)
# iqr <- IQR(week_6_housing$`Sale Price`)
# up <- Q[2] + 1.5 * iqr # Upper Range
# low <- Q[1] - 1.5 * iqr # Lower Range
# eliminated <- subset(week_6_housing, week_6_housing$`Sale Price` > up & week_6_housing$`Sale Price` < low)
# boxplot(eliminated, xlab = "Sale Price") 

sd(week_6_housing$`Sale Price`)
sd(album2$adverts)
sd(album2$sales)

"Histograms"
qplot(data = week_6_housing, x = `Sale Price`)
qplot(data = week_6_housing, x = `Sale Date`)
qplot(data = week_6_housing, x = lat)
qplot(data = week_6_housing, x = building_grade)
qplot(data = week_6_housing, x = square_feet_total_living)
qplot(data = week_6_housing, x = bedrooms)
qplot(data = week_6_housing, x = bath_full_count)
qplot(data = week_6_housing, x = bath_half_count)
qplot(data = week_6_housing, x = bath_3qtr_count)
qplot(data = week_6_housing, x = year_built)
qplot(data = week_6_housing, x = year_renovated)
qplot(data = week_6_housing, x = sq_ft_lot)

"Simple Regressions"
priceDate <- lm(`Sale Price` ~ `Sale Date`, data = week_6_housing)
summary(priceDate)
# priceZip <- lm(`Sale Price` ~ zip5, data = week_6_housing)
# summary(priceZip)
priceGrade <- lm(`Sale Price` ~ building_grade, data = week_6_housing)
summary(priceGrade)
priceSqFt <- lm(`Sale Price` ~ square_feet_total_living, data = week_6_housing)
summary(priceSqFt)
priceBeds <- lm(`Sale Price` ~ bedrooms, data = week_6_housing)
summary(priceBeds)
priceBaFul <- lm(`Sale Price` ~ bath_full_count, data = week_6_housing)
summary(priceBaFul)
priceBaHaf <- lm(`Sale Price` ~ bath_half_count, data = week_6_housing)
summary(priceBaHaf)
priceBa3Q <- lm(`Sale Price` ~ bath_3qtr_count, data = week_6_housing)
summary(priceBa3Q)
priceYrBlt <- lm(`Sale Price` ~ year_built, data = week_6_housing)
summary(priceYrBlt)
priceYrRen <- lm(`Sale Price` ~ year_renovated, data = week_6_housing)
summary(priceYrRen)
priceLot <- lm(`Sale Price` ~ sq_ft_lot, data = week_6_housing)
summary(priceLot)

"Model"
housing_lm.2 <- lm(`Sale Price` ~ sq_ft_lot + bedrooms + square_feet_total_living + 
                     bath_full_count, data = week_6_housing)
summary(housing_lm.2)

"Diagnostics"
week_6_housing.2 <- week_6_housing[, c('Sale Date', 'Sale Price', "addr_full", 
                                       "building_grade", "square_feet_total_living", 
                                       "year_built", "sq_ft_lot")]

week_6_housing.2$residuals<-resid(housing_lm)
week_6_housing.2$standardized.residuals<-rstandard(housing_lm)
week_6_housing.2$studentized.residuals<-rstudent(housing_lm)
week_6_housing.2$cooks.distance<-cooks.distance(housing_lm)
week_6_housing.2$dfbeta<-dfbeta(housing_lm)
week_6_housing.2$dffit<-dffits(housing_lm)
week_6_housing.2$leverage<-hatvalues(housing_lm)
week_6_housing.2$covariance.ratios<-covratio(housing_lm)

  "Large Residuals"
week_6_housing.2$large.residual<-week_6_housing.2$standardized.residuals > 2 | 
  week_6_housing.2$standardized.residuals < -2

sum(week_6_housing.2$large.residual)

week_6_housing.2[week_6_housing.2$large.residual, 
                 c('Sale Date', "building_grade", "square_feet_total_living", 
                   "bedrooms", "bath_full_count", "bath_half_count", "bath_3qtr_count", 
                   "year_built", "year_renovated", "sq_ft_lot", "standardized.residuals")]

week_6_residuals<-week_6_housing.2[week_6_housing.2$large.residual, 
                                   c('Sale Date', "building_grade", 
                                     "square_feet_total_living", "bedrooms", 
                                     "bath_full_count", "bath_half_count", 
                                     "bath_3qtr_count", "year_built", 
                                     "year_renovated", "sq_ft_lot", 
                                     "standardized.residuals")]

write.table(week_6_residuals, "Week 6 Residuals.csv", sep = ",", row.names = FALSE)

week_6_diagnostics<-week_6_housing.2[week_6_housing.2$large.residual, 
                                     c("cooks.distance", "leverage", 
                                       "covariance.ratios")]

write.table(week_6_diagnostics, "Week 6 Diagnostics.csv", sep = ",", row.names = FALSE)


"Replace 0s with NAs"
week_6_housing.3 <- week_6_housing
week_6_housing.3$year_renovated[week_6_housing.3$year_renovated==0] <- NA
ggplot(week_6_housing.3, aes(x = year_renovated, y = `Sale Price`)) + geom_point() + geom_smooth()
boxplot(week_6_housing.3$year_renovated, xlab = "Year Renovated")
qplot(data = week_6_housing.3, x = year_renovated)
priceYrRen.3 <- lm(`Sale Price` ~ year_renovated, data = week_6_housing.3)
summary(priceYrRen.3)


housing_lm.3 <- lm(`Sale Price` ~ square_feet_total_living + bedrooms + bath_full_count + year_built + building_grade, data = week_6_housing.3)
housing_lm.3 <- lm(`Sale Price` ~ square_feet_total_living + bedrooms + bath_full_count + year_built + building_grade + year_renovated, data = week_6_housing.3)
summary(housing_lm.3)


"Replace >0s with 1s"
week_6_housing.4 <- week_6_housing
week_6_housing.4$year_renovated[week_6_housing.4$year_renovated>0] <- 1

housing_lm.4 <- lm(`Sale Price` ~ square_feet_total_living + bedrooms + bath_full_count + year_built + building_grade, data = week_6_housing.4)
housing_lm.4 <- lm(`Sale Price` ~ square_feet_total_living + bedrooms + bath_full_count + year_built + building_grade + year_renovated, data = week_6_housing.4)
summary(housing_lm.4)


"Remove a row?"
week_6_housing.5 <- week_6_housing
week_6_housing.5 <- week_6_housing.5[!week_6_housing.5$lat < 47.5, ]


"Assignment"
week_6_housing <- read_excel("completed/Week6/week-6-housing.xlsx")
week_6_housing$bath_full_count[week_6_housing$bath_full_count==23] <- 4
week_6_housing <- week_6_housing[!week_6_housing$lat < 47.5, ]
priceSqFt <- lm(`Sale Price` ~ sq_ft_lot, data = week_6_housing)
housing_lm <- lm(`Sale Price` ~ sq_ft_lot + square_feet_total_living + building_grade + year_built, data = week_6_housing)

ggplot(week_6_housing, aes(x = `Sale Price`)) + geom_histogram()
ggplot(week_6_housing, aes(x = `Sale Price`, fill = factor(zip5))) + geom_histogram()
ggplot(week_6_housing, aes(x = `Sale Price`, fill = factor(zip5))) + geom_histogram() + facet_wrap(~zip5)

"Leverage"
diagnostics_df.2 <- diagnostics_df
avgLeverage <- mean(diagnostics_df.2$leverage)
avgLeverage
avgLeverage * 2
avgLeverage * 3

diagnostics_df.2$leverageX2<-diagnostics_df.2$leverage > (avgLeverage * 2)
diagnostics_df.2[diagnostics_df.2$leverageX2 , c("leverage")]
sum(diagnostics_df.2$leverageX2)
diagnostics_df.2$leverageX3<-diagnostics_df.2$leverage > (avgLeverage * 3)
diagnostics_df.2[diagnostics_df.2$leverageX3 , c("leverage")]
sum(diagnostics_df.2$leverageX3)

"Playing around w/Lg Residual AND Leverage"
# diagnostics_df$large.residual<-diagnostics_df$standardized.residuals > 2 | 
#   diagnostics_df$standardized.residuals < -2

# sum(diagnostics_df$large.residual)

# diagnostics_df[diagnostics_df$large.residual, 
#                c("sq_ft_lot", "square_feet_total_living", "building_grade", 
#                  "year_built", "standardized.residuals")]

diagnostics_df$leverageX2<-diagnostics_df$leverage > (avgLeverage * 2)

sum(diagnostics_df$leverageX2)
sum(diagnostics_df$large.residual & diagnostics_df$leverageX2)

# diagnostics_df[diagnostics_df$large.residual & diagnostics_df$leverageX2, 
#                c("sq_ft_lot", "square_feet_total_living", "building_grade", 
#                  "year_built", "leverage")]

diag_df_lev_sort <- diagnostics_df[order(-diagnostics_df$leverage),]
# diag_df_lev_sort[diag_df_lev_sort$large.residual & diag_df_lev_sort$leverageX2,
#                c("sq_ft_lot", "square_feet_total_living", "building_grade",
#                  "year_built", "leverage")]
diag_df_lev_sort[diag_df_lev_sort$large.residual, 
                 c("sq_ft_lot", "square_feet_total_living", "building_grade", 
                   "year_built", "leverage", "standardized.residuals")]

"Saving Leverage section from RMD file"
leverage_df <- diagnostics_df
avgLeverage <- mean(leverage_df$leverage)
avgLeverage

avgLeverage * 2
leverage_df$leverageX2<-leverage_df$leverage > (avgLeverage * 2)
sum(leverage_df$leverageX2)

avgLeverage * 3
leverage_df$leverageX3<-leverage_df$leverage > (avgLeverage * 3)
sum(leverage_df$leverageX3)

lev3X <- leverage_df[leverage_df$leverageX3, c("leverage")]
lev3X[order(-lev3X),]

"Plots and Histograms"
diagnostics_df.2 <- diagnostics_df
diagnostics_df.2$fitted <- diagnostics_df.2$fitted.values

hist(diagnostics_df.2$studentized.residuals)
ggplot(diagnostics_df.2, aes(studentized.residuals)) + geom_histogram()
histogram<-ggplot(diagnostics_df.2, aes(studentized.residuals)) + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Studentized Residual", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(diagnostics_df.2$studentized.residuals, na.rm = TRUE), sd = sd(diagnostics_df.2$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
histogram
scatter <- ggplot(diagnostics_df.2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")
library(ggplot2)

# plot(diagnostics_df.2$fitted.values,rstandard(diagnostics_df.2))
# plot(diagnostics_df.2)
plot(housing_lm)

hist(diagnostics_df$studentized.residuals)
hist(rstudent(housing_lm))
hist(rstandard(housing_lm))

qplot(sample = diagnostics_df.2$studentized.residuals)

diagnostics_df[diagnostics_df$large.residual,
          c("sq_ft_lot", "square_feet_total_living", "building_grade", 
            "year_built", "cooks.distance", "standardized.residuals")
          
