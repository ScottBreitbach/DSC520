## 18.1 Summary Statistics

x <- sample(x = 1:100, size = 100, replace = TRUE)
x

mean(x)


# copy x
y <- x


# choose a random 20 elements, using sample, to set to NA
y[sample(x = 1:100, size = 20, replace = FALSE)] <- NA
y

mean(y)  # will return NA
mean(y, na.rm = TRUE)


# calculate a WEIGHTED MEAN of a set of numbers:
grades <- c(95, 72, 87, 66)
weights <- c(1/2, 1/4, 1/8, 1/8)

mean(grades)  # 80
weighted.mean(x = grades, w = weights)  # 84.625


# calculate VARIANCE:
var(x)
  # can verify in R:
sum((x - mean(x))^2) / (length(x) - 1)


# Calculate STANDARD DEVIATION:
sd(x)
  # verify in R:
sqrt(var(x))

sd(y)  # will return NA
sd(y, na.rm = TRUE)


# other commonly used functions for summary statistics:
min(x)
max(x)
median(x)
min(y) # will return NA
min(y, na.rm = TRUE)

# Calculate 25th and 75th quantile:
quantile(x, probs = c(.25, .75))
quantile(y, probs = c(.25, .75), na.rm = TRUE)
  # calculate other quantiles:
quantile(x, probs = c(.1, .25, .5, .75, .99))

# get all these functions in one go:
summary(x)
summary(y)


## 18.2  Correlation and Covariance

# Testing relationship between two variables using correlation and covariance
library(ggplot2)
head(economics)
# pce is personal consumption expenditures
# psavert is personal savings rate

# Calculate CORRELATION:
cor(economics$pce, economics$psavert)
  # calculate each part of the correlation
xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
nMinusOne <- (nrow(economics) - 1)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)
  # use correlation formula
sum(xPart * yPart) / (nMinusOne * xSD * ySD)

# to compare multiple variables at once, use cor on a matrix:
cor(economics[, c(2, 4:6)])
  # visualize this info using a plot with ggpairs:
GGally::ggpairs(economics[, c(2, 4:6)])

# build a heatmap of the correlation numbers
library(reshape2)
library(scales)
  # build the correlation matrix
econCor <- cor(economics[, c(2, 4:6)])
  # melt it into the long format
econMelt <- melt(econCor, varnames = c("x", "y"), value.name = "Correlation")
  # order according to correlation
econMelt <- econMelt[order(econMelt$Correlation), ]
  # display the melted data
econMelt
  # plot it with ggplot draw tiles filling the color based on Correlation
ggplot(econMelt, aes(x = x, y = y)) +
  geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low = muted("red"), mid = "white",
                       high = "steelblue",
                       guide = guide_colorbar(ticks = FALSE, barheight = 10),
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(x = NULL, y = NULL)


# dealing with NAs in multiple columns
m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)
  # combine
theMat <- cbind(m, n, p, q, r)
  # "everything" (any NAs will result in NA)
cor(theMat, use = "everything")
  # "all.obs" (a single NA will cause an error)
cor(theMat, use = "all.obs")
  # "complete.obs" and "na.or.complete" (keep only rows w/o NAs)
  # note: if no good rows can be found, complete returns error, na returns NA
cor(theMat, use = "complete.obs")
cor(theMat, use = "na.or.complete")
    # calculate the correlation just on complete rows?
cor(theMat[c(1, 4, 7, 9, 10), ])
identical(cor(theMat, use = "complete.obs"),
          cor(theMat[c(1, 4, 7, 9, 10), ]))    # TRUE
  # "pairwise.complete" (compares every 2 columns 
  # and keeps rows where neither is NA)
cor(theMat, use = "pairwise.complete.obs")
    # compare to these:
cor(theMat[, c("m", "n")], use = "complete")
cor(theMat[, c("m", "p")], use = "complete")

# 'tips' data from reshape2 package:
data(tips, package = "reshape2")
head(tips)
GGally::ggpairs(tips)

# xkcd
install.packages("RXKCD")
library(RXKCD)
getXKCD(which = "552")

# cov function uses same arguments as cor function
cov(economics$pce, economics$psavert)
cov(economics[, c(2, 4:6)])
  # check that they're the same:
identical(cov(economics$pce, economics$psavert),
          cor(economics$pce, economics$psavert) *
            sd(economics$pce) * sd(economics$psavert))  # TRUE
