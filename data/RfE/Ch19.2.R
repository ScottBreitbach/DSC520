library(ggplot2)

setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC520/")

housing <- read.table("data/RfE/housing.csv", 
                      sep = ",", header = TRUE, 
                      stringsAsFactors = FALSE)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")
head(housing)

ggplot(housing, aes(x = ValuePerSqFt)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Square Foot")

ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Square Foot")

ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Square Foot") +
  facet_wrap(~Boro)

ggplot(housing, aes(x = SqFt)) + geom_histogram()
ggplot(housing, aes(x = Units)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ], aes(x = SqFt)) + 
  geom_histogram()
ggplot(housing[housing$Units < 1000, ], aes(x = Units)) + 
  geom_histogram()
