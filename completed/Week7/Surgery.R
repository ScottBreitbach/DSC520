"Function to compute R^2 for Log Reg"

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1 - exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - (exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3),  "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),  "\n")
}

logisticPseudoR2s(survival_glm)


"Run the data through the model"
res <- predict(survival_glm, surgery_df, type = "response")
res
"Validate the model - confusion matrix"
confmatrix <- table(Actual_Value=surgery_df$Died_1Yr, Predicted_Value = res > 0.5)
confmatrix
"Calculate Accuracy"
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
