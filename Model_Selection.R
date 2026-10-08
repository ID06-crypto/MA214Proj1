library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Loading Data
df <- read_csv("team_stats_2003_2023.csv")

# Defining response and predictors
response <- "win_loss_perc"

predictors <- setdiff(
  names(df),
  response
)

# Splitting into Training and Testing Data
set.seed(123)

n <- nrow(df)

train_index <- sample(
  1:n,
  size = 0.80 * n
)

train_df <- df[train_index, ]
test_df <- df[-train_index, ]



# Fit the full model
full_formula <- as.formula(
  paste(
    response,
    "~",
    paste(predictors, collapse = " + ")
  )
)

full_model <- lm(
  full_formula,
  data = train_df
)


results <- data.frame(x
  dropped_predictor = "Full Model",
  adjusted_r_squared = summary(full_model)$adj.r.squared,
  AIC = AIC(full_model)
)


# Drop one predictor at a time and calculate AIC and R^2 Adjusted
for (col in predictors) {
  
  # Keep every predictor except the one being dropped
  remaining_predictors <- setdiff(
    predictors,
    col
  )
  
  # Create formula
  formula <- as.formula(
    paste(
      response,
      "~",
      paste(remaining_predictors, collapse = " + ")
    )
  )
  
  # Fit  model
  model <- lm(formula, data = train_df)
  
  # Save results
  results <- rbind(
    results,
    data.frame(
      dropped_predictor = col,
      adjusted_r_squared = summary(model)$adj.r.squared,
      AIC = AIC(model)
    )
  )
}