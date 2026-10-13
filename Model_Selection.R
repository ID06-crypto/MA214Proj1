library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Load data
df <- read_csv("clean_data.csv")
df <- df[, colSums(is.na(df)) == 0]

# Response and predictors
response   <- "win_pct"
predictors <- setdiff(names(df), response)

# Full model (baseline)
# Backticks protect column names that contain spaces or odd characters
make_formula <- function(vars) {
  reformulate(sprintf("`%s`", vars), response = response)
}

full_model  <- lm(make_formula(predictors), data = df)
base_adj_r2 <- summary(full_model)$adj.r.squared
base_aic    <- AIC(full_model)

# Drop one predictor at a time
results <- data.frame(
  dropped_predictor  = predictors,
  adjusted_r_squared = NA_real_,
  AIC                = NA_real_
)

for (i in seq_along(predictors)) {
  model <- lm(make_formula(setdiff(predictors, predictors[i])), data = df)
  results$adjusted_r_squared[i] <- summary(model)$adj.r.squared
  results$AIC[i]                <- AIC(model)
}

# Normalization
# Min-max scaling to 0 to 1; guards against divide-by-zero if all values are equal
minmax <- function(x) {
  rng <- max(x) - min(x)
  if (rng == 0) return(rep(0, length(x)))
  (x - min(x)) / rng
}

results <- results %>%
  mutate(
    # Change relative to the full model
    delta_AIC    = AIC - base_aic,                      # > 0: dropping makes AIC worse
    delta_adj_r2 = adjusted_r_squared - base_adj_r2,    # < 0: dropping makes fit worse
    
    # Normalized importance (0 = least important, 1 = most important)
    # Both are oriented so that higher = dropping the variable hurts more
    norm_AIC_importance    = minmax(delta_AIC),
    norm_adj_r2_importance = minmax(-delta_adj_r2)
  ) %>%
  arrange(desc(norm_AIC_importance))

# Print results
cat("\nFull model baseline:\n")
cat("  Adjusted R^2:", round(base_adj_r2, 4), "\n")
cat("  AIC:         ", round(base_aic, 2), "\n\n")
print(as.data.frame(results), digits = 4)

# Plot 1: Change in AIC when each predictor is dropped
p1 <- results %>%
  mutate(effect = ifelse(delta_AIC > 0,
                         "Dropping hurts (keep it)",
                         "Dropping helps (candidate to remove)")) %>%
  ggplot(aes(x = reorder(dropped_predictor, delta_AIC), y = delta_AIC, fill = effect)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("Dropping hurts (keep it)" = "#2c7fb8",
                               "Dropping helps (candidate to remove)" = "#d95f02")) +
  labs(title = "Change in AIC When Each Predictor Is Dropped",
       subtitle = "Relative to the full model (lower AIC is better)",
       x = NULL, y = "Delta AIC", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Change in adjusted R^2 when each predictor is dropped
p2 <- results %>%
  mutate(effect = ifelse(delta_adj_r2 < 0,
                         "Dropping hurts (keep it)",
                         "Dropping helps (candidate to remove)")) %>%
  ggplot(aes(x = reorder(dropped_predictor, -delta_adj_r2), y = delta_adj_r2, fill = effect)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("Dropping hurts (keep it)" = "#2c7fb8",
                               "Dropping helps (candidate to remove)" = "#d95f02")) +
  labs(title = "Change in Adjusted R^2 When Each Predictor Is Dropped",
       subtitle = "Relative to the full model (higher adjusted R^2 is better)",
       x = NULL, y = "Delta adjusted R^2", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Normalized importance, AIC vs adjusted R^2 side by side
p3 <- results %>%
  select(dropped_predictor, norm_AIC_importance, norm_adj_r2_importance) %>%
  pivot_longer(-dropped_predictor, names_to = "metric", values_to = "importance") %>%
  mutate(metric = recode(metric,
                         norm_AIC_importance    = "AIC",
                         norm_adj_r2_importance = "Adjusted R^2")) %>%
  ggplot(aes(x = reorder(dropped_predictor, importance, FUN = max),
             y = importance, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("AIC" = "#2c7fb8", "Adjusted R^2" = "#d95f02")) +
  labs(title = "Normalized Predictor Importance (Leave-One-Out)",
       subtitle = "0 = least important, 1 = most important (min-max scaled within each metric)",
       x = NULL, y = "Normalized importance", fill = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
print(p2)
print(p3)
