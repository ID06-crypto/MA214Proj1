# ============================================================
# Project 1, Part 3: Modeling and Diagnostics -- LOGISTIC REGRESSION
# MA 214 Applied Statistics
# ============================================================
# For a binary response. Filled in with mtcars (vs = 1: straight engine).
# Replace the lines marked  # <- EDIT  with your own.


library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)


# ------------------------------------------------------------
# 1. Loading the data
# ------------------------------------------------------------

# raw_data <- read_csv("my_data.csv")                      # <- EDIT: your data file
raw_data <- mtcars                                         # <- EDIT: delete this line

clean_data <- raw_data |>                                  # <- EDIT: your cleaning steps
  mutate(am = factor(am, levels = 0:1, labels = c("automatic", "manual")))

# 1b. Name your variables. Use the column names in clean_data, in quotes.
response   <- "vs"                                         # <- EDIT: binary, coded 0/1
predictors <- c("hp", "wt")                                # <- EDIT: at least two numerical
group_var  <- "am"                                         # <- EDIT: one categorical variable

table(clean_data[[response]])       # must be 0/1; 1 = the outcome you predict

# The first level of the factor is the reference level.
clean_data[[group_var]] <- factor(clean_data[[group_var]])
# clean_data[[group_var]] <- relevel(clean_data[[group_var]], ref = "LEVEL")   # <- EDIT (optional)

# Every model must use the same rows.
clean_data <- drop_na(clean_data, all_of(c(response, predictors, group_var)))

nrow(clean_data)                    # worksheet: rows after cleaning
levels(clean_data[[group_var]])     # first level = reference
mean(clean_data[[response]])        # share of 1s


# ------------------------------------------------------------
# 2. Candidate models (A fits, B compares)
# ------------------------------------------------------------
main_x <- "hp"                      # <- EDIT: the predictor your question is about

g1 <- glm(vs ~ hp, data = clean_data, family = binomial)           # <- EDIT: simple
g2 <- glm(vs ~ hp + wt, data = clean_data, family = binomial)      # <- EDIT: multiple
g3 <- glm(vs ~ hp + wt + am, data = clean_data, family = binomial) # <- EDIT: + categorical

# Model 4 is your choice.
g4 <- glm(vs ~ hp + am, data = clean_data, family = binomial)      # <- EDIT

# AIC: lower is better.
AIC(g1, g2, g3, g4)

# Accuracy: higher is better.
mean((fitted(g1) > 0.5) == clean_data$vs)     # <- EDIT: your response
mean((fitted(g2) > 0.5) == clean_data$vs)
mean((fitted(g3) > 0.5) == clean_data$vs)
mean((fitted(g4) > 0.5) == clean_data$vs)

cor(select(clean_data, all_of(predictors)))

# Your recommended model.
final_model <- g4                                          # <- EDIT


# ------------------------------------------------------------
# 3. Interpret (A)
# ------------------------------------------------------------
summary(final_model)
round(coef(final_model), 3)         # log odds
round(exp(coef(final_model)), 3)    # odds ratios
round(exp(confint(final_model)), 3)

# Worksheet: one sentence for a slope, one for an indicator.


# ------------------------------------------------------------
# 4. Check the model (C)
# ------------------------------------------------------------
diag_data <- clean_data |>
  mutate(prob = fitted(final_model))

# (i) Classification, cutoff 0.5
diag_data <- mutate(diag_data, pred = as.integer(prob > 0.5))
table(predicted = diag_data$pred, actual = diag_data[[response]])
mean(diag_data$pred == diag_data[[response]])    # accuracy
# Always guessing the more common outcome:
max(mean(diag_data[[response]]), 1 - mean(diag_data[[response]]))

# (ii) Calibration: points near the dashed line are good
diag_data |>
  mutate(bin = ntile(prob, 5)) |>
  group_by(bin) |>
  summarise(predicted = mean(prob), observed = mean(.data[[response]]), n = n()) |>
  ggplot(aes(x = predicted, y = observed)) +
  geom_abline(linetype = "dashed") +
  geom_point(size = 2) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Mean predicted probability", y = "Share of actual 1s")

# (iii) Linearity in the log odds: roughly a straight line is good
diag_data |>
  mutate(bin = ntile(.data[[main_x]], 4)) |>                       # <- EDIT: 4 groups
  group_by(bin) |>
  summarise(x = mean(.data[[main_x]]), n = n(), k = sum(.data[[response]]),
            log_odds = log((k + 0.5) / (n - k + 0.5))) |>
  ggplot(aes(x = x, y = log_odds)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.7) +
  labs(x = main_x, y = "Log odds of 1")

# (iv) Cases the model gets wrong
diag_data |>
  filter(pred != .data[[response]]) |>
  select(all_of(c(response, predictors, group_var)), prob)


# ------------------------------------------------------------
# 5. Predict (D)
# ------------------------------------------------------------
# Range of each predictor (stay inside it)
clean_data |>
  summarise(across(all_of(predictors), list(min = min, max = max)))

# Two hypothetical cars: change horsepower, keep weight and transmission fixed.
# Each position in c(...) is one case: first car, second car.
# EDIT: use your model's predictor names, realistic values, and existing categories.
new_cases <- tibble(
  hp = c(120, 180),                         # <- EDIT: main predictor
  wt = c(3, 3),                             # <- EDIT: same in both cases
  am = c("automatic", "automatic")           # <- EDIT: same category in both cases
)

new_cases$prob <- predict(final_model, newdata = new_cases, type = "response")
new_cases           # worksheet: case and predicted probability

# ggsave("calibration.png", width = 6, height = 4)
