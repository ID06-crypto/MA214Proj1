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

raw_data <- read_csv("team_stats_2003_2023.csv")                      # <- EDIT: your data file

# Keep or drop columns.
select(raw_data, -ties)                     # drop ties, not relevant to our research question

# Rename cols with long names, easier to type
rename(raw_data, win_pct = win_loss_perc, yds_play = yds_per_play_offense)

# Remove impossible values or rows outside your question.
# Checked for impossible values and didn't find any, none dropped.

# Derived variable margin of victory -> margin per game because mov had a lot of missing vals in og dataset
mutate(raw_data, margin_pg = points_diff / g)

# Group renamed teams by franchise (to get 32 unique NFL teams).
mutate(raw_data, franchise = case_when(
  grepl("Redskins|Football Team|Commanders", team) ~ "Washington",
  grepl("Raiders",  team)                          ~ "Raiders",
  grepl("Chargers", team)                          ~ "Chargers",
  grepl("Rams",     team)                          ~ "Rams",
  TRUE                                             ~ team))

# Changed to per game metrics, so 16- and 17-game seasons are comparable.
mutate(raw_data,
       to_pg  = turnovers / g,
       pen_pg = penalties / g,
       opp_pg = points_opp / g)

# Putting the steps together with the pipe |>
clean_data <- raw_data |>
  
  # mov has a lot of missing vals, so rebuild it for every row
  mutate(margin_pg = points_diff / g) |>
  # group together renamed teams by franchise to get 32 unique NFL teams
  mutate(franchise = case_when(
    grepl("Redskins|Football Team|Commanders", team) ~ "Washington",
    grepl("Raiders",  team)                          ~ "Raiders",
    grepl("Chargers", team)                          ~ "Chargers",
    grepl("Rams",     team)                          ~ "Rams",
    TRUE                                             ~ team)) |>
  # 2021 to 2023 have 17 games, so compare totals per game
  mutate(to_pg  = turnovers / g,
         pen_pg = penalties / g,
         opp_pg = points_opp / g) |>
  # group years into three equal eras, in time order, to mirror in class dataset + see if metrics/predictors change through time
  mutate(era = case_when(
    year <= 2009 ~ "2003-09",
    year <= 2016 ~ "2010-16",
    TRUE         ~ "2017-23")) |>
  mutate(era = factor(era, levels = c("2003-09", "2010-16", "2017-23"))) |>
  # shorter names
  rename(win_pct = win_loss_perc,
         yds_play = yds_per_play_offense)


nrow(raw_data)               # rows before cleaning (672)
nrow(clean_data)             # rows after cleaning (672)
colSums(is.na(clean_data))   # should all be 0
n_distinct(clean_data$franchise)   # 32


# 1b. Name your variables. Use the column names in clean_data, in quotes.
response   <- "win_pct"                                         # <- EDIT: binary, coded 0/1
predictors <- c("turnovers", "fumbles_lost", "penalties")                                # <- EDIT: at least two numerical
group_var  <- "era"                                         # <- EDIT: one categorical variable

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
main_x <- "turnovers"                      # <- EDIT: the predictor your question is about

g1 <- glm(win_pct ~ turnovers, data = clean_data, family = binomial)           # <- EDIT: simple
g2 <- glm(win_pct ~ turnovers + fumbles_lost, data = clean_data, family = binomial)      # <- EDIT: multiple
g3 <- glm(win_pct ~ turnovers + fumbles_lost + yds_play + era, data = clean_data, family = binomial) # <- EDIT: + categorical

# Model 4 is your choice.
g4 <- glm(win_pct ~ turnovers + yds_play + era, data = clean_data, family = binomial)      # <- EDIT

# AIC: lower is better.
AIC(g1, g2, g3, g4)

# Accuracy: higher is better.
mean((fitted(g1) > 0.5) == clean_data$win_pct)     # <- EDIT: your response
mean((fitted(g2) > 0.5) == clean_data$win_pct)
mean((fitted(g3) > 0.5) == clean_data$win_pct)
mean((fitted(g4) > 0.5) == clean_data$win_pct)

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
