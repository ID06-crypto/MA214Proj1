# ==================================================
# PART 0: File Initialization
# ==================================================

# Importing data analysis functions
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Read csv data
raw_data <- read_csv("team_stats_2003_2023.csv")



# ==================================================
# PART 1: Data Cleaning
# ==================================================

# Keep or drop columns.
select(raw_data, -ties)                     # drop ties, not relevant to our research question

# Rename cols with long names, easier to type
rename(raw_data, win_pct = win_loss_perc, yds_play = yds_per_play_offense, net_yds = pass_net_yds_per_att)

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
         yds_play = yds_per_play_offense,
         net_yds = pass_net_yds_per_att)


nrow(raw_data)               # rows before cleaning (672)
nrow(clean_data)             # rows after cleaning (672)
colSums(is.na(clean_data))   # should all be 0
n_distinct(clean_data$franchise)   # 32

# Name your variables. Use the column names in clean_data, in quotes.
response   <- "win_pct"                                         # Our main response variable, binary
predictors <- c("score_pct", "turnovers", "rush_td")            # Three numerical predictors
group_var  <- "era"                                             # One categorical predictor

table(clean_data[[response]])       # must be 0/1; 1 = the outcome you predict

# The first level of the factor is the reference level.
clean_data[[group_var]] <- factor(clean_data[[group_var]])

# Every model must use the same rows.
clean_data <- drop_na(clean_data, all_of(c(response, predictors, group_var)))

nrow(clean_data)                    # worksheet: rows after cleaning
levels(clean_data[[group_var]])     # first level = reference
mean(clean_data[[response]])        # share of 1s



# ==================================================
# PART 2: Exploratory Data Analysis
# ==================================================

clean_data |>
  ggplot(aes(x = penalties)) +
  geom_density(fill = "blue", alpha = 0.5, linewidth = 1) +
  labs(title = "Density of Penalties (Season)")
theme_minimal()

# Correlation between penalties and fumbles lost
cor(clean_data$penalties, clean_data$fumbles_lost) # ~0.0711

clean_data |>
  ggplot(aes(x = scale(penalties), y = scale(win_pct))) +
  geom_point() +
  labs(title = "win_pct ~ penalties", 
       x = "Penalties (scaled)", 
       y = "Win/Loss Percentage (scaled)") +
  theme_minimal()

# Histogram of the response: how win percentage is spread across all 672
# team-seasons (each bar counts the team-seasons in one win% range).
ggplot(clean_data, aes(x = win_pct)) +
  geom_histogram(bins = 20) 
labs(x = "Win percentage (fraction of games won)", y = "Number of team seasons")
# Saves the histogram above as a picture for the Google Doc.
ggsave("win_hist.png", width = 6, height = 4)

# Bar chart of the categorical variable: number of team-seasons in each era.
# Each era has the same number of rows (224), so the bars are equal.
ggplot(clean_data, aes(x = era)) +
  geom_bar() +
  labs(x = "Era (seasons)", y = "Number of team seasons")

# Scatterplot of win percentage against offensive yards per play. Each dot is
# one team-season. The solid line is the straight-line (lm) fit and the dashed
# line is a smooth curve (loess): if they are close, the pattern is linear.
ggplot(clean_data, aes(x = yds_play, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Offensive yards per play (yards)", y = "Win percentage (fraction of games won)")
# Saves the scatterplot above as a picture for the Google Doc.
ggsave("win_vs_yds_play.png", width = 6, height = 4)

# Boxplot of win percentage for each era: compares the median, spread and
# outliers of win% across the three groups of seasons.
ggplot(clean_data, aes(x = era, y = win_pct)) +
  geom_boxplot() +
  labs(x = "Era (seasons)", y = "Win percentage (fraction of games won)")

# Does the yards-per-play relationship differ by era? First plot: all dots
# on one graph, colored by era. Second plot: one panel per era.
ggplot(clean_data, aes(x = yds_play, y = win_pct, color = era)) +
  geom_point() +
  labs(x = "Offensive yards per play (yards)", y = "Win percentage (fraction of games won)")
ggplot(clean_data, aes(x = yds_play, y = win_pct)) +
  geom_point() +
  facet_wrap(~ era) +
  labs(x = "Offensive yards per play (yards)", y = "Win percentage (fraction of games won)")

# Grid of scatterplots: each of the five variables plotted against every
# other, to spot predictors that move together (a tight line = high correlation).
pairs(select(clean_data, win_pct, yds_play, to_pg, pen_pg, opp_pg))

# Correlation between two quantitative variables...
cor(clean_data$win_pct, clean_data$yds_play)
cor(clean_data$win_pct, clean_data$margin_pg)
cor(clean_data$win_pct, clean_data$opp_pg)
cor(clean_data$win_pct, clean_data$total_yards)
cor(clean_data$win_pct, clean_data$plays_offense)
cor(clean_data$win_pct, clean_data$yds_play)
cor(clean_data$win_pct, clean_data$fumbles_lost)
cor(clean_data$win_pct, clean_data$turnovers)
cor(clean_data$win_pct, clean_data$pass_cmp)
cor(clean_data$win_pct, clean_data$pass_att)
cor(clean_data$win_pct, clean_data$pass_td)
cor(clean_data$win_pct, clean_data$net_yds)
cor(clean_data$win_pct, clean_data$pass_fd)
cor(clean_data$win_pct, clean_data$rush_yds)
cor(clean_data$win_pct, clean_data$rush_td)
cor(clean_data$win_pct, clean_data$rush_yds_per_att)
cor(clean_data$win_pct, clean_data$rush_fd)
cor(clean_data$win_pct, clean_data$penalties)
cor(clean_data$win_pct, clean_data$pen_fd)
cor(clean_data$win_pct, clean_data$score_pct)
cor(clean_data$win_pct, clean_data$turnover_pct)
cor(clean_data$win_pct, clean_data$exp_pts_tot)
cor(clean_data$win_pct, clean_data$to_pg)
cor(clean_data$win_pct, clean_data$pen_pg)



# ==================================================
# PART 3: Model Candidacy
# ==================================================

main_x <- "score_pct"

# Potential Model Candidates
m1 <- lm(win_pct ~ score_pct, data = clean_data) 
m2 <- lm(win_pct ~ score_pct + rush_td, data = clean_data)      
m3 <- lm(win_pct ~ score_pct + rush_td + turnovers, data = clean_data) 
m4 <- lm(win_pct ~ score_pct + rush_td + era, data = clean_data) 
m5 <- lm(win_pct ~ score_pct + turnovers + era, data = clean_data) 
m6 <- lm(win_pct ~ score_pct + rush_td + turnovers + era, data = clean_data) 

# Adjusted R^2 calculations
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared
summary(m5)$adj.r.squared
summary(m6)$adj.r.squared

# AIC calculations
AIC(m1, m2, m3, m4, m5, m6)

# Check for multicollinearity
cor(select(clean_data, all_of(predictors)))

# Model we decide on
final_model <- m6



# ==================================================
# PART 4: Model Diagnostics & Interpretations
# ==================================================

summary(final_model)
round(coef(final_model), 3)

#ALL DIAGNOSTICS BELOW

diag_data <- clean_data |>
  mutate(fitted = fitted(final_model),
         resid  = resid(final_model))

# Residuals vs fitted values
ggplot(diag_data, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  labs(x = paste("Fitted", response), y = "Residual")

# Residuals vs the main predictor
ggplot(diag_data, aes(x = .data[[main_x]], y = resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  labs(x = main_x, y = "Residual")

# Normal Q-Q plot: check whether residual points roughly follow the line.
ggplot(diag_data, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot of residuals",
       x = "Theoretical normal quantiles", y = "Residual quantiles")

# PREDICTIONS BELOW

# Graph our predicted data vs actual data
clean_data$predicted <- predict(m6)

ggplot(clean_data, aes(x = predicted, y = win_pct)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs. Predicted NFL Winning Percentage",
    x = "Predicted Winning Percentage",
    y = "Actual Winning Percentage"
  ) +
  theme_minimal()
