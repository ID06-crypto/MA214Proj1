# Import data analysis functions
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

nfl_data <- read_csv("team_stats_2003_2023.csv") # NFL Team Statistics 2003-2023

# Quick looks at the NFL Team Data
head(nfl_data)      # first rows: what does one row represent?
dim(nfl_data)       # number of observations, number of variables
names(nfl_data)     # variable names
glimpse(nfl_data)   # type of each variable
summary(nfl_data)   # ranges, medians, and NA counts

# Data tidying
nfl_data <- nfl_data %>%
  select(-c(team, g, ties))
  
nfl_data |>
  ggplot(aes(x = penalties)) +
  geom_density(fill = "blue", alpha = 0.5, linewidth = 1) +
  labs(title = "Density of Penalties (Season)")
  theme_minimal()

penalties_normalized = min_max_norm
# fumbles ~ penalties, looking for any possible collinearity
ggplot(nfl_data, aes(x = fumbles_lost, y = penalties)) +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Scatterplot between fumbles and penatlies", x = "Fumbles Lost", y = "Penalties")

# Correlation between penalties and fumbles lost
cor(nfl_data$penalties, nfl_data$fumbles_lost) # ~0.0711

# =============
# Model Fitting
# =============

# 
nfl_data |>
  ggplot(aes(x = scale(penalties), y = scale(win_loss_perc))) +
  geom_point() +
  labs(title = "winn_loss_perc ~ penalties", 
       x = "Penalties (scaled)", 
       y = "Winn/Loss Percentage (scaled)") +
  theme_minimal()

cor(nfl_data$penalties, nfl_data$win_loss_perc)