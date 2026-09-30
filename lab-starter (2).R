# ============================================================
# Project 1, Part 2: Data Cleaning and EDA
# MA 214 Applied Statistics
# ============================================================
# A toolbox of functions for cleaning and exploring your data.
# You will not need all of them. Copy the ones you need into
# P1_GroupNumber_EDA.R and change the variable names to yours.
#
# Every line runs on the placeholder data (airquality), so you can
# try a function and see what it does before using it on your data.
# Do not use setwd(). Keep any data file in this same folder.

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 0. Load your data (everyone)
# ------------------------------------------------------------
# TODO: replace these two lines with the loading code from Part 1.
data(airquality)
raw_data <- airquality


# ------------------------------------------------------------
# 1. Inspect (A): what is wrong with the raw data?
# ------------------------------------------------------------
dim(raw_data)                    # rows and columns
glimpse(raw_data)                # type of each variable
head(raw_data, 10)               # first 10 rows
summary(raw_data)                # ranges, medians, NA counts
colSums(is.na(raw_data))         # missing values in each column
sum(duplicated(raw_data))        # repeated rows
n_distinct(raw_data$Month)       # number of distinct values
range(raw_data$Wind, na.rm = TRUE)          # smallest and largest value
table(raw_data$Month, useNA = "ifany")      # counts of each level
count(raw_data, Month, sort = TRUE)         # the same, as a data frame
filter(raw_data, Wind > 18)                 # look at suspicious rows
filter(raw_data, is.na(Ozone))              # look at rows with a missing value


# ------------------------------------------------------------
# 2. Clean (A): each step is a decision for the cleaning log
# ------------------------------------------------------------

# Keep or drop columns.
select(raw_data, Ozone, Temp, Month)        # keep these
select(raw_data, -Day)                      # drop this one

# Rename: new_name = old_name.
rename(raw_data, ozone = Ozone, temp_f = Temp)

# Remove impossible values or rows outside your question.
filter(raw_data, Temp > 0)
filter(raw_data, Month %in% c(6, 7, 8))

# A number that stands for a category -> labeled factor.
mutate(raw_data, Month = factor(Month, levels = 5:9,
                                labels = c("May", "Jun", "Jul", "Aug", "Sep")))

# Choose the reference level (the group the others are compared to).
mutate(raw_data, Month = relevel(factor(Month), ref = "7"))

# A code such as -99 or 999 that means "missing" -> NA.
mutate(raw_data, Ozone = na_if(Ozone, 999))

# Missing values: drop rows missing the variables you will use...
drop_na(raw_data, Ozone, Solar.R)
# ...or fill them in, and say why that is reasonable.
replace_na(raw_data, list(Solar.R = 0))

# Derived variables: unit change, log, ratio.
mutate(raw_data,
       temp_c    = (Temp - 32) * 5 / 9,
       log_ozone = log(Ozone),
       ozone_per_degree = Ozone / Temp)

# Group a quantitative variable, or combine levels.
mutate(raw_data, windy = if_else(Wind > 10, "windy", "calm"))
mutate(raw_data, season = case_when(
  Month %in% c(5, 6) ~ "early",
  Month %in% c(7, 8) ~ "peak",
  TRUE               ~ "late"))
mutate(raw_data, temp_group = cut(Temp, breaks = c(50, 70, 85, 100)))

# Fix a variable stored with the wrong type.
mutate(raw_data, Day = as.numeric(Day))
parse_number(c("$1,200", "35%"))            # text with symbols -> number

# Remove repeated rows.
distinct(raw_data)

# Put the steps together with the pipe |>. One comment per step saying WHY.
clean_data <- raw_data |>
  rename(ozone = Ozone, solar = Solar.R, wind = Wind, temp_f = Temp,
         month = Month, day = Day) |>
  mutate(month = factor(month, levels = 5:9,
                        labels = c("May", "Jun", "Jul", "Aug", "Sep"))) |>
  drop_na(ozone, solar)

nrow(raw_data)     # rows before cleaning
nrow(clean_data)   # rows after cleaning

# Save the cleaned data (optional).
# write_csv(clean_data, "clean_data.csv")


# ------------------------------------------------------------
# 3. Summaries (B)
# ------------------------------------------------------------

# One quantitative variable: center, spread, range.
clean_data |>
  summarise(n = n(), mean = mean(ozone), median = median(ozone),
            sd = sd(ozone), min = min(ozone), max = max(ozone))
quantile(clean_data$ozone, c(0.25, 0.5, 0.75))

# The same summary for each group.
clean_data |>
  group_by(month) |>
  summarise(n = n(), mean = mean(ozone), median = median(ozone),
            sd = sd(ozone))

# Counts and proportions of a categorical variable.
clean_data |>
  count(month) |>
  mutate(prop = n / sum(n))

# Correlation between two quantitative variables...
cor(clean_data$ozone, clean_data$temp_f)
# ...and between every pair (look for |r| > 0.8 among predictors).
clean_data |>
  select(where(is.numeric)) |>
  cor() |>
  round(2)


# ------------------------------------------------------------
# 4. Plots (B)
# ------------------------------------------------------------
# Label every axis with the variable and its units.

# Distribution of one quantitative variable.
ggplot(clean_data, aes(x = ozone)) +
  geom_histogram(bins = 20) +
  labs(x = "Ozone (ppb)", y = "Number of days")

# Counts of a categorical variable.
ggplot(clean_data, aes(x = month)) +
  geom_bar() +
  labs(x = "Month", y = "Number of days")

# Response against a quantitative predictor, with a straight line (lm)
# and a smooth curve (loess): if they disagree, the pattern is curved.
ggplot(clean_data, aes(x = temp_f, y = ozone)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Maximum temperature (F)", y = "Ozone (ppb)")

# Response across groups.
ggplot(clean_data, aes(x = month, y = ozone)) +
  geom_boxplot() +
  labs(x = "Month", y = "Ozone (ppb)")

# Does the relationship differ across groups? Color, or one panel per group.
ggplot(clean_data, aes(x = temp_f, y = ozone, color = month)) +
  geom_point() +
  labs(x = "Maximum temperature (F)", y = "Ozone (ppb)", color = "Month")
ggplot(clean_data, aes(x = temp_f, y = ozone)) +
  geom_point() +
  facet_wrap(~ month) +
  labs(x = "Maximum temperature (F)", y = "Ozone (ppb)")

# Every quantitative variable against every other, in one picture.
pairs(select(clean_data, ozone, solar, wind, temp_f))

# Save a plot to include in the Google Doc (optional).
# ggsave("ozone_vs_temp.png", width = 6, height = 4)
