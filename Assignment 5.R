###############################
#########Assignment 5##########
###############################

# Load libraries and data
library(tidyverse)
library(haven)
library(modelsummary)

data <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 5/Washington.dta")


# replication of study
binary_regression <- lm(nowtot ~ ngirls, data = data)
summary(binary_regression)

regression_some_controls <- lm(nowtot ~ ngirls + factor(totchi), data = data)
summary(regression_some_controls)

regression_all_controls <- lm(nowtot ~ ngirls + female + white + repub +
                                age + I(age^2) + srvlng + I(srvlng^2) + demvote +
                                factor(rgroup) +
                                factor(totchi) + factor(region), data = data)

summary(regression_all_controls)

# summary of all models

# Optional: rename variables for clarity
variable_labels <- c(
  ngirls = "Number of daughters",
  female = "Female legislator",
  white = "White",
  repub = "Republican",
  age = "Age",
  `I(age^2)` = "Age squared",
  srvlng = "Years in Congress",
  `I(srvlng^2)` = "Years in Congress squared",
  demvote = "Democratic presidential vote share"
)

modelsummary(
  list(
    "Model 1: No controls" = binary_regression,
    "Model 2: Child fixed effects" = regression_some_controls,
    "Model 3: Full model (Table 2)" = regression_all_controls
  ),
  stars = TRUE,
  coef_map = variable_labels,
  fmt = 2,  # 2 decimal places
  title = "Effect of Number of Daughters on NOW Voting Score",
  coef_omit = "factor\\(totchi\\)|factor\\(region\\)|factor\\(rgroup\\)|Intercept",
  gof_omit = "IC|Log|F|Adj|RMSE"
)


# Bonus visualization
ggplot(data, aes(x = age, y = nowtot)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "darkred", linewidth = 1.2) +
  labs(
    title = "Squared Fit: NOW Score vs age of congress members",
    x = "Age",
    y = "NOW Voting Score"
  ) +
  theme_minimal()
