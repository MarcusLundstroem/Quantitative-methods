###############################
#########Assignment 7##########
###############################

# Load libraries and data
library(tidyverse)
library(haven)
library(modelsummary)
library(rdrobust)

data <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 7/RDD VT23 data.dta")
View(data)

#1

data <- data %>%
  filter(!is.na(dpi_gvs), !is.na(wdi_taxrev)) %>%
  mutate(running = dpi_gvs - 50)  # Centering around 50%

data <- data %>%
  mutate(treatment = ifelse(dpi_gvs >= 50, 1, 0))

model_parametric <- lm(wdi_taxrev ~ dpi_gvs + treatment, data = data)
summary(model_parametric)

#2
model_quad <- lm(wdi_taxrev ~ dpi_gvs + I(dpi_gvs^2) + treatment, data = data)
summary(model_quad)

model_cubic <- lm(wdi_taxrev ~ dpi_gvs + I(dpi_gvs^2) + I(dpi_gvs^3) + treatment, data = data)
summary(model_cubic)

#3
ggplot(data, aes(x = running, y = wdi_taxrev)) +
  geom_point(alpha = 0.5, color = "black", shape = 16, size = 2) +
  
  geom_smooth(data = subset(data, running < 0), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = subset(data, running >= 0), method = "lm", se = TRUE, color = "blue") +
  
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  
  labs(
    title = "RDD Scatter Plot with Trends at Majority Cutoff",
    x = "Vote Share Relative to Majority Threshold (Centered at 50%)",
    y = "Tax Revenue (% of GDP)"
  ) +
  theme_minimal(base_size = 13)

#4

#filter
data_bw <- data %>%
  filter(running >= -5, running <= 5)

#check diagnostics
summary(data_bw$running)
summary(data_bw$wdi_taxrev)
nrow(data_bw)

#5
model_ols_bw <- lm(wdi_taxrev ~ running + treatment, data = data_bw)
summary(model_ols_bw)

rdd_result <- rdrobust(y = data$wdi_taxrev, x = data$running)
summary(rdd_result)

# Regression outputs
modelsummary(
  list(
    "Parametric (Full Sample)" = model_parametric,
    "Quadratic (Full Sample)" = model_quad,
    "Cubic (Full Sample)" = model_cubic,
    "OLS (±5% Bandwidth)" = model_ols_bw
  ),
  statistic = "({std.error})",
  stars = TRUE,
  coef_rename = c(
    "treatment" = "Majority Government",
    "dpi_gvs" = "Vote Share",
    "I(dpi_gvs^2)" = "Vote Share²",
    "I(dpi_gvs^3)" = "Vote Share³",
    "running" = "Vote Share (Centered)"
  ),
  gof_omit = "IC|Log|Adj|F|RMSE",
  title = "Note: The non-parametric RDD estimate from rdrobust() yields a treatment effect of 11.46 (95% robust CI: [6.50, 17.89], p < 0.001). This estimate is based on an optimal bandwidth of 3.12 around the 50% vote share threshold."
)

#6
rdplot(y = data$wdi_taxrev,
       x = data$running,
       title = "Non-parametric RDD Plot: Tax Revenue vs. Vote Share",
       x.label = "Vote Share Relative to Majority Threshold (Centered at 0)",
       y.label = "Tax Revenue (% of GDP)",
       col.lines = "blue",
       col.dots = "gray40")

#7
#answered in text
