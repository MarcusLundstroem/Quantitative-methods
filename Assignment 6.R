###############################
#########Assignment 6##########
###############################

# Load libraries and data
library(tidyverse)
library(ggrepel)
library(haven)
library(modelsummary)
library(AER)

data <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 6/Acemoglu.dta")

# Assignment questions

base_data <- data %>%
  filter(baseco == 1)


ggplot(base_data, aes(x = avexpr, y = logpgp95, label = shortnam)) +
  geom_point() +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Protection Against Expropriation vs. Log GDP per Capita (1995)",
    x = "Average Expropriation Risk (1985–1995)",
    y = "Log GDP per Capita (1995)"
  ) +
  theme_minimal()


ggplot(base_data, aes(x = logem4, y = avexpr, label = shortnam)) +
  geom_point() +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Settler Mortality vs. Average expropriation Risk",
    x = "Log of Settler Mortality",
    y = "Average Expropriation Risk (1985–1995)"
  ) +
  theme_minimal()


ggplot(base_data, aes(x = logem4, y = logpgp95, label = shortnam)) +
  geom_point() +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Settler Mortality vs. Log GDP per Capita (1995)",
    x = "Log of Settler Mortality",
    y = "Log GDP per Capita (1995)"
  ) +
  theme_minimal()

iv_model <- ivreg(logpgp95 ~ avexpr | logem4, data = base_data)
summary(iv_model)


first_stage <- lm(avexpr ~ logem4, data = base_data)
summary(first_stage)

base_data$avexpr_hat <- fitted(first_stage)

second_stage <- lm(logpgp95 ~ avexpr_hat, data = base_data)
summary(second_stage)

summary(iv_model, diagnostics = TRUE)