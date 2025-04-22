###############################
#########Assignment 4##########
###############################

# Load libraries and data
library(tidyverse)
library(haven)
library(fixest)
library(did)         
library(sandwich)
library(lmtest)
library(modelsummary)

Data_Assignment_DiffinDiff <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 4/Data Assignment DiffinDiff.dta")
rawdata <- Data_Assignment_DiffinDiff
remove(Data_Assignment_DiffinDiff)


#######
###1###
#######

#Answered in word

#######
###2###
#######

###
#a#
###

did_data <- rawdata %>%
  filter(time %in% c(2, 3)) %>%
  mutate(post = if_else(time == 3, 1, 0))


did_model_06_10 <- feols(sd ~ crisis * post, data = did_data)
summary(did_model_06_10)


did_model_06_10_controls <- feols(
  sd ~ crisis * post + meanage + meanage2 + education + foreignborn,
  data = did_data
)
summary(did_model_06_10_controls)

variable_labels <- c(
  "crisis" = "Crisis Intensity",
  "post" = "Post-Crisis Period",
  "crisis:post" = "DiD Interaction",
  "meanage" = "Mean Age",
  "meanage2" = "Mean Age²",
  "education" = "Post-High School Education (%)",
  "foreignborn" = "Foreign Born (%)"
)

modelsummary(
  list(
    "2006–2010 (No Controls)" = did_model_06_10,
    "2006–2010 (With Controls)" = did_model_06_10_controls
  ),
  coef_map = variable_labels,
  stars = TRUE,
  gof_omit = "IC|Log|Adj|FE"
)



###
#b#
###

placebo_data <- rawdata %>%
  filter(time %in% c(1, 2)) %>%
  mutate(post = if_else(time == 2, 1, 0))

placebo_model <- feols(sd ~ crisis * post, data = placebo_data)
summary(placebo_model)

placebo_model_controls <- feols(
  sd ~ crisis * post + meanage + meanage2 + education + foreignborn,
  data = placebo_data
)
summary(placebo_model_controls)

remove(variable_labels)

modelsummary(
  list(
    "2002–2006 (No Controls)" = placebo_model,
    "2002–2006 (With Controls)" = placebo_model_controls
  ),
  stars = TRUE,
  gof_omit = "IC|Log|Adj|FE"
)



###
#c#
###

# Answered in word

#######
#Bonus#
#######
# Split municipalities into "High" and "Low" crisis groups
data <- rawdata %>%
  mutate(crisis_group = if_else(crisis > median(crisis, na.rm = TRUE), "High Crisis", "Low Crisis"))

trend_data <- data %>%
  group_by(time, crisis_group) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE)) %>%
  ungroup()

ggplot(trend_data, aes(x = time, y = mean_sd, color = crisis_group, group = crisis_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("2002", "2006", "2010")) +
  labs(
    title = "Parallel Trends: Support for Sweden Democrats (SD)",
    x = "Election Year",
    y = "Average SD Support (%)",
    color = "Crisis Exposure"
  ) +
  theme_minimal()
