# Load libraries and data
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(modelsummary)
library(haven)
qog_std_ts_jan22_1_ <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 3/qog_std_ts_jan22 (1).dta")

qog_data <- qog_std_ts_jan22_1_
remove(qog_std_ts_jan22_1_)

#####
##1##
#####
qog_data$year <- as.numeric(as.character(qog_data$year))
pdata <- pdata.frame(qog_data, index = c("ccode", "year"))

pdata$year <- as.numeric(as.character(pdata$year))

# Check for duplicates
anyDuplicated(paste(pdata$ccode, pdata$year))

# Check if panel is balanced
is.pbalanced(pdata)

# Check range of years
range(pdata$year, na.rm = TRUE)

length(unique(pdata$ccode))   # number of countries
length(unique(pdata$year))    # number of years
nrow(pdata)                   # number of observations

#####
##2##
#####

gfs_data <- pdata %>%
  filter(!is.na(gfs_envr))

gfs_panel_info <- tibble(
  Metric = c("Number of Countries (Cases)", "Number of Years", "Total Observations"),
  Value = c(length(unique(gfs_data$ccode)),
            length(unique(gfs_data$year)),
            nrow(gfs_data))
)

datasummary_df(gfs_panel_info, title = "Panel Structure Summary: gfs_envr")
datasummary_skim(gfs_data %>% select(gfs_envr), title = "Variable Summary: gfs_envr")

fh_data <- pdata %>%
  filter(!is.na(fh_polity2))

fh_panel_info <- tibble(
  Metric = c("Number of Countries (Cases)", "Number of Years", "Total Observations"),
  Value = c(length(unique(fh_data$ccode)),
            length(unique(fh_data$year)),
            nrow(fh_data))
)

datasummary_df(fh_panel_info, title = "Panel Structure Summary: fh_polity2")
datasummary_skim(fh_data %>% select(fh_polity2), title = "Variable Summary: fh_polity2")

#####
##3##
#####
ggplot(gfs_data, aes(x = year, y = gfs_envr, group = ccode)) +
  geom_line(color = "gray50", alpha = 0.4, linewidth = 0.3) +
  scale_x_continuous(breaks = seq(min(gfs_data$year), max(gfs_data$year), by = 5)) +
  coord_cartesian(ylim = c(0, max(gfs_data$gfs_envr, na.rm = TRUE))) +
  labs(title = "Environmental Protection Expenditure (gfs_envr) Over Time",
       subtitle = "One line per country",
       x = "Year", y = "gfs_envr (% of government spending)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(fh_data, aes(x = year, y = fh_polity2, group = ccode)) +
  geom_line(color = "gray50", alpha = 0.4, linewidth = 0.3) +  # thinner, lighter lines
  scale_x_continuous(breaks = seq(min(fh_data$year), max(fh_data$year), by = 5)) +  # fewer year labels
  coord_cartesian(ylim = c(0, 10)) +  # lock y-axis to known scale
  labs(title = "Democracy Score (fh_polity2) Over Time",
       subtitle = "One line per country",
       x = "Year", y = "Democracy Score (0–10)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Faceting

# Step 1: Create long format from pdata
pdata_long <- pdata %>%
  select(cname, year, fh_polity2, gfs_envr) %>%
  pivot_longer(cols = c(fh_polity2, gfs_envr),
               names_to = "variable",
               values_to = "value") %>%
  drop_na()

# Step 2: Sample 25 countries and filter
set.seed(123)  # Reproducible sample
sample_countries <- sample(unique(pdata_long$cname), 25)

pdata_long_sample <- pdata_long %>%
  filter(cname %in% sample_countries)

# Step 3: Plot
ggplot(pdata_long_sample, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~ cname, scales = "free_y") +
  labs(
    title = "Democracy and Environmental Spending (Sample of 25 Countries)",
    x = "Year",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#####
##4##
#####
ols_model <- lm(gfs_envr ~ fh_polity2, data = pdata)
summary(ols_model)

ols_model_countrydummy <- lm(gfs_envr ~ fh_polity2 + factor(ccode), data = pdata)
summary(ols_model_countrydummy)

modelsummary(
  list(
    "OLS without Country Dummies" = ols_model,
    "OLS with Country Dummies" = ols_model_countrydummy
  ),
  title = "Comparison of OLS Models: Democracy and Environmental Protection",
  coef_omit = "factor\\(ccode\\)",  # omit country dummies for clarity
  stars = TRUE  # add significance stars
)

#####
##5##
#####
fe_model <- plm(gfs_envr ~ fh_polity2, data = pdata, model = "within")
summary(fe_model)

modelsummary(
  list(
    "OLS (no country dummies)" = ols_model,
    "OLS (with country dummies)" = ols_model_countrydummy,
    "Fixed Effects (within)" = fe_model
  ),
  title = "Comparison of OLS and Fixed Effects Models",
  coef_omit = "factor\\(ccode\\)",
  stars = TRUE
)

#####
##6##
#####
re_model <- plm(gfs_envr ~ fh_polity2, data = pdata, model = "random")
summary(re_model)

modelsummary(
  list(
    "OLS (no dummies)" = ols_model,
    "OLS (country dummies)" = ols_model_countrydummy,
    "Fixed Effects (FE)" = fe_model,
    "Random Effects (RE)" = re_model
  ),
  title = "Comparison of OLS, Fixed Effects, and Random Effects Models",
  coef_omit = "factor\\(ccode\\)",
  stars = TRUE
)
#####
##7##
#####

fe_model_control <- plm(gfs_envr ~ fh_polity2 + wel_evi, data = pdata, model = "within")
summary(fe_model_control)

re_model_control <- plm(gfs_envr ~ fh_polity2 + wel_evi, data = pdata, model = "random")
summary(re_model_control)

hausman_test <- phtest(fe_model_control, re_model_control)
print(hausman_test)

#####
##8##
#####
re_model_control <- plm(gfs_envr ~ fh_polity2 + wel_evi, data = pdata, model = "random")
summary(re_model_control)

modelsummary(
  list(
    "FE model" = fe_model_control,
    "RE model" = re_model_control
  ),
  stars = c('***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1),
  title = "Model Summary with Custom Significance Codes"
)

#####
##9##
#####

# Clustered SEs for each
re_clustse <- vcovHC(re_model, type = "HC1", cluster = "group")
re_control_clustse   <- vcovHC(re_model_control,   type = "HC1", cluster = "group")

modelsummary(
  list(
    "RE (no control, default SEs)" = re_model,
    "RE (no control, clustered SEs)" = re_model,
    "RE (with wel_evi, default SEs)" = re_model_control,
    "RE (with wel_evi, clustered SEs)" = re_model_control
  ),
  vcov = list(
    "RE (no control, default SEs)" = NULL,
    "RE (no control, clustered SEs)" = re_clustse,
    "RE (with wel_evi, default SEs)" = NULL,
    "RE (with wel_evi, clustered SEs)" = re_control_clustse
  ),
  stars = c('***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1),
  title = "Random Effects Models with and without Clustered Standard Errors"
)