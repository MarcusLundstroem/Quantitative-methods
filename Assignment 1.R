library(tidyverse)
library(modelsummary)
library(haven)
library(hrbrthemes)
qog_std_cs_jan23_stata14 <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 1/qog_std_cs_jan23_stata14.dta")
View(qog_std_cs_jan23_stata14)

qogdata <- qog_std_cs_jan23_stata14
remove(qog_std_cs_jan23_stata14)

# 1. Causal hypothesis

# Variables (wbgi_pvn, vdem_polyarchy)

summary(qogdata$vdem_polyarchy)
summary(qogdata$wbgi_pvn)
summary(qogdata$al_ethnic2000)
summary(qogdata$al_religion2000)
summary(qogdata$al_wdi_acel)

# 2. Bivariate regression
Bivariate_model <- lm(wbgi_pvn ~ vdem_polyarchy, data = qogdata)
summary(Bivariate_model)

# 3. Answered in word

# 4. Multivariate regressions and regression outputs
qogdata_filtered <- qogdata %>%
  select(cname, wbgi_pvn, vdem_polyarchy, wdi_acel, al_ethnic2000, al_religion2000)

Multivariate_model1 <- lm(wbgi_pvn ~ vdem_polyarchy + wdi_acel, data = qogdata)
summary(Multivariate_model1)

Multivariate_model2 <- lm(wbgi_pvn ~ vdem_polyarchy + wdi_acel + al_ethnic2000, data = qogdata)
summary(Multivariate_model2)

Multivariate_model3 <- lm(wbgi_pvn ~ vdem_polyarchy + wdi_acel + al_ethnic2000 + al_religion2000, data = qogdata)
summary(Multivariate_model3)


modelsummary(
  list("Model 1" = Bivariate_model, 
       "Model 2" = Multivariate_model1, 
       "Model 3" = Multivariate_model2,
       "Model 4" = Multivariate_model3),
  coef_map = c(
    "vdem_polyarchy" = "Polyarchy (V-Dem)",
    "wdi_acel" = "Electricity Access (%)",
    "al_ethnic2000" = "Ethnic Fractionalization",
    "al_religion2000" = "Religious Fractionalization"
  ),
  stars = TRUE,
  gof_omit = "IC|Log|F",
  title = "Dependent Variable: Political stability and abscence of terrorism (World Bank Governance Indicators)",
  notes = "Standard errors in parentheses. * p < 0.05, ** p < 0.01, *** p < 0.001"
)


# 5. Visualizations
ggplot(qogdata_filtered, aes(x = vdem_polyarchy, y = wbgi_pvn)) +
  geom_point(color = "black", size = 1.7, alpha = 1) +
  geom_smooth(method = "lm", color = "red", fill="#69b3a2", se = TRUE, linetype = "solid") +
  labs(
    title = "Relationship between Polyarchy and Political Stability",
    subtitle = "Linear regression with 95% confidence interval",
    x = "Polyarchy (V-Dem)",
    y = "Political Stability"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 13),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

ggplot(qogdata_filtered, aes(x = wbgi_pvn)) +
  geom_histogram(bins = 30, fill = "grey60", color = "white", alpha = 0.9) +
  labs(
    title = "Distribution of Political Stability",
    x = "Political Stability (WB)",
    y = "Number of Countries"
  ) +
  theme_classic(base_size = 12)

ggplot(qogdata_filtered, aes(x = vdem_polyarchy)) +
  geom_histogram(bins = 30, fill = "grey60", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Polyarchy (V-Dem)",
    x = "Polyarchy Score",
    y = "Number of Countries"
  ) +
  theme_classic(base_size = 12)
