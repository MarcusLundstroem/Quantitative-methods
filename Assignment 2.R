# Load libraries and data
library(tidyverse)
library(modelsummary)
library(interactions)
library(stargazer)
library(haven)
qog_std_cs_jan23_stata14 <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 2/qog_std_cs_jan23_stata14.dta")
View(qog_std_cs_jan23_stata14)

qog_data <- qog_std_cs_jan23_stata14
remove(qog_std_cs_jan23_stata14)

#######
###1###
#######
df <- qog_data %>%
  filter(!is.na(wdi_co2) & !is.na(wdi_gdpcapcur)) %>%
  select(cname, wdi_co2, wdi_gdpcapcur, wdi_pop, bmr_dem)

ggplot(df, aes(x = wdi_gdpcapcur, y = wdi_co2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "CO2 Emissions vs GDP per Capita",
       x = "GDP per Capita (Current USD)",
       y = "CO2 Emissions")

# Create GDP bins
df <- df %>%
  mutate(gdp_bin = cut(wdi_gdpcapcur, breaks = 20))

# Calculate bin means
binned_df <- df %>%
  group_by(gdp_bin) %>%
  summarise(mean_gdp = mean(wdi_gdpcapcur, na.rm = TRUE),
            mean_co2 = mean(wdi_co2, na.rm = TRUE))

# Plot
ggplot(binned_df, aes(x = mean_gdp, y = mean_co2)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Binned Scatterplot: CO2 vs GDP",
       x = "Mean GDP per Capita (per bin)",
       y = "Mean CO2 Emissions (per bin)")

df$log_gdp <- log(df$wdi_gdpcapcur)

#######
###2###
#######


model_log <- lm(wdi_co2 ~ log_gdp, data = df)
summary(model_log)

model_poly <- lm(wdi_co2 ~ poly(wdi_gdpcapcur, 2), data = df)
summary(model_poly)

modelsummary(
  list("Log GDP model" = model_log, "Polynomial model" = model_poly),
  statistic = "std.error",
  stars = TRUE,             
  gof_omit = "IC|Log.Lik",   
  output = "html"        
)

ggplot(df, aes(x = wdi_gdpcapcur, y = wdi_co2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "darkred") +
  labs(
    title = "Polynomial Fit: CO2 Emissions vs GDP per Capita",
    x = "GDP per Capita (Current USD)",
    y = "CO2 Emissions"
  )

ggplot(df, aes(x = log_gdp, y = wdi_co2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "darkgreen") +
  labs(
    title = "CO2 Emissions vs GDP per Capita (Log Model)",
    x = "GDP per Capita (Current USD)",
    y = "CO2 Emissions",
  )

#######
###3###
#######

# Extract DFBETA for the GDP variable
df$dfbeta_gdp <- dfbeta(model_log)


df_influential <- df %>%
  arrange(desc(abs(dfbeta_gdp))) %>%
  select(cname, wdi_gdpcapcur, wdi_co2, dfbeta_gdp) %>%
  head(5)

print(df_influential)

df_no_outlier <- df %>% filter(cname != "Qatar")

model_log_no_outlier <- lm(wdi_co2 ~ log_gdp, data = df_no_outlier)

modelsummary(
  list("Log GDP model" = model_log, "Log GDP model (excl Qatar)" = model_log_no_outlier),
  statistic = "std.error",
  stars = TRUE,             
  gof_omit = "IC|Log.Lik",   
  output = "html"        
)

#######
###4###
#######

# Define list of influential outliers
excluded_countries <- c("Qatar", 
                        "United Arab Emirates (the)", 
                        "Bahrain", 
                        "Kuwait", 
                        "Liechtenstein")

# Filter dataset
qog_data_filtered <- qog_data %>%
  filter(!cname %in% excluded_countries) %>%     # remove 5 outliers
  filter(wdi_pop >= 100000) %>%                  # remove countries < 100,000 people
  filter(!is.na(wdi_gdpcapcur) & !is.na(wdi_co2))  # make sure variables are not missing

model_log_filtered <- lm(wdi_co2 ~ log(wdi_gdpcapcur), data = qog_data_filtered)

modelsummary(
  list("Original Model" = model_log,
       "Filtered (No outliers + Pop ≥ 100k)" = model_log_filtered),
  statistic = "std.error",
  stars = TRUE
)

#######
###5###
#######

# Separate the data
df_dem <- df %>% filter(bmr_dem == 1)
df_non_dem <- df %>% filter(bmr_dem == 0)

# Run separate regressions
model_dem <- lm(wdi_co2 ~ log(wdi_gdpcapcur), data = df_dem)
model_non_dem <- lm(wdi_co2 ~ log(wdi_gdpcapcur), data = df_non_dem)

# Output
modelsummary(
  list("Democracies" = model_dem,
       "Non-Democracies" = model_non_dem),
  statistic = "std.error",
  stars = TRUE
)

# Run an interaction model
model_interaction <- lm(wdi_co2 ~ log(wdi_gdpcapcur) * bmr_dem, data = df)
summary(model_interaction)

modelsummary(
  list("Interaction Model" = model_interaction),
  statistic = "std.error",  
  stars = TRUE              
)

model_interaction_fixed_for_plotting <- lm(wdi_co2 ~ log_gdp * bmr_dem, data = df)

interact_plot(model_interaction_fixed_for_plotting,
              pred = log_gdp,
              modx = bmr_dem,
              modx.labels = c("Non-Democracy", "Democracy"),
              interval = TRUE,
              plot.points = TRUE,
              x.label = "Log GDP per Capita",
              y.label = "CO2 Emissions",
              main.title = "Interaction: GDP × Democracy on CO2 Emissions")


#######
###6###
#######

# Answered in word
