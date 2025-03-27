# load libraries
library(tidyverse)

# load data
library(haven)
qog_std_cs_jan22_1_1_ <- read_dta("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Statsvetenskapliga metoder/Data & workshop material/Assignment 1 (voluntary)/qog_std_cs_jan22 (1) (1).dta")
View(qog_std_cs_jan22_1_1_)

# rename data
qogdata <- qog_std_cs_jan22_1_1_


# Central tendency and dispersion for fh_cl (civil liberties)
# Ordinal measurement:  Civil Liberties Rating- Civil liberties allow for the freedoms of expression and belief, associational and organizational rights, rule of law, and personal autonomy without interference from the state. The more specific list of rights considered vary over the years. Countries are graded between 1 (most free) and 7 (least free).
summary(qogdata$fh_cl)
mean(qogdata$fh_cl, na.rm = TRUE)
median(qogdata$fh_cl, na.rm = TRUE)
sd(qogdata$fh_cl, na.rm = TRUE)
var(qogdata$fh_cl, na.rm = TRUE)
range(qogdata$fh_cl, na.rm = TRUE)
IQR(qogdata$fh_cl, na.rm = TRUE)


# Central tendency and dispersion for wgov_minfem (women in cabinet)
# Continious measurement of number of women in cabinet
summary(qogdata$wgov_minfem)
mean(qogdata$wgov_minfem, na.rm = TRUE)
median(qogdata$wgov_minfem, na.rm = TRUE)
sd(qogdata$wgov_minfem, na.rm = TRUE)
var(qogdata$wgov_minfem, na.rm = TRUE)
range(qogdata$wgov_minfem, na.rm = TRUE)
IQR(qogdata$wgov_minfem, na.rm = TRUE)


# Run a binary linear regression
binary_model <- lm(fh_cl ~ wgov_minfem, data = qogdata)

# View regression summary
summary(binary_model)


# before running a multiple regression, filter the data
qogdata_filtered <- qogdata %>%
  select(cname, wgov_minfem, fh_cl, vdem_polyarchy)

# Multivariate regression: Civil liberties ~ Women in cabinet + vdem_polyarchy
multivariate_model <- lm(fh_cl ~ wgov_minfem + vdem_polyarchy, data = qogdata_filtered)

# View results
summary(multivariate_model)

# d. Calculate the predicted value of civil liberties if women wgov_minfem=5

# Create a new data frame with your predictor set to 5
newdata <- data.frame(wgov_minfem = 5)

# Get the predicted value using your bivariate model
predict(binary_model, newdata = newdata)

# e. Calculate the predicted value of civil liberties if wgov_minfem=5 and wdi_gini=38
multivariate_model2 <- lm(fh_cl ~ wgov_minfem + wdi_gini, data = qogdata)
summary(multivariate_model2)

# Create a new data frame with the desired predictor values
newdata2 <- data.frame(wgov_minfem = 5, wdi_gini = 38)

# Predict civil liberties based on the model
predict(multivariate_model2, newdata = newdata2)


# f. Run a regression with civiliberties, wgov_minfem and wdi_gini, use the information Stata provides to put in all values in the Root MSE equation below.

# Residual standard error (Root MSE)
summary(multivariate_model2)$sigma

# Degrees of freedom (residual df)
summary(multivariate_model2)$df[2]

# Residual sum of squares
rss <- sum(resid(multivariate_model2)^2)

# Compute Root MSE manually (should match sigma)
sqrt(rss / summary(multivariate_model2)$df[2])
