#--------------------------------#
#   1. SETUP: LOAD LIBRARIES     #
#--------------------------------#
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(stats)
library(gridExtra)
library(lmtest)
library(MASS)
library(FactoMineR)
library(e1071)  # For skewness calculation

#--------------------------------#
#   2. LOADING & CLEANING DATA   #
#--------------------------------#

# Load Dataset
df <- read_excel("Merged_CO2_OilPrice.xlsx")

# Rename Columns for Clarity
df <- rename(df, 'Global_Crude_OilPrice(€)' = 'DCOILBRENTEU')
df <- rename(df, 'ComplianceCost(€)' = 'ComplianceCost(€).1')
install.packages("psych")
library(psych)
describe(df)
# Drop Unnecessary Columns
df <- df %>% dplyr::select(-c(Date, Year, Month))

# Load necessary library
library(dplyr)


# Check for Duplicates
if (sum(duplicated(df)) > 0) {
  cat("\nThere are duplicate values present.\n")
} else {
  cat("\nThere are no duplicate values present.\n")
}

# Check for Missing Values
sapply(df, function(x) sum(is.na(x))) %>% sort(decreasing = TRUE)

#-------------------------------------------#
#   3. EXPLORATORY DATA ANALYSIS   #
#-------------------------------------------#



numeric_cols <- names(df)[sapply(df, is.numeric)]
num_plots <- length(numeric_cols)

if (num_plots > 1) {
  rows <- ceiling(sqrt(num_plots))
  cols <- ceiling(num_plots / rows)
} else {
  rows <- 1
  cols <- 1
}

par(mfrow = c(rows, cols))  # Adjust layout dynamically
par(mar = c(3, 3, 2, 1))  # Reduce margins

### BEFORE OUTLIER REMOVAL DATA ANALYSIS

par(mfrow = c(rows, cols))  # Adjust layout dynamically
par(mar = c(3, 3, 2, 1))  # Reduce margins

for (col in numeric_cols) {
  boxplot(df[[col]], main = paste("Boxplot of", col), col = "orange", border = "black")
}

# Scatter plots of each independent variable with CO2 Emissions before outlier removal

par(mfrow = c(3, 1))  # Adjust layout for 6 plots

plot(df$`ProductionVolume(Tonnes)`, df$`CO₂Emissions(tonnes)`, 
     main = "Production Volume vs CO₂ Emissions", xlab = "Production Volume", ylab = "CO₂ Emissions", col = "blue")

plot(df$`EnergyConsumption(GJ/tcs)`, df$`CO₂Emissions(tonnes)`, 
     main = "Energy Consumption vs CO₂ Emissions", xlab = "Energy Consumption", ylab = "CO₂ Emissions", col = "red")

plot(df$`RawMaterialComposition(%RecycledorUtilized)`, df$`CO₂Emissions(tonnes)`, 
     main = "Raw Material Composition vs CO₂ Emissions", xlab = "Raw Material Composition", ylab = "CO₂ Emissions", col = "purple")

par(mfrow = c(3, 1))  # Adjust layout for 6 plots
plot(df$`GlobalCO₂EmissionsIntensity(tCO₂/tcs)`, df$`CO₂Emissions(tonnes)`, 
     main = "Global CO₂ EmissionIntensity vs CO₂ Emissions", xlab = "Energy Consumption Residual", ylab = "CO₂ Emissions", col = "pink")

plot(df$`ComplianceCost(€)`, df$`CO₂Emissions(tonnes)`, 
     main = "Compliance Cost vs CO₂ Emissions", xlab = "Compliance Cost", ylab = "CO₂ Emissions", col = "green")

plot(df$`Global_Crude_OilPrice(€)`, df$`CO₂Emissions(tonnes)`, 
     main = "Crude Oil Price vs CO₂ Emissions", xlab = "Crude Oil Price", ylab = "CO₂ Emissions", col = "orange")



par(mfrow = c(3, 2))  # Adjusting layout for 6 histograms

hist(df$`ProductionVolume(Tonnes)`, 
     main = "Histogram of Production Volume", xlab = "Production Volume", col = "blue", border = "black")

hist(df$`EnergyConsumption(GJ/tcs)`, 
     main = "Histogram of Energy Consumption", xlab = "Energy Consumption", col = "red", border = "black")

hist(df$`ComplianceCost(€)`, 
     main = "Histogram of Compliance Cost", xlab = "Compliance Cost", col = "green", border = "black")

hist(df$`RawMaterialComposition(%RecycledorUtilized)`, 
     main = "Histogram of Raw Material Composition", xlab = "Raw Material Composition", col = "purple", border = "black")

hist(df$`Global_Crude_OilPrice(€)`, 
     main = "Histogram of Crude Oil Price", xlab = "Crude Oil Price", col = "orange", border = "black")

hist(df$`GlobalCO₂EmissionsIntensity(tCO₂/tcs)`, 
     main = "Histogram of Global CO₂ Emission Intensity", xlab = "CO₂ Emissions", col = "cyan", border = "black")




#-------------------------------------------#
#   3. OUTLIER DETECTION & REMOVAL (IQR)   #
#-------------------------------------------#
remove_outliers_iqr <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df_clean <- df
  
  for (col in names(df)[num_cols]) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    df_clean <- df_clean[df_clean[[col]] >= lower_bound & df_clean[[col]] <= upper_bound, ]
  }
  return(df_clean)
}

df_no_outliers <- remove_outliers_iqr(df)

par(mfrow = c(rows, cols))  # Adjust layout dynamically
par(mar = c(3, 3, 2, 1))  # Reduce margins

for (col in numeric_cols) {
  boxplot(df_no_outliers[[col]], main = paste("Boxplot of", col), col = "orange", border = "black")
}
# Scatter plots of each independent variable with Share_Price after outlier removal

par(mfrow = c(3, 1))  # Adjust layout for 6 plots

plot(df_no_outliers$`ProductionVolume(Tonnes)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Production Volume vs CO₂ Emissions", xlab = "Production Volume", ylab = "CO₂ Emissions", col = "blue")

plot(df_no_outliers$`EnergyConsumption(GJ/tcs)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Energy Consumption vs CO₂ Emissions", xlab = "Energy Consumption", ylab = "CO₂ Emissions", col = "red")


plot(df_no_outliers$`RawMaterialComposition(%RecycledorUtilized)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Raw Material Composition vs CO₂ Emissions", xlab = "Raw Material Composition", ylab = "CO₂ Emissions", col = "purple")

par(mfrow = c(3, 1))  # Adjust layout for 6 plots
plot(df_no_outliers$`GlobalCO₂EmissionsIntensity(tCO₂/tcs)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Global CO₂ EmissionIntensity vs CO₂ Emissions", xlab = "Energy Consumption Residual", ylab = "CO₂ Emissions", col = "pink")

plot(df_no_outliers$`ComplianceCost(€)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Compliance Cost vs CO₂ Emissions", xlab = "Compliance Cost", ylab = "CO₂ Emissions", col = "green")


plot(df_no_outliers$`Global_Crude_OilPrice(€)`, df_no_outliers$`CO₂Emissions(tonnes)`, 
     main = "Crude Oil Price vs CO₂ Emissions", xlab = "Crude Oil Price", ylab = "CO₂ Emissions", col = "orange")



par(mfrow = c(3, 1))  # Adjusting layout for 6 histograms

hist(df_no_outliers$`ProductionVolume(Tonnes)`, 
     main = "Histogram of Production Volume", xlab = "Production Volume", col = "blue", border = "black")

hist(df_no_outliers$`EnergyConsumption(GJ/tcs)`, 
     main = "Histogram of Energy Consumption", xlab = "Energy Consumption", col = "red", border = "black")

hist(df_no_outliers$`RawMaterialComposition(%RecycledorUtilized)`, 
     main = "Histogram of Raw Material Composition", xlab = "Raw Material Composition", col = "purple", border = "black")

par(mfrow = c(3, 1))  # Adjusting layout for 6 histograms
hist(df_no_outliers$`ComplianceCost(€)`, 
     main = "Histogram of Compliance Cost", xlab = "Compliance Cost", col = "green", border = "black")

hist(df_no_outliers$`Global_Crude_OilPrice(€)`, 
     main = "Histogram of Crude Oil Price", xlab = "Crude Oil Price", col = "orange", border = "black")

hist(df_no_outliers$`GlobalCO₂EmissionsIntensity(tCO₂/tcs)`, 
     main = "Histogram of Global CO₂ Emission Intensity", xlab = "CO₂ Emissions", col = "cyan", border = "black")



cat("Original dataset size:", nrow(df), "\n")
cat("Cleaned dataset size:", nrow(df_no_outliers), "\n")
cat("Number of rows removed:", nrow(df) - nrow(df_no_outliers), "\n")

library(reshape2)  # For melting correlation and covariance matrices


#-----------------------------------#
#   5. SKEWNESS CHECK & CORRECTION  #
#-----------------------------------#

# Compute skewness for numeric columns
skew_values <- sapply(df_no_outliers, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)

# Identify variables with high skewness (|skewness| > 1)
skewed_vars <- names(skew_values[abs(skew_values) > 1])
print("Highly skewed variables:")
print(skewed_vars)

# Apply Log Transformation for Right-Skewed Data
df_transformed <- df_no_outliers
for (var in skewed_vars) {
  df_transformed[[var]] <- log(df_transformed[[var]])  # log(1 + x) to handle zero values
}


# Re-check skewness after transformation
skew_values_transformed <- sapply(df_transformed, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)
print("Skewness after transformation:")
print(skew_values_transformed)

#------------------------------------#
#   5. MULTICOLLINEARITY CHECK & FIX #
#------------------------------------#

# Checking Multicollinearity (VIF)
# Load necessary library
library(car)

# Assuming df_transformed is your dataframe and CO₂Emissions(tonnes) is your target variable
# Prepare the dataframe excluding the target variable
X <- df_transformed[, !names(df_transformed) %in% c("CO₂Emissions(tonnes)")]

# Fit a linear model using all predictors
model <- lm(`CO₂Emissions(tonnes)` ~ ., data = df_transformed)

# Calculate VIF
vif_values <- vif(model)

# Create a data frame to print the VIF for each variable
vif_data <- data.frame(Variable = names(vif_values), VIF = vif_values)
print(vif_data)


# Handling High Multicollinearity in `EnergyConsumption(GJ/tcs)` via Residualization
residual_model <- lm(`EnergyConsumption(GJ/tcs)` ~ `GlobalCO₂EmissionsIntensity(tCO₂/tcs)`, data = df_transformed)
df_transformed$EnergyConsumption_resid <- residuals(residual_model)

#--------------------------------#
#   6. MODEL IMPLEMENTATION     #
#--------------------------------#

# Fit Linear Regression Model (OLS)
model_ols <- lm(log(`CO₂Emissions(tonnes)`) ~ `ProductionVolume(Tonnes)` + `EnergyConsumption_resid` + 
                  `ComplianceCost(€)` + `RawMaterialComposition(%RecycledorUtilized)` + `Global_Crude_OilPrice(€)`, 
                data = df_transformed)

summary(model_ols)
vif(model_ols)

# Residuals vs. Fitted Plot
ggplot(data.frame(Fitted = fitted(model_ols), Residuals = residuals(model_ols)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals vs. Fitted')

# Breusch-Pagan Test for Heteroscedasticity
bptest(model_ols)

# Normality Check (Shapiro-Wilk & Q-Q Plot)
shapiro.test(residuals(model_ols))
qqnorm(residuals(model_ols))
qqline(residuals(model_ols), col = "red")

# Linearity Test Plot
ggplot(data.frame(Fitted = fitted(model_ols), Residuals = residuals(model_ols)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted with Loess Smoothing")


length(fitted(model_ols))
length(residuals(model_ols))
nrow(df_transformed)

#----------------------------------#
#   7. FIXING HETEROSCEDASTICITY  #
#----------------------------------#

# Applying Weighted Least Squares (WLS)
weights <- 1 / fitted(model_ols)^2  

model_wls <- lm(log(`CO₂Emissions(tonnes)`) ~ `ProductionVolume(Tonnes)` + `EnergyConsumption_resid` + 
                  `ComplianceCost(€)` + `RawMaterialComposition(%RecycledorUtilized)` + `Global_Crude_OilPrice(€)`, 
                data = df_transformed, weights = weights)

summary(model_wls)
vif(model_wls)

# Residuals vs. Fitted Plot
ggplot(data.frame(Fitted = fitted(model_wls), Residuals = residuals(model_wls)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals vs. Fitted')

# Breusch-Pagan Test for Heteroscedasticity
bptest(model_wls)

# Normality Check (Shapiro-Wilk & Q-Q Plot)
shapiro.test(residuals(model_wls))
qqnorm(residuals(model_wls))
qqline(residuals(model_wls), col = "red")

# Linearity Test Plot
ggplot(df_transformed, aes(x = fitted(model_wls), y = residuals(model_wls))) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted with Loess Smoothing")


#----------------------------------#
#   8. FINAL MODEL WITH POLYNOMIAL TERMS #
#----------------------------------#

# Adding Polynomial Terms for Better Fit
weights_poly <- 1 / fitted(model_wls)^2

df_transformed$ProductionVolume_sq <- df_transformed$`ProductionVolume(Tonnes)`^3
df_transformed$ComplianceCost_sq <- df_transformed$`ComplianceCost(€)`^3
df_transformed$RawMaterialComposition_sq <- df_transformed$`RawMaterialComposition(%RecycledorUtilized)`^3
#df_transformed$Global_Crude_OilPrice_sq <- df_transformed$`Global_Crude_OilPrice(€)`^3

model_poly <- lm(log(`CO₂Emissions(tonnes)`) ~  ProductionVolume_sq + log(`EnergyConsumption_resid`)
                 +ComplianceCost_sq + RawMaterialComposition_sq +`Global_Crude_OilPrice(€)`
                 , data = df_transformed,weights=weights_poly)

summary(model_poly)
vif(model_poly)

#----------------------------------#
#   9. MODEL DIAGNOSTICS & TESTS  #
#----------------------------------#

# Residuals vs. Fitted Plot
ggplot(data.frame(Fitted = fitted(model_poly), Residuals = residuals(model_poly)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals vs. Fitted')

# Breusch-Pagan Test for Heteroscedasticity
bptest(model_poly)

# Normality Check (Shapiro-Wilk & Q-Q Plot)
shapiro.test(residuals(model_poly))
qqnorm(residuals(model_poly))
qqline(residuals(model_poly), col = "red")

# Linearity Test Plot
ggplot(data.frame(Fitted = fitted(model_poly), Residuals = residuals(model_poly)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted with Loess Smoothing")


vif(model_poly)

## Plot of autocorrelation
plot(residuals(model_poly) ~ fitted(model_poly), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")





### EXTRA#####
acf(residuals(model_poly))
df_transformed$Lag_CO2 <- lag(df_transformed$`CO₂Emissions(tonnes)`, 1)
model_poly_lag <- lm(log(`CO₂Emissions(tonnes)`) ~ ProductionVolume_sq + log(EnergyConsumption_resid) + Lag_CO2+ComplianceCost_sq + `RawMaterialComposition_sq` +`Global_Crude_OilPrice(€)`, 
                     data = df_transformed, weights = weights_poly)

summary(model_poly_lag)

vif(model_poly_lag)

# Residuals vs. Fitted Plot
ggplot(data.frame(Fitted = fitted(model_poly_lag), Residuals = residuals(model_poly_lag)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals vs. Fitted')

# Breusch-Pagan Test for Heteroscedasticity
bptest(model_poly_lag)

# Normality Check (Shapiro-Wilk & Q-Q Plot)
shapiro.test(residuals(model_poly_lag))
qqnorm(residuals(model_poly_lag))
qqline(residuals(model_poly_lag), col = "red")

# Linearity Test Plot
ggplot(data.frame(Fitted = fitted(model_poly_lag), Residuals = residuals(model_poly_lag)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted with Loess Smoothing")

acf(residuals(model_poly))

## Plot of autocorrelation
plot(residuals(model_poly_lag) ~ fitted(model_poly_lag), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")