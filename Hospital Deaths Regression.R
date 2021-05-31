#
# Andrew Disher
# Bridgewater State University
# Thesis on Covid-19 in New England
#
# TASK: Time series analysis of Massachusetts data; build an ARIMAX model to forecast deaths
#       with prevalence.
#

# Packages
library(readxl)
library(ggplot2)
library(zoo) # Rollmean and rollsum functions
library(car) # boxCox function to assess heteroschedasticity
library(forecast) # Time series functionality
library(tseries) # Stationarity hypothesis tests
library(lmtest) # Test model coefficients for significance
library(scales)
library(TSA)
library(nortest)
library(Metrics)
library(dplyr)


# Import relevant data sets
Cases_by_Test_Date <- read_excel("BSU Stuff/Undergraduate Thesis/Data/COVID19/Massachusetts-dashboard_12-06-2020/Updated Data for ARIMAX/CasesByDate (Test Date).xlsx")
Deaths_by_Date <- read_excel("BSU Stuff/Undergraduate Thesis/Data/COVID19/Massachusetts-dashboard_12-06-2020/Updated Data for ARIMAX/DateofDeath.xlsx")
Hospitalizations_by_Date <- read_excel("BSU Stuff/Undergraduate Thesis/Data/COVID19/Massachusetts-dashboard_12-06-2020/Updated Data for ARIMAX/Hospitalization from Hospitals.xlsx")
Vaccines_by_Date <- read.csv("~/BSU Stuff/Undergraduate Thesis/Data/COVID19/Massachusetts-dashboard_12-06-2020/Updated Data for ARIMAX/Vaccinations/us-daily-covid-vaccine-doses-administered.csv")

# Convert tables to data frames
Cases_by_Test_Date <- as.data.frame(Cases_by_Test_Date)
Deaths_by_Date <- as.data.frame(Deaths_by_Date)
Hospitalizations_by_Date <- as.data.frame(Hospitalizations_by_Date)


# Data Preparation --------------------------------------------------------

### Cases_by_Test_Date changes ###

# Convert Date column to class Date
Cases_by_Test_Date$Date <- as.Date(Cases_by_Test_Date$Date, format = "%Y-%m-%d")

# Remove moving average column (not needed)
Cases_by_Test_Date <- Cases_by_Test_Date[, -4]

# Rename Columns
colnames(Cases_by_Test_Date) <- c("Date", "Total_Cases", "New_Cases")


# Create a prevelance column 
# NOTE: This data set does not come with a column estimating the number of active cases. There is an estimate
#       of active cases in the Cases (Report Date) data set, however we cannot merge report date data with 
#       test date data. The criteria for active cases, as stated by a note in the Massachusetts dashboard
#       data documentation page, states that active cases relating to patients who have recieved a Covid-19
#       diagnosis within the last 21 days. 
# SOLUTION: We will calculate our own active cases column by creating a rolling sum of new cases with a span
#           of 21 days. 
Cases_by_Test_Date$Estimated_Active_Cases <- c(Cases_by_Test_Date$Total_Cases[1:20], rollsum(Cases_by_Test_Date$New_Cases, k=21))


# Prevalence Column 
Cases_by_Test_Date$Prevalence <- Cases_by_Test_Date$Estimated_Active_Cases/((6893574 - Cases_by_Test_Date$Total_Cases)/100000)


### Changes to Deaths_by_Date ###

# Convert to type date
Deaths_by_Date$`Date of Death` <- as.character(Deaths_by_Date$`Date of Death`)
Deaths_by_Date$`Date of Death` <- as.Date(Deaths_by_Date$`Date of Death`, format = "%Y-%m-%d")

# Remove unneeded columns
Deaths_by_Date <- Deaths_by_Date[, -c(4:6)]

# Rename Columns
colnames(Deaths_by_Date) <- c('Date', 'Confirmed_Deaths', 'Total_Conf_Deaths')


### Changes to Hospitalizations_by_Date ###

# Convert to type date
Hospitalizations_by_Date$Date <- as.character(Hospitalizations_by_Date$Date)
Hospitalizations_by_Date$Date <- as.Date(Hospitalizations_by_Date$Date, format = "%Y-%m-%d")


### Vaccines Data Changes ###
Vaccines_by_Date <- subset(Vaccines_by_Date, Entity == 'Massachusetts')
Vaccines_by_Date <- Vaccines_by_Date[, -c(1, 2)]

# Convert data column to type Date
Vaccines_by_Date$Day <- as.character(Vaccines_by_Date$Day)
Vaccines_by_Date$Day <- as.Date(Vaccines_by_Date$Day, format = '%Y-%m-%d')

# Rename columns
colnames(Vaccines_by_Date) <- c("Date", "New_Vaccinations")



# Create a data frame for forecast evalutaion later, and select date range that I want for my training data.
Forecast_Test_Data <- cbind(Cases_by_Test_Date[Cases_by_Test_Date$Date >= as.Date("2021-03-17", format = "%Y-%m-%d") & Cases_by_Test_Date$Date <= as.Date("2021-03-23", format = "%Y-%m-%d"), c(1, 4)], 
                            Deaths_by_Date[Deaths_by_Date$Date >= as.Date("2021-03-17", format = "%Y-%m-%d") & Deaths_by_Date$Date <= as.Date("2021-03-23", format = "%Y-%m-%d"), c(2)], 
                            Hospitalizations_by_Date[Hospitalizations_by_Date$Date >= as.Date("2021-03-17", format = "%Y-%m-%d") & Hospitalizations_by_Date$Date <= as.Date("2021-03-23", format = "%Y-%m-%d"), c(2)])

colnames(Forecast_Test_Data)[c(3, 4)] <- c('Confirmed_Deaths', 'Patients')

Training_Data <- cbind(Cases_by_Test_Date[Cases_by_Test_Date$Date >= as.Date("2020-04-05", format = "%Y-%m-%d") & Cases_by_Test_Date$Date <= as.Date("2021-03-16", format = "%Y-%m-%d"), c(1, 4)], 
                       Deaths_by_Date[Deaths_by_Date$Date >= as.Date("2020-04-05", format = "%Y-%m-%d") & Deaths_by_Date$Date <= as.Date("2021-03-16", format = "%Y-%m-%d"), c(2)], 
                       Hospitalizations_by_Date[Hospitalizations_by_Date$Date >= as.Date("2020-04-05", format = "%Y-%m-%d") & Hospitalizations_by_Date$Date <= as.Date("2021-03-16", format = "%Y-%m-%d"), c(2)])
# , Vaccines_by_Date[Vaccines_by_Date$Date >= as.Date("2020-04-05", format = "%Y-%m-%d") & Vaccines_by_Date$Date <= as.Date("2021-03-29", format = "%Y-%m-%d"), 2]

colnames(Training_Data)[c(3, 4)] <- c('Confirmed_Deaths', 'Patients')


# Exploratory Analysis ----------------------------------------------------

# Graph the active cases time series
(ggplot(data = Training_Data, aes(x = Date, y = Estimated_Active_Cases))
 + geom_line(color = "red", size = 1)
 + geom_point(color = "blue", pch = 16)
 + xlab("Date") + ylab("Estimated Active Cases")
 + ggtitle("Estimated Active Cases of CoViD-19 in Massachusetts")
 + scale_y_continuous(breaks = seq(0, 110000, by = 10000), limits = c(0, 110000), labels = function(x){paste0(x/1000, ',000')})
 + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-25", format = "%Y-%m-%d"))) 
 + theme(axis.text.x = element_text(angle=25))
 + theme_bw()
 + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# Graph the confirmed deaths time series
(ggplot(data = Training_Data, aes(x = Date, y = Confirmed_Deaths))
  + geom_line(color = "red", size = 1)
  + geom_point(color = "blue", pch = 16)
  + xlab("Date") + ylab("New Confirmed Deaths")
  + ggtitle("Daily Confirmed Deaths of CoViD-19 in Massachusetts")
  + scale_y_continuous(breaks = seq(0, 200, by = 20))
  + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-25", format = "%Y-%m-%d"))) 
  + theme(axis.text.x = element_text(angle=25))
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)

# Graph the Patients time series
(ggplot(data = Training_Data, aes(x = Date, y = Patients))
  + geom_line(color = "red", size = 1)
  + geom_point(color = "blue", pch = 16)
  + xlab("Date") + ylab("Covid-19 Patients")
  + ggtitle("CoViD-19 Patients in Massachusetts Hospitals")
  + scale_y_continuous(breaks = seq(0, 4000, by = 500), limits = c(0, 4000))
  + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-25", format = "%Y-%m-%d"))) 
  + theme(axis.text.x = element_text(angle=25))
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# Scatterplot Matrix
pairs(~ Confirmed_Deaths + Estimated_Active_Cases + Patients, data = Training_Data)

# Correlation Matrix
cor(Training_Data[, c(2:4)])

# EAC and CD = .9558002
# ICU and CD = 0.9745010
# EAC and ICU = 0.9737175

# NOTE: High correlation between predictor variables (might need to remove one or use PCA)


# Graph confirmed deaths against Estimated Active Cases
(ggplot(data = Training_Data, aes(x = Estimated_Active_Cases, y = Confirmed_Deaths))
  + geom_point(color = "blue", pch = 16)
  + xlab("Estimated Active CoViD-19 Cases") + ylab("New Confirmed Deaths")
  + ggtitle("New Confirmed Deaths vs Estimated Active CoViD-19 Cases")
  + scale_y_continuous(breaks = seq(0, 200, by = 20))
  + scale_x_continuous(breaks = seq(0, 110000, by = 10000), limits = c(0, 110000), labels = function(x){paste0(x/1000, ',000')})
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)

# Graph confirmed deaths against Patients
(ggplot(data = Training_Data, aes(x = Patients, y = Confirmed_Deaths))
  + geom_point(color = "blue", pch = 16)
  + xlab("CoViD-19 Patients") + ylab("New Confirmed Deaths")
  + ggtitle("New Confirmed Deaths vs CoViD-19 Patients")
  + scale_y_continuous(breaks = seq(0, 200, by = 20))
  + scale_x_continuous(breaks = seq(0, 4000, by = 500), limits = c(0, 4000))
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)

# Graph cases against patients
(ggplot(data = Training_Data, aes(x = Estimated_Active_Cases, y = Patients))
  + geom_point(color = "blue", pch = 16)
  + xlab("Estimated Active CoViD-19 Cases") + ylab("Covid-19 Patients")
  + ggtitle("Covid-19 Patients vs Estimated Active CoViD-19 Cases")
  + scale_y_continuous(breaks = seq(0, 4000, by = 500), limits = c(0, 4000))
  + scale_x_continuous(breaks = seq(0, 110000, by = 10000), limits = c(0, 110000), labels = function(x){paste0(x/1000, ',000')})
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# Create a Linear Regression ----------------------------------------------

# Model creation
regMod <- lm(data = Training_Data, Confirmed_Deaths ~ Patients)
summary(regMod)

# Store the residuals and fitted values
regMod_residuals <- resid(regMod)
regMod_fitted <- fitted(regMod)


# Diagnostics Plots

# Linearity and non-constant variance assumption
plot(regMod_fitted, regMod_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")

# Normality Assumption
qqnorm(regMod_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(regMod_residuals, datax = TRUE, col = "red")
hist(regMod_residuals, breaks = 15, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))

# ACF and PACF of residuals
acf(regMod_residuals, lag.max = 50, type = "correlation", main = expression('Autocorrelation Function'),
    ylab = expression('ACF'), xlab = expression('Lag, k'))
acf(regMod_residuals, lag.max = 50, type = "partial", main = expression('Partial Autocorrelation Function'),
    ylab = expression('PACF'), xlab = expression('Lag, k'))

# NOTE 1: There was non-constant variance in the residuals vs fitted values plot. 
#         Check for a box-cox lambda with a liklihood plot for Y
boxCox(Training_Data$Confirmed_Deaths + 1 ~ Training_Data$Patients, lambda = seq(-3, 3, by = 1)) 
BoxCox.lambda(Training_Data$Confirmed_Deaths + 1, method = 'guerrero')
# Choose lambda = .5 transformation
Training_Data$Conf_Deaths_BC <- BoxCox(Training_Data$Confirmed_Deaths + 1, lambda = .5)


# Graph transformed confirmed deaths against ICU
(ggplot(data = Training_Data, aes(x = Patients, y = Conf_Deaths_BC))
  + geom_point(color = "blue", pch = 16)
  + xlab("CoViD-19 Patients") + ylab("Transformed New Confirmed Deaths")
  + ggtitle("Transformed New Confirmed Deaths (lambda = .5) vs Covid-19 Patients")
  + scale_x_continuous(breaks = seq(0, 4000, by = 500), limits = c(0, 4000))
  + scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30))
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# Model creation with box cox transformation on y
regMod2 <- lm(data = Training_Data, Conf_Deaths_BC ~ Patients)
summary(regMod2)

# Store the residuals and fitted values
regMod2_residuals <- resid(regMod2)
regMod2_fitted <- fitted(regMod2)


# Diagnostics Plots

# Linearity and non-constant variance assumption
plot(regMod2_fitted, regMod2_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")

# Normality Assumption
qqnorm(regMod2_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(regMod2_residuals, datax = TRUE, col = "red")
hist(regMod2_residuals, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))

# ACF and PACF of residuals
acf(regMod2_residuals, lag.max = 50, type = "correlation", main = expression('Autocorrelation Function'),
    ylab = expression('ACF'), xlab = expression('Lag, k'))
acf(regMod2_residuals, lag.max = 50, type = "partial", main = expression('Partial Autocorrelation Function'),
    ylab = expression('PACF'), xlab = expression('Lag, k'))

# NOTE: The residuals seem to be dependent upon time. We must use arima errors model


# Diagnostic Tests --------------------------------------------------------

# Non-constant variance
ncvTest(regMod2)


# ARIMA for noise term ----------------------------------------------------

# Graph the residuals time series
(ggplot(data = Training_Data, aes(x = Date, y = regMod2_residuals))
 + geom_line(color = "red", size = 1)
 + geom_point(color = "blue", pch = 16)
 + xlab("Date") + ylab("Residuals")
 + ggtitle("Time series of Residuals for Regression Model")
 + scale_y_continuous(breaks = seq(-5, 5, 2.5), limits = c(-5, 5))
 + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-25", format = "%Y-%m-%d"))) 
 + theme_bw()
 + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# ACF and PACF of residuals
acf(regMod2_residuals, lag.max = 50, type = "correlation", main = expression('Autocorrelation Function'),
    ylab = expression('ACF'), xlab = expression('Lag, k'))
acf(regMod2_residuals, lag.max = 50, type = "partial", main = expression('Partial Autocorrelation Function'),
    ylab = expression('PACF'), xlab = expression('Lag, k'))

# Take non-seasonal difference of time series, since ACF indicates severe non-stationarity
residuals_Diff <- data.frame(Date = Training_Data$Date, Residuals = regMod2_residuals)
residuals_Diff$Residuals <- c(array(NA, dim = c(1, 1)), diff(residuals_Diff$Residuals, 1))
residuals_Diff <- residuals_Diff[-c(1:1), ]


# Graph the residuals time series
(ggplot(data = residuals_Diff, aes(x = Date, y = Residuals))
  + geom_line(color = "red", size = 1)
  + geom_point(color = "blue", pch = 16)
  + xlab("Date") + ylab("Residuals")
  + ggtitle("Differenced Time series of Residuals for Regression Model")
  + scale_y_continuous(breaks = seq(-5, 5, 2.5), limits = c(-5, 5))
  + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-01", format = "%Y-%m-%d"), as.Date("2021-03-25", format = "%Y-%m-%d"))) 
  + theme_bw()
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)

# ACF and PACF of residuals
acf(residuals_Diff$Residuals, lag.max = 50, type = "correlation", main = expression('ACF for Differenced Regression Errors'),
    ylab = expression('ACF'), xlab = expression('Lag, k'))
acf(residuals_Diff$Residuals, lag.max = 50, type = "partial", main = expression('PACF for Differenced Regression Errors'),
    ylab = expression('PACF'), xlab = expression('Lag, k'))


# Create an ARIMA(0,0,0)x(0,0,0) model.  ----------------------------------
arima_resid_mod <- stats::arima(regMod2_residuals, order = c(0, 1, 0)) 
summary(arima_resid_mod)

# Fiding BIC.
AIC(arima_resid_mod, k = log(length(regMod2_residuals)))

# Results:
# (AIC = 1275.45)
# (BIC = 1279.298)


# Create an ARIMA(0,1,1)x(0,0,0) model.  ---------------------------------- # Best for Performance
arima_resid_mod <- stats::arima(regMod2_residuals, order = c(0, 1, 1)) 
summary(arima_resid_mod)

# Fiding BIC.
AIC(arima_resid_mod, k = log(length(regMod2_residuals)))

# Results:
# (AIC = 1085.16)
# (BIC = 1092.852)
# Check the p-values of the model coefficients to see if they are significant. 
coeftest(arima_resid_mod, df = 346-1-1)



# Diagnostics 

# Inverse roots
autoplot(arima_resid_mod)

# Store the residuals and fitted values
arima_residuals <- as.vector(residuals(arima_resid_mod))
arima_fitted <- as.vector(fitted(arima_resid_mod))

# ACF and PACF for model Residuals. Shows random, white noise.
residual_mod_ACF <- stats::acf(arima_residuals, lag.max = 25, type = "correlation", main = expression('Autocorrelation Function of Residuals'), 
    ylab = expression('ACF'), xlab = expression('Lag, k'))
residual_mod_PACF <- stats::acf(arima_residuals, lag.max = 25, type = "partial", main = expression('Partial Autocorrelation Function of Residuals'), 
    ylab = expression('PACF'), xlab = expression('Lag, k'))


# Diagnostics Plots
par(mfrow=c(2,2),oma=c(0,0,0,0))

qqnorm(arima_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(arima_residuals, datax = TRUE, col = "red")
plot(arima_fitted, arima_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(arima_residuals, breaks = 15, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(arima_residuals, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(arima_residuals, pch = 16, cex = .5)
abline(h = 0, col = "red")


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(arima_residuals, type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_residual_mod <- as.array(cbind(residual_mod_ACF$lag[-1], residual_mod_ACF$acf[-1], LJ_Stat, LJ_Pvalue, residual_mod_PACF$lag, residual_mod_PACF$acf))
colnames(Ljung_Box_residual_mod)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_residual_mod <- as.data.frame(Ljung_Box_residual_mod)

# Plot Ljung-Box p-values
plot(Ljung_Box_residual_mod$Lag, Ljung_Box_residual_mod$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Ljung-Box Statistics'))
abline(h = .05, col = 'red', lty = "dashed")

# Ljung-Box statistic for the first 25 lags
Box.test(arima_residuals, type = "Ljung", lag = 25, fitdf=1)


# Regression with ARIMA Errors --------------------------------------------


# Fit a new lm model with arima modeling the residuals
regModARIMA <- with(Training_Data, Arima(Conf_Deaths_BC, xreg = Patients, order = c(0,1,1)))
summary(regModARIMA)

# Test the coefficients
coeftest(regModARIMA, df = 346-1-2)


# Inverse roots
autoplot(regModARIMA)

# Diagnostic Plots
regModARIMA_residuals <- as.vector(residuals(regModARIMA))
regModARIMA_fitted <- as.vector(fitted(regModARIMA))

# ACF and PACF for model Residuals. Shows random, white noise.
regModARIMA_ACF <-  stats::acf(regModARIMA_residuals, lag.max = 25, type = "correlation", main = expression('Autocorrelation Function of Residuals'), 
                               ylab = expression('ACF'), xlab = expression('Lag, k'))
regModARIMA_PACF <- stats::acf(regModARIMA_residuals, lag.max = 25, type = "partial", main = expression('Partial Autocorrelation Function of Residuals'), 
                               ylab = expression('PACF'), xlab = expression('Lag, k'))


par(mfrow=c(2,2),oma=c(0,0,0,0))

qqnorm(regModARIMA_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(regModARIMA_residuals, datax = TRUE, col = "red")
plot(regModARIMA_fitted, regModARIMA_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(regModARIMA_residuals, breaks = 15, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(regModARIMA_residuals, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(regModARIMA_residuals, pch = 16, cex = .5)
abline(h = 0, col = "red")

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(regModARIMA_residuals, type = "Ljung", lag = number, fitdf=2)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_regModARIMA <- as.array(cbind(regModARIMA_ACF$lag[-1], regModARIMA_ACF$acf[-1], LJ_Stat, LJ_Pvalue, regModARIMA_PACF$lag, regModARIMA_PACF$acf))
colnames(Ljung_Box_regModARIMA)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_regModARIMA <- as.data.frame(Ljung_Box_regModARIMA)

# Plot Ljung-Box p-values
plot(Ljung_Box_regModARIMA$Lag, Ljung_Box_regModARIMA$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Ljung-Box Statistics for Regression Model with ARIMA(0,1,1) Errors'))
abline(h = .05, col = 'red', lty = "dashed")

# Ljung-Box statistic for the first 25 lags
Box.test(regModARIMA_residuals, type = "Ljung", lag = 25, fitdf=2)

# Normality test to be paired with the QQ plot
shapiro.test(regModARIMA_residuals)


# Model Forecasting and Evaluation ----------------------------------------


# Obtain daily forecasts.
Deaths_Forecasts <- forecast(regModARIMA, xreg = Forecast_Test_Data$Patients, h = 7, level = 95, biasadj = TRUE)
Deaths_Forecasts

# Create a data frame specifically for plotting the model, actual data, and forecasts
Deaths_Forecasts_df <- as.data.frame(Deaths_Forecasts)
regModARIMA_Graphing <- as.data.frame(cbind(double(353), 
                                            c(Training_Data$Confirmed_Deaths, Forecast_Test_Data$Confirmed_Deaths),
                                            c((InvBoxCox(regModARIMA_fitted, lambda = .5)-1), InvBoxCox(Deaths_Forecasts_df$`Point Forecast`, lambda = .5)-1), 
                                            c(double(346), InvBoxCox(Deaths_Forecasts_df$`Lo 95`, lambda = .5)-1), 
                                            c(double(346), InvBoxCox(Deaths_Forecasts_df$`Hi 95`, lambda = .5)-1)))
regModARIMA_Graphing$V1 <- c(Training_Data$Date, Forecast_Test_Data$Date)

colnames(regModARIMA_Graphing) <- c("Date", "Training_Data", "Fitted_Values", "Lower_95", "Upper_95")

# Insert NAs
for (row in 1:346) {
  for (column in c(4,5)) {
    regModARIMA_Graphing[row, column] <- NA
  }
}




# Plain ARIMA model for Confirmed Deaths ----------------------------------


# Graph the confirmed deaths time series
(ggplot(data = Training_Data, aes(x = Date, y = Confirmed_Deaths))
 + geom_line(color = "red", size = 1)
 + geom_point(color = "blue", pch = 16)
 + xlab("Date") + ylab("New Confirmed Deaths")
 + ggtitle("Daily Confirmed Deaths of Covid-19 in Massachusetts")
 + scale_y_continuous(breaks = seq(0, 200, by = 20))
 + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-16", format = "%Y-%m-%d"))) 
 + theme(axis.text.x = element_text(angle=25))
 + theme_bw()
)

# ACF of deaths
acf(Training_Data$Confirmed_Deaths, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(Training_Data$Confirmed_Deaths + 1 ~ Training_Data$Date, lambda = seq(-3, 3, by = 1))
BoxCox.lambda(Training_Data$Confirmed_Deaths, method = 'guerrero') # 0.3393836
# Choose lambda = .5 transformation
Training_Data$Conf_Deaths_BC <- BoxCox(Training_Data$Confirmed_Deaths + 1, lambda = .5)


# Graph the transformed confirmed deaths time series
(ggplot(data = Training_Data, aes(x = Date, y = Conf_Deaths_BC))
  + geom_line(color = "red", size = 1)
  + geom_point(color = "blue", pch = 16)
  + xlab("Date") + ylab("New Deaths by Day")
  + ggtitle("Daily Confirmed Deaths of Covid-19 in Massachusetts (transformed)")
  + scale_y_continuous(breaks = seq(0, 25, by = 5))
  + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-16", format = "%Y-%m-%d"))) 
  + theme(axis.text.x = element_text(angle=25))
  + theme_bw()
)

# ACF of deaths transformed
acf(Training_Data$Conf_Deaths_BC, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Take non-seasonal difference of time series, since ACF indicates severe non-stationarity
Deaths_Diff <- Training_Data
Deaths_Diff$Conf_Deaths_BC <- c(array(NA, dim = c(1, 1)), diff(Deaths_Diff$Conf_Deaths_BC, 1))
Deaths_Diff <- Deaths_Diff[-c(1:1), ]

# Graph the differenced confirmed deaths time series
(ggplot(data = Deaths_Diff, aes(x = Date, y = Conf_Deaths_BC))
  + geom_line(color = "red", size = 1)
  + geom_point(color = "blue", pch = 16)
  + xlab("Date") + ylab("New Deaths by Day")
  + ggtitle("Daily Confirmed Deaths of Covid-19 in Massachusetts (differenced)")
  + scale_y_continuous(breaks = seq(-5, 5, by = 2.5), limits = c(-5, 5))
  + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month", limits = c(as.Date("2020-04-05", format = "%Y-%m-%d"), as.Date("2021-03-16", format = "%Y-%m-%d"))) 
  + theme(axis.text.x = element_text(angle=25))
  + theme_bw()
)

# ACF of deaths differenced
acf(Deaths_Diff$Conf_Deaths_BC, lag.max = 50, type = "correlation", main = expression('ACF for Differenced Deaths Series'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
# PACF of deaths differenced
acf(Deaths_Diff$Conf_Deaths_BC, lag.max = 50, type = "partial", main = expression('PACF for Differenced Deaths Series'),
    ylab = expression('PACF, r'[k]), xlab = expression('Lag, k'))


# Create an ARIMA(0,1,1)x(0,0,0) model.  ----------------------------------
deaths_mod<- stats::arima(Training_Data$Conf_Deaths_BC, order = c(0, 1, 0)) 
summary(deaths_mod)

# Fiding BIC.
AIC(deaths_mod, k = log(length(Training_Data$Conf_Deaths_BC)))

# Results:
# (AIC = 1282.55)
# (BIC = 1286.394)


# Create an ARIMA(0,1,1)x(0,0,0) model.  ----------------------------------
deaths_mod<- stats::arima(Training_Data$Conf_Deaths_BC, order = c(0, 1, 1)) 
summary(deaths_mod)

# Fiding BIC.
AIC(deaths_mod, k = log(length(Training_Data$Conf_Deaths_BC)))

# Results:
# (AIC = 1156.79)
# (BIC = 1164.487)
# Check the p-values of the model coefficients to see if they are significant. 
coeftest(deaths_mod, df = 346-1-1)


# Store the residuals and fitted values
deaths_residuals <- as.vector(residuals(deaths_mod))
deaths_fitted <- as.vector(fitted(deaths_mod))

# Inverse roots
autoplot(deaths_mod)

# ACF and PACF for model Residuals. Shows random, white noise.
deaths_mod_ACF <- stats::acf(deaths_residuals, lag.max = 25, type = "correlation", main = expression('Autocorrelation Function of Residuals'), 
                             ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
deaths_mod_PACF <- stats::acf(deaths_residuals, lag.max = 25, type = "partial", main = expression('Partial Autocorrelation Function of Residuals'), 
                              ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Diagnostics Plots
par(mfrow=c(2,2),oma=c(0,0,0,0))

qqnorm(deaths_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(deaths_residuals, datax = TRUE, col = "red")
plot(deaths_fitted, deaths_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(deaths_residuals, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(deaths_residuals, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(deaths_residuals, pch = 16, cex = .5)
abline(h = 0, col = "red")


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(deaths_residuals, type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_deaths_mod <- as.array(cbind(deaths_mod_ACF$lag[-1], deaths_mod_ACF$acf[-1], LJ_Stat, LJ_Pvalue, deaths_mod_PACF$lag, deaths_mod_PACF$acf))
colnames(Ljung_Box_deaths_mod)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_deaths_mod <- as.data.frame(Ljung_Box_deaths_mod)

# Plot Ljung-Box p-values
plot(Ljung_Box_deaths_mod$Lag, Ljung_Box_deaths_mod$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Ljung-Box Statistics for ARIMA(0,1,1) Model'))
abline(h = .05, col = 'red', lty = "dashed")

# Ljung-Box statistic for the first 25 lags
Box.test(deaths_residuals, type = "Ljung", lag = 25, fitdf=1)




# Inadvisable ARIMA(5,1,1) Model ------------------------------------------


# Create an ARIMA(5,1,1)x(0,0,0) model.  ----------------------------------
bad_deaths_mod<- stats::arima(Training_Data$Conf_Deaths_BC, order = c(5, 1, 1)) 
summary(bad_deaths_mod)

# Fiding BIC.
AIC(bad_deaths_mod, k = log(length(Training_Data$Conf_Deaths_BC)))

# Results:
# (AIC = 1156.79)
# (BIC = 1164.487)
# Check the p-values of the model coefficients to see if they are significant. 
coeftest(bad_deaths_mod, df = 346-1-6)


# Store the residuals and fitted values
bad_deaths_residuals <- as.vector(residuals(bad_deaths_mod))
bad_deaths_fitted <- as.vector(fitted(bad_deaths_mod))


# Inverse roots
autoplot(bad_deaths_mod)

# ACF and PACF for model Residuals. Shows random, white noise.
bad_deaths_mod_ACF <- stats::acf(bad_deaths_residuals, lag.max = 25, type = "correlation", main = expression('Autocorrelation Function of Residuals'), 
                             ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
bad_deaths_mod_PACF <- stats::acf(bad_deaths_residuals, lag.max = 25, type = "partial", main = expression('Partial Autocorrelation Function of Residuals'), 
                              ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Diagnostics Plots
par(mfrow=c(2,2),oma=c(0,0,0,0))

qqnorm(bad_deaths_residuals, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(bad_deaths_residuals, datax = TRUE, col = "red")
plot(bad_deaths_fitted, bad_deaths_residuals, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(bad_deaths_residuals, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(bad_deaths_residuals, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(bad_deaths_residuals, pch = 16, cex = .5)
abline(h = 0, col = "red")


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(bad_deaths_residuals, type = "Ljung", lag = number, fitdf=5)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_bad_deaths_mod <- as.array(cbind(bad_deaths_mod_ACF$lag[-1], bad_deaths_mod_ACF$acf[-1], LJ_Stat, LJ_Pvalue, bad_deaths_mod_PACF$lag, bad_deaths_mod_PACF$acf))
colnames(Ljung_Box_bad_deaths_mod)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_bad_deaths_mod <- as.data.frame(Ljung_Box_bad_deaths_mod)

# Plot Ljung-Box p-values
plot(Ljung_Box_bad_deaths_mod$Lag, Ljung_Box_bad_deaths_mod$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Ljung-Box Statistics'))
abline(h = .05, col = 'red', lty = "dashed")

# Ljung-Box statistic for the first 25 lags
Box.test(bad_deaths_residuals, type = "Ljung", lag = 25, fitdf=5)




# Model Forecasting and Evaluation  for ARIMA(0,1,1) ---------------------------------------


# Obtain daily forecasts .
Deaths_Forecasts_2 <- forecast(deaths_mod, h = 7, level = 95, biasadj = TRUE)
Deaths_Forecasts_2

# Create a data frame specifically for plotting the model, actual data, and forecasts
Deaths_Forecasts_df_2 <- as.data.frame(Deaths_Forecasts_2)
deaths_mod_Graphing <- as.data.frame(cbind(double(353), 
                                           c((InvBoxCox(deaths_fitted, lambda = .5)-1), InvBoxCox(Deaths_Forecasts_df_2$`Point Forecast`, lambda = .5)-1), 
                                           c(double(346), InvBoxCox(Deaths_Forecasts_df_2$`Lo 95`, lambda = .5)-1), 
                                           c(double(346), InvBoxCox(Deaths_Forecasts_df_2$`Hi 95`, lambda = .5)-1)))
deaths_mod_Graphing$V1 <- c(Training_Data$Date, Forecast_Test_Data$Date)

colnames(deaths_mod_Graphing) <- c("Date", "Fitted_Values", "Lower_95", "Upper_95")

# Insert NAs
for (row in 1:346) {
  for (column in 3:4) {
    deaths_mod_Graphing[row, column] <- NA
  }
}


# Model Forecasting and Evaluation  for ARIMA(5,1,1) ---------------------------------------


# Obtain daily forecasts .
Deaths_Forecasts_3 <- forecast(bad_deaths_mod, h = 7, level = 95, biasadj = TRUE)
Deaths_Forecasts_3

# Create a data frame specifically for plotting the model, actual data, and forecasts
Deaths_Forecasts_df_3 <- as.data.frame(Deaths_Forecasts_3)
bad_deaths_mod_Graphing <- as.data.frame(cbind(double(353), 
                                           c((InvBoxCox(bad_deaths_fitted, lambda = .5)-1), InvBoxCox(Deaths_Forecasts_df_3$`Point Forecast`, lambda = .5)-1), 
                                           c(double(346), InvBoxCox(Deaths_Forecasts_df_3$`Lo 95`, lambda = .5)-1), 
                                           c(double(346), InvBoxCox(Deaths_Forecasts_df_3$`Hi 95`, lambda = .5)-1)))
bad_deaths_mod_Graphing$V1 <- c(Training_Data$Date, Forecast_Test_Data$Date)

colnames(bad_deaths_mod_Graphing) <- c("Date", "Fitted_Values", "Lower_95", "Upper_95")

# Insert NAs
for (row in 1:346) {
  for (column in 3:4) {
    bad_deaths_mod_Graphing[row, column] <- NA
  }
}


# Model Comparation -------------------------------------------------------


# Plot the model vs actual data
(ggplot() 
 #+ geom_ribbon(data = deaths_mod_Graphing, aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill="blue") #3399FF
 #+ geom_ribbon(data = regModARIMA_Graphing, aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill="pink") #3399FF
 #+ geom_ribbon(data = bad_deaths_mod_Graphing, aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill="orange") #3399FF
 + geom_line(data = regModARIMA_Graphing[regModARIMA_Graphing$Date <= as.Date("2021-03-16"),], aes(Date, Training_Data, color="Actual Data")) #006B3C
 + geom_line(data = bad_deaths_mod_Graphing[bad_deaths_mod_Graphing$Date <= as.Date("2021-03-16"),], aes(Date, Fitted_Values, color = "ARIMA(5,1,1)"))
 + geom_line(data = deaths_mod_Graphing[deaths_mod_Graphing$Date <= as.Date("2021-03-16"),], aes(Date, Fitted_Values, color = "ARIMA(0,1,1)"))
 + geom_line(data = regModARIMA_Graphing[regModARIMA_Graphing$Date <= as.Date("2021-03-16"),], aes(Date, Fitted_Values, color = "Regression with ARIMA(0,1,1) Errors"))
 + scale_y_continuous(breaks = seq(0, 250, by = 20))
 + ylab('New Confirmed Deaths') + theme_bw()
 + scale_x_date(labels = date_format("%m/%Y"), date_breaks = "4 weeks", limits = c(as.Date("2020-03-30", format = "%Y-%m-%d"), as.Date("2021-03-16", format = "%Y-%m-%d"))) 
 + ggtitle("New Confirmed Deaths in Massachusetts")
 + scale_color_manual("", breaks = c("Actual Data", "Regression with ARIMA(0,1,1) Errors", 
                                     "ARIMA(0,1,1)", "ARIMA(5,1,1)"), 
                      values = c("#006B3C", "Blue", "Orange", "Red"))
 + theme(legend.position = "bottom")
 + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)

# Zoomed view
(ggplot() 
  #+ geom_ribbon(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("blue", .75))
  #+ geom_ribbon(data = bad_deaths_mod_Graphing[327:nrow(bad_deaths_mod_Graphing),], aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("orange", .75))
  #+ geom_ribbon(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("pink", .75)) #3399FF
  + geom_line(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(Date, Training_Data, color="Actual Data")) #006B3C
  + geom_line(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(Date, Fitted_Values, color = "Regression with ARIMA(0,1,1) Errors"))
  + geom_line(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(Date, Fitted_Values, color = "ARIMA(0,1,1)"))
  + geom_line(data = bad_deaths_mod_Graphing[327:nrow(bad_deaths_mod_Graphing),], aes(Date, Fitted_Values, color = "ARIMA(5,1,1)"))
  + scale_x_date(labels = date_format("%b %d"), date_breaks = "3 days") 
  + theme(axis.text.x = element_text(angle=25))
  + scale_y_continuous(limits = c(-5, 80), breaks = seq(-5, 80, by = 10))
  + ylab('New Confirmed Deaths') + theme_bw()
  + ggtitle("New Confirmed Deaths in Massachusetts")
  + geom_point(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, y = Training_Data, color = "Actual Data"))
  + geom_point(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, y = Fitted_Values, color = "Regression with ARIMA(0,1,1) Errors"))
  + geom_point(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(x = Date, y = Fitted_Values, color = "ARIMA(0,1,1)"))
  + geom_point(data = bad_deaths_mod_Graphing[327:nrow(bad_deaths_mod_Graphing),], aes(x = Date, y = Fitted_Values, color = "ARIMA(5,1,1)"))
  # LABELS
  + geom_label(aes(x = as.Date("2021-03-20"), y = 75, label = "Forecasts"), 
               color = "black", size = 6)
  + geom_vline(xintercept = as.Date("2021-03-17"), linetype = "dashed")
  + scale_color_manual("", breaks = c("Actual Data", "Regression with ARIMA(0,1,1) Errors", 
                                      "ARIMA(0,1,1)", "ARIMA(5,1,1)"), 
                       values = c("#006B3C", "Blue", "Orange", "Red"))
  + theme(legend.position = "bottom")
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)


# Zoomed view with prediction intervals
(ggplot() 
  + geom_ribbon(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("blue", .75))
  + geom_ribbon(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("pink", .8)) #3399FF
  + geom_line(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(Date, Training_Data, color="Actual Data")) #006B3C
  + geom_line(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(Date, Fitted_Values, color = "Regression with ARIMA(0,1,1) Errors"))
  + geom_line(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(Date, Fitted_Values, color = "ARIMA(0,1,1)"))
  + scale_x_date(labels = date_format("%b %d"), date_breaks = "3 days") 
  + theme(axis.text.x = element_text(angle=25))
  + scale_y_continuous(limits = c(-5, 80), breaks = seq(-5, 80, by = 10))
  + ylab('New Confirmed Deaths') + theme_bw()
  + ggtitle("New Confirmed Deaths in Massachusetts")
  + geom_point(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, y = Training_Data, color = "Actual Data"))
  + geom_point(data = regModARIMA_Graphing[327:nrow(regModARIMA_Graphing),], aes(x = Date, y = Fitted_Values, color = "Regression with ARIMA(0,1,1) Errors"))
  + geom_point(data = deaths_mod_Graphing[327:nrow(deaths_mod_Graphing),], aes(x = Date, y = Fitted_Values, color = "ARIMA(0,1,1)"))
  # LABELS
  + geom_label(aes(x = as.Date("2021-03-20"), y = 75, label = "Forecasts"), 
               color = "black", size = 6)
  + geom_vline(xintercept = as.Date("2021-03-17"), linetype = "dashed")
  + scale_color_manual("", breaks = c("Actual Data", "Regression with ARIMA(0,1,1) Errors", 
                                      "ARIMA(0,1,1)"), 
                       values = c("#006B3C", "Blue", "Red"))
  + theme(legend.position = "bottom")
  + labs(caption = c('Data sourced from *MA CoViD-19 Dashboard'))
)




 # Out of Sample Forecast error measures
ROWS_OOS <- (nrow(regModARIMA_Graphing)-6):nrow(regModARIMA_Graphing)
ROWS_IS <- 1:nrow((regModARIMA_Graphing)-7)

Model_1_EM_OOS <- c(rmse(regModARIMA_Graphing[ROWS_OOS, 2], regModARIMA_Graphing[ROWS_OOS, 3]), 
                    mae(regModARIMA_Graphing[ROWS_OOS, 2], regModARIMA_Graphing[ROWS_OOS, 3]), 
                    mape(regModARIMA_Graphing[ROWS_OOS, 2], regModARIMA_Graphing[ROWS_OOS, 3]), 
                    mase(regModARIMA_Graphing[ROWS_OOS, 2], regModARIMA_Graphing[ROWS_OOS, 3]))

Model_2_EM_OOS <- c(rmse(regModARIMA_Graphing[ROWS_OOS, 2], deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mae(regModARIMA_Graphing[ROWS_OOS, 2], deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mape(regModARIMA_Graphing[ROWS_OOS, 2], deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mase(regModARIMA_Graphing[ROWS_OOS, 2], deaths_mod_Graphing[ROWS_OOS, 2]))

Model_3_EM_OOS <- c(rmse(regModARIMA_Graphing[ROWS_OOS, 2], bad_deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mae(regModARIMA_Graphing[ROWS_OOS, 2], bad_deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mape(regModARIMA_Graphing[ROWS_OOS, 2], bad_deaths_mod_Graphing[ROWS_OOS, 2]), 
                    mase(regModARIMA_Graphing[ROWS_OOS, 2], bad_deaths_mod_Graphing[ROWS_OOS, 2]))

Model_1_EM_IS <- c(rmse(regModARIMA_Graphing[ROWS_IS, 2], regModARIMA_Graphing[ROWS_IS, 3]), 
                   mae(regModARIMA_Graphing[ROWS_IS, 2], regModARIMA_Graphing[ROWS_IS, 3]), 
                   mape(regModARIMA_Graphing[ROWS_IS, 2], regModARIMA_Graphing[ROWS_IS, 3]), 
                   mase(regModARIMA_Graphing[ROWS_IS, 2], regModARIMA_Graphing[ROWS_IS, 3]))

Model_2_EM_IS <- c(rmse(regModARIMA_Graphing[ROWS_IS, 2], deaths_mod_Graphing[ROWS_IS, 2]), 
                   mae(regModARIMA_Graphing[ROWS_IS, 2], deaths_mod_Graphing[ROWS_IS, 2]), 
                   mape(regModARIMA_Graphing[ROWS_IS, 2], deaths_mod_Graphing[ROWS_IS, 2]), 
                   mase(regModARIMA_Graphing[ROWS_IS, 2], deaths_mod_Graphing[ROWS_IS, 2]))

Model_3_EM_IS <- c(rmse(regModARIMA_Graphing[ROWS_IS, 2], bad_deaths_mod_Graphing[ROWS_IS, 2]), 
                   mae(regModARIMA_Graphing[ROWS_IS, 2], bad_deaths_mod_Graphing[ROWS_IS, 2]), 
                   mape(regModARIMA_Graphing[ROWS_IS, 2], bad_deaths_mod_Graphing[ROWS_IS, 2]), 
                   mase(regModARIMA_Graphing[ROWS_IS, 2], bad_deaths_mod_Graphing[ROWS_IS, 2]))

Forecast_Error_Measures <- data.frame(Model_1_OOS = Model_1_EM_OOS, 
                                      Model_2_OOS = Model_2_EM_OOS, 
                                      Model_3_OOS = Model_3_EM_OOS,
                                      Model_1_IS = Model_1_EM_IS,
                                      Model_2_IS = Model_2_EM_IS, 
                                      Model_3_IS = Model_3_EM_IS)

rownames(Forecast_Error_Measures) <- c("RMSE", "MAE", "MAPE", "MASE")



