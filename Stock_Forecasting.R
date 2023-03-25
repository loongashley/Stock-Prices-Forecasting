# Load in libraries
library(zoo)
library(TSA)
library(broom)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fitdistrplus)
setwd("C://Users//loong//Desktop//Time Series//A1")
df <- read.csv("assignment1Data2023.csv",header = TRUE)
tseries <- read.zoo(df)
data_ts <- as.ts(tseries) # linear model estimates a single slope coefficient for the entire time period, frequency is 1
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") # df with weekends
df_weekdays <- data.frame(data_ts)
df_weekdays$weekday <- rep_len(weekdays, nrow(df_weekdays))
seasons <- c("day-1", "day-2", "day-3", "day-4", "day-5", "day-6", "day-7") 
df_weekdays$seasons <- rep_len(seasons, nrow(df_weekdays)) # for seasonal model, cycles of 7 days

df3 <- ts(df$x, frequency = 7) # trial and error, set the frequency to 7, trend lines improve
############## Exploratory Data Analysis
head(df)
str(df)
range(df$x)
sd(df$x)
var(df$x)
summary(df$x)
which(is.na(df$x))
boxplot(df$x, ylab="Price Returns (in AUD100,000)", main="Boxplot of Investment Portfolio")
descdist(df$x, discrete = FALSE)
hist(df$x, freq = FALSE, main ='Histogram of ROI Portfolio of a Share Market Trader', xlab = 'Returns
(in AUD100,000)')
lines(density(df$x), col ='red')
# Calculate correlation for first lag
y = data_ts
x = zlag(data_ts)
index = 2:length(x) # Create an index to get rid of the first NA value i
cor(y[index],x[index])
plot(y=data_ts,x=zlag(data_ts),ylab='Prices(in AUD100,000)', xlab='Previous Day Sales',
     main = "Scatter plot of Investment Portfolio prices in consequtive days")
plot(data_ts[-127],data_ts[-1],main="Scatterplot Of ROI Portfolio on consecutive days",ylab='Returns
(in AUD100,000)', xlab='Previous Days')
abline(lm(data_ts[-1] ~ data_ts[-127]))
############## Functions defined
fitted_plot <- function(model_no, model_name, dataset){
  plot(ts(fitted(model_no)), ylim = c(min(c(fitted(model_no),as.vector(dataset))),
                                      max(c(fitted(model_no),as.vector(dataset)))),
       xlab='Time (In Days)', ylab='Returns (in AUD100,000)', main = paste0("Fitted ", model_name, "
for Portfolio Return"), col = 'red')
  lines(as.vector(dataset),type="o")
}
residual_analysis <- function(model_no, model_name, dataset) {
  par(mfrow=c(2,2))
  residual_model <- rstudent(model_no)
  plot(y=residual_model, x=as.vector(time(dataset)), xlab='Time (In days)',ylab='Standardized
Residuals',type='o',
       main = paste0("Time series plot of residuals (", model_name, ")"))
  lines(as.vector(dataset),type="o")
  hist(residual_model,xlab='Residuals',main = paste0("Histogram of Residuals (", model_name, ")"))
  acf(residual_model, main = paste0("ACF (", model_name, ")"))
  qqnorm(residual_model, main = paste0("Q-Q plot of Residuals (", model_name, ")"))
  qqline(residual_model, col = 2, lwd = 1, lty = 2)
  shapiro.test(residual_model)
}
############## Model 1: Linear Model
model1 = lm(df3~time(df3))
summary(model1)
fitted_plot(model1, "Linear Model", df3)
residual_analysis(model1, "Linear Model", df3) # Residual Analysis for Linear Trend
# Model 2: Quadratic Model
t = time(data_ts)
t2 = t^2
model2 = lm(df3~t+t2)
summary(model2)
fitted_plot(model2, "Quadratic Model", df3)
residual_analysis(model2, "Quadratic Model", df3) # Residual Analysis for Quadratic Trend
# Model 3: Harmonic Model
har.=harmonic(df3,1)
model3=lm(df3~har.)
summary(model3)
fitted_plot(model3, "Harmonic Model", df3)
residual_analysis(model3, "Harmonic Model", df3) # Residual Analysis for Harmonic Trend
# Model 4: Cyclical & Seasonal Model
season. = as.factor(df_weekdays$seasons)
model4=lm(df3~season.-1) # -1 removes the intercept term
summary(model4) 
fitted_plot(model4, "Cyclical Model", df3)
residual_analysis(model4, "Cyclical Model", df3) # Residual Analysis for Cyclical Trend


model5=lm(df3~season.) # include intercept term
summary(model5)
fitted_plot(model5, "Seasonal Model", df3)
residual_analysis(model5, "Seasonal Model", df3) # Residual Analysis for Seasonal Trend
# Model 6: Harmonic + Linear Term (Best)
har.=harmonic(df3,1)
data <- data.frame(df3,har. , t)
model6 <- lm(df3 ~ cos.2.pi.t. + t , data = data)
summary(model6)
fitted_plot(model6, "Cosine & Linear Term Model", df3)
residual_analysis(model6, "Cosine & Linear", df3) # Residual Analysis for Harmonic Trend
############## Forecast with best model
par(mfrow=c(1,1))
h <- 15 # predict 15 days
increment <- 1/7
original.t <- time(data_ts) # Take original time
n <- length(original.t) # Find the length of observation period
lastTimePoint <- original.t[n] # Find the last time point
t_df <- seq(lastTimePoint + increment,
            lastTimePoint + increment * h, increment)
t1 <- cos(2*pi*t_df) # Create the variables in model6, but i start from term 5 prevent overwrite
aheadTimes <- data.frame(cos.2.pi.t. = t1 , t= t_df)
frcModel6 <- predict(model6, newdata = aheadTimes, interval = "prediction")
plot(data_ts, xlim= c(1,180), ylim = c(-100,200), ylab = "Stock Return series",
     main = "Forecasts from the Cosine & Linear term model fitted to the Portfolio Returns")
lines(ts(as.vector(frcModel6[,3]), start = 128), col="blue", type="l")
lines(ts(as.vector(frcModel6[,1]), start = 128), col="red", type="l")
lines(ts(as.vector(frcModel6[,2]), start = 128), col="blue", type="l")
legend("topright", lty=1, pch=1, col=c("black","blue","red"), text.width = 18,
       c("Data","5% forecast limits", "Forecasts"))
############## References
# Time Series Analysis (Math 1318)- Modules 1 & 2, Authored by Dr Haydar Demirhan