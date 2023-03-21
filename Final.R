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

df2 <- ts(df$x, frequency = 5) # analyzing the weekday patterns for cyclical & seasonal model, set the frequency to 5
df3 <- ts(df$x, frequency = 7) # trial and error, set the frequency to 7, trend lines improve


##############  Exploratory Data Analysis
head(df)
str(df)
range(df$x)
sd(df$x) 
var(df$x) 
summary(df$x) 
which(is.na(df$x))

boxplot(df$x, ylab="Price Returns (in AUD100,000)", main="Boxplot of Investment Portfolio")
descdist(df$x, discrete = FALSE)
hist(df$x, freq = FALSE, main ='Histogram of Investment', xlab = 'Returns (in AUD100,000)')
lines(density(df$x), col ='red')  

# Calculate correlation for first lag

y = data_ts 
x = zlag(data_ts) 
index = 2:length(x) # Create an index to get rid of the first NA value i
cor(y[index],x[index]) 

plot(y=data_ts,x=zlag(data_ts),ylab='Prices(in AUD100,000)', xlab='Previous Day Sales',
     main = "Scatter plot of Investment Portfolio prices in consequtive days")
plot(data_ts[-127],data_ts[-1],main="Scatterplot Of Investment Returns on consecutive days",ylab='Returns (in AUD100,000)', xlab='Previous Days')
abline(lm(data_ts[-1] ~ data_ts[-127]))


##############  Functions defined

fitted_plot <- function(model_no, model_name, dataset){
  plot(ts(fitted(model_no)), ylim = c(min(c(fitted(model_no),as.vector(dataset))), max(c(fitted(model_no),as.vector(dataset)))), 
       xlab='Time (In Days)', ylab='Returns (in AUD100,000)', main = paste0("Fitted ", model_name, " for Portfolio Return"), col = 'red') 
  lines(as.vector(dataset),type="o")
}

residual_analysis <- function(model_no, model_name, dataset) {
  par(mfrow=c(2,2))  
  residual_model <- rstudent(model_no)
  plot(y=residual_model, x=as.vector(time(dataset)), xlab='Time (In days)',ylab='Standardized Residuals',type='o',
       main = paste0("Time series plot of residuals (", model_name, ")"))
  lines(as.vector(dataset),type="o")
  hist(residual_model,xlab='Residuals',main = paste0("Histogram of Residuals (", model_name, ")"))
  acf(residual_model, main = paste0("ACF (", model_name, ")"))
  qqnorm(residual_model, main = paste0("Q-Q plot of Residuals (", model_name, ")"))
  qqline(residual_model, col = 2, lwd = 1, lty = 2)
  shapiro.test(residual_model)
}


##############  Model 1: Linear Model 

model1 = lm(data_ts~time(data_ts)) 
summary(model1)
plot(data_ts,type='o', xlab='Time (In Days)', ylab='Returns (in AUD100,000)', main = "Fitted Linear Model to Stocks")
abline(model1, col='red') 

residual_analysis(model1, "Linear Model", data_ts) # Residual Analysis for Linear Trend
# Model 2: Quadratic Model 

t = time(data_ts)
t2 = t^2
model2 = lm(data_ts~t+t2) 
summary(model2) 
fitted_plot(model2, "Quadratic Model", data_ts)

residual_analysis(model2, "Quadratic Model", data_ts) # Residual Analysis for Quadratic Trend
# Model 3: Harmonic Model 

har.=harmonic(df3,1)
model3=lm(df3~har.)
summary(model3)

fitted_plot(model3, "Harmonic Model", df3)

residual_analysis(model3, "Harmonic Model", df3) # Residual Analysis for Harmonic Trend
# Model 4: Cyclical & Seasonal Model 

day.=season(df2) # period added to improve table display
model4=lm(df2~day.-1) # -1 removes the intercept term
summary(model4) 

fitted_plot(model4, "Quadratic Model", df2)

model5=lm(df2~day.) # include intercept term 
summary(model5)

residual_analysis(model4, "Cyclical Model", df2) # Residual Analysis for Cyclical Trend
residual_analysis(model5, "Seasonal Model", df2) # Residual Analysis for Seasonal Trend

# Labelled Residual Plot for Seasonal Trend

color = rep(NA, length = length(df_weekdays$weekday))
levels(day.) <- c("Monday", "Tuesday","Wednesday","Thursday","Friday")
color[which(day.=="Monday")] = "red"
color[which(day.=="Tuesday")] = "blue"
color[which(day.=="Wednesday")] = "dark green"
color[which(day.=="Thursday")] = "orange"
color[which(day.=="Friday")] = "black"

plot(y=rstudent(model5),x=as.vector(time(data_ts)),xlab='Time', ylab='Standardized Residuals',type='l', main = "Time series plot of residuals with labels (Seasonal Model)")
points(y=rstudent(model5),x=as.vector(time(data_ts)), col = color, pch=as.vector(day.))

# Model 6: Cosine + Sine + Linear + Quadratic Term (Best)
har.=harmonic(df3,1)
# model6=lm(df3~ har. + t + t2)
data <- data.frame(df3,har. , t , t2)
model6 <- lm(df3 ~ cos.2.pi.t. + sin.2.pi.t. + t + t2 , data = data)
summary(model6)
fitted_plot(model6, "Harmonic + Linear + Quadratic Term Model", df3)

residual_analysis(model6, "Harmonic + Linear + Quadratic", df3) # Residual Analysis for Harmonic Trend


############## Forecast with best model

par(mfrow=c(1,1))

h <- 15 # predict 15 days
increment <- 1 
original.t <- time(data_ts) # Take original time
n <- length(original.t) # Find the length of observation period
lastTimePoint <- original.t[n] # Find the last time point
t_df <- seq(lastTimePoint + increment,
            lastTimePoint + increment * h, increment)

t5 <- cos(2*pi*t_df) # Create the variables in model6, but i start from term 5 prevent overwrite
t6 <- sin(2*pi*t_df)
t7 <- t_df
t8 <- t_df^2

aheadTimes <- data.frame(cos.2.pi.t. = t5 , sin.2.pi.t. = t6, t = t7, t2 = t8)
frcModel6 <- predict(model6, newdata = aheadTimes, interval = "prediction")

plot(data_ts, xlim= c(1,180), ylim = c(-100,200), ylab = "Stock Return series",
     main = "Forecasts from the Harmonic + Linear + Quadraric term model fitted to the stock return series.")
lines(ts(as.vector(frcModel6[,3]), start = 128), col="blue", type="l")
lines(ts(as.vector(frcModel6[,1]), start = 128), col="red", type="l")
lines(ts(as.vector(frcModel6[,2]), start = 128), col="blue", type="l")
legend("topright", lty=1, pch=1, col=c("black","blue","red"), text.width = 18,
       c("Data","5% forecast limits", "Forecasts"))

############## References
Time Series Analysis (Math 1318)- Modules 1 & 2, Authored by Dr Haydar Demirhan

























