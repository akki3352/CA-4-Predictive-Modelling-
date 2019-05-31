
#Install required packages
install.packages("tidyverse")
install.packages("e1071")
install.packages("broom")
install.packages("tidyverse")
install.packages('caret', dependencies = TRUE)
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")
library(tidyverse)
library(caret)
library(tidyverse)
library(broom)
library(ggfortify)
library(tidyr)
library(dplyr)
library(e1071)
library(ggfortify)
library(ggplot2)
library(forecast)
library(tseries)

# Loading data
Crime_Unemployment_rate <- read.csv("Crime_Unemployment_rate.csv")
Crime_Unemployment_rate[,1] <- NULL
Crime_Unemployment_rate$Unemployment_rate_greater_than_12 <- ifelse(Crime_Unemployment_rate$Unemployment_Rate > 12,
                                                                    "High", "Low")


#The correlation coefficient measures the level of the association between two variables 
cor(Crime_Unemployment_rate$Crime_rate, Crime_Unemployment_rate$Unemployment_Rate)

# Scatter plots can help visualise any linear relationships between the 
# dependent (response) distance variable and independent (predictor) speed variables
# According to Scatter plots, we can say that when unemploymnet rat is increased, crime rate also increased.


scatter.smooth(x = Crime_Unemployment_rate$Crime_rate, 
               y = Crime_Unemployment_rate$Unemployment_Rate, 
               main = "Crime_rate ~ Unemployment_Rate",
               xlab = "Crime rate",
               ylab = "Unemployment rate")

# Skewness function to examine normality of data
# divide graph area in 2 columns
par(mfrow = c(1, 2))
# density plot for 'Crime Rate'
plot(density(Crime_Unemployment_rate$Crime_rate), main = "Density Plot: Crime Rate", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(Crime_Unemployment_rate$Crime_rate), 2)))


# Lets fill in the area under the density plot in blue
polygon(density(Crime_Unemployment_rate$Crime_rate), col = "blue")


# density plot for 'Unemployment Rate'
plot(density(Crime_Unemployment_rate$Unemployment_Rate), main = "Density Plot: Unemployment Rate", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(Crime_Unemployment_rate$Unemployment_Rate), 2)))


# Lets fill in the area under the density plot in blue
polygon(density(Crime_Unemployment_rate$Unemployment_Rate), col = "blue")


# Create Training and Test data
# setting seed to reproduce results of random sampling
set.seed(200)

# sample chooses a random sample
# from 1:all records from Crime_Unemployment_rate, 75% of rows
training.samples <- Crime_Unemployment_rate$Unemployment_Rate %>%
  createDataPartition(p = 0.75, list = FALSE)

# model training data
train.data  <- Crime_Unemployment_rate[training.samples, ]

#model testing data
test.data <- Crime_Unemployment_rate[-training.samples, ]

# Build the model on training data
lr_model <- lm(Crime_rate ~ Unemployment_Rate , data = Crime_Unemployment_rate)

# model summary
summary(lr_model)


# augment dataset to add fitted values and residuals by using the function augment()
model.diag.metrics <- augment(lr_model)
head(model.diag.metrics)

#plots the residuals error (in red color) between observed values and the fitted regression line.
ggplot(model.diag.metrics, aes(Unemployment_Rate, Crime_rate)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Unemployment_Rate, yend = .fitted), color = "blue", size = 0.3)


#Create the diagnostic plots using plot:
par(mfrow = c(2, 2))
plot(lr_model)
# Create the diagnostic plots using ggfortify:
autoplot(lr_model)

#Plot multiple time series data
T_Crime_Unemployment_rate <- Crime_Unemployment_rate %>%
  select(Period, Crime_rate, Unemployment_Rate) %>%
  gather(key = "variable", value = "value", -Period)
head(T_Crime_Unemployment_rate, 3)


# Multiple line plot
ggplot(T_Crime_Unemployment_rate, aes(x = Period, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# Area plot
ggplot(T_Crime_Unemployment_rate, aes(x = Period, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))


# Base plot with Period axis
p <- ggplot(data = Crime_Unemployment_rate, aes(x = Period, y = Crime_rate)) + 
  geom_line(color = "#00AFBB", size = 1)
p

ts_crime_unemployment <- ts(Crime_Unemployment_rate$Crime_rate, Crime_Unemployment_rate$Unemployment_Rate)
plot(ts_crime_unemployment)

m_ets = ets(ts_crime_unemployment)
f_ets = forecast(m_ets, h=24) # forecast 24 months into the future
plot(f_ets)


m_tbats = tbats(ts_crime_unemployment)
f_tbats = forecast(m_tbats, h=24)
plot(f_tbats)
