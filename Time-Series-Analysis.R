# Q 6 Load the Open Power System Data from the below link and perform the Time Series Analysis with all insights and explore

# 1. Load the dataset in R (replace 'your_dataset.csv' with your actual dataset file path)
data <- read.csv('opsd_germany_daily.csv')

# 2. Check for the Structure and the Data type of data
str(data)

# 3. Check for missing values in the dataset
summary(is.na(data))

# 4. Check for the Starting date and Ending date
library(xts)
#data <- xts(Consumption, order.by = Date)
start <- min(index(date))  
end <- max(index(data))
start
end


# 5. Check for the frequency of the dataset
frequency(data)

# 6. Check for the summary of the dataset
summary(data)
data_ts <- ts(data$Consumption, start = c(2006, 1), frequency = 365)


# 7. Plot the decomposition of the dataset – Break data into trend, seasonal, and random
decomposed <- decompose(data_ts)
plot(decomposed)

# 8. Plot the dataset
plot(data)

# 9. Plot the time-series of the dataset
data_clean <- na.omit(data)
data_ts <- ts(data_clean$Consumption, start = c(2006, 1), frequency = 365)
plot.ts(data_ts)


# 10. Draw the regressor line for the time-series plot
abline(lm(data_ts ~ time(data_ts)))

# 11. Print the cycle across the years for the dataset
cycle(data_ts)

# 12. Make the dataset stationary (constant mean and variance) and plot it
# a. Log transformation
data_log <- log(data_ts)
plot(data_log)

# b. Stationary mean (first difference after log transformation)
data_diff <- diff(data_log)
plot(data_diff)

# 13. Plot box plot across months for seasonal effect
boxplot(data_ts ~ cycle(data_ts))

# 14. Draw a box plot for each month
library(lubridate)
data_clean <- na.omit(data)
boxplot(data_clean$Consumption ~ format(as.Date(data_clean$Date), "%m"), 
        xlab = "Month", ylab = "Values", main = "Boxplot by Month")

# 15.  Time Series Plots for Wind, Solar, and Wind.Solar
par(mfrow = c(3, 1))
plot.ts(data_clean$Wind, main = "Wind Over Time", ylab = "Wind", xlab = "Time")
plot.ts(data_clean$Solar, main = "Solar Over Time", ylab = "Solar", xlab = "Time")
plot.ts(data_clean$Wind.Solar, main = "Wind.Solar Over Time", ylab = "Wind + Solar", xlab = "Time")

# 16. Scatter Plots
par(mfrow = c(1, 3))
plot(data_clean$Wind, data_clean$value, main = "Consumption vs Wind", xlab = "Wind", ylab = "Consumption")
plot(data_clean$Solar, data_clean$value, main = "Consumption vs Solar", xlab = "Solar", ylab = "Consumption")
plot(data_clean$Wind.Solar, data_clean$value, main = "Consumption vs Wind.Solar", xlab = "Wind + Solar", ylab = "Consumption")


#17 Multivariate Time Series Plot
plot.ts(cbind(data_clean$value, data_clean$Wind, data_clean$Solar, data_clean$Wind.Solar), 
        main = "Consumption, Wind, Solar, and Wind.Solar Over Time", 
        col = c("blue", "green", "orange", "red"), 
        plot.type = "single", ylab = "Values")
legend("topright", legend = c("Consumption", "Wind", "Solar", "Wind + Solar"), 
       col = c("blue", "green", "orange", "red"), lty = 1)

# 18 Seasonal Effect Analysis
par(mfrow = c(1, 3))
boxplot(data_clean$Wind ~ factor(month(as.Date(data_clean$Date))), 
        xlab = "Month", ylab = "Wind", main = "Monthly Wind")
boxplot(data_clean$Solar ~ factor(month(as.Date(data_clean$Date))), 
        xlab = "Month", ylab = "Solar", main = "Monthly Solar")
boxplot(data_clean$Wind.Solar ~ factor(month(as.Date(data_clean$Date))), 
        xlab = "Month", ylab = "Wind + Solar", main = "Monthly Wind + Solar")


