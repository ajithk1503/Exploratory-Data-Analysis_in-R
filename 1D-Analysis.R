# Q 7a Load the structued dataset and perform the EDA for 1 Dimension analysis

data<- read.csv('telco customer churn (2).csv')
# 1. Check Structure and Summary of the Dataset
str(data)
summary(data)
# 2. Check for Missing Values
colSums(is.na(data))
# 3. Categorical Variables Analysis
# Gender
table(data$gender)
barplot(table(data$gender), main = "Gender Distribution", xlab = "Gender", ylab = "Count")
# Senior Citizen
library(dplyr)
data <- data %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No"))
barplot(table(data$SeniorCitizen), main = "Senior Citizen Distribution", xlab = "Senior Citizen", ylab = "Count")
#partner
table(data$Partner)
barplot(table(data$Partner), main = "Partner Distribution", xlab = "Partner", ylab = "Count")
# phone service
table(data$PhoneService)
barplot(table(data$PhoneService), main = "Phone Service Distribution", xlab = "Phone Service", ylab = "Count")
# Internet Service
table(data$InternetService)
barplot(table(data$InternetService), main = "Internet Service Distribution", xlab = "Internet Service", ylab = "Count")

# tenure
summary(data$tenure)
data$tenure <- as.numeric(as.character(data$tenure))
sum(is.na(data$tenure))
hist(data$tenure, main = "Tenure Distribution", xlab = "Tenure", ylab = "Frequency", col = "lightblue")

# !!! 1 Dimensional Analysis
#mean
mean_tenure <- mean(data$tenure, na.rm = TRUE)
mean_tenure
# median
median_tenure <- median(data$tenure, na.rm = TRUE)
median_tenure
#quantile
quantiles_tenure <- quantile(data$tenure, probs = seq(0, 1, 0.25), na.rm = TRUE)
quantiles_tenure
#percentile
percentiles_tenure <- quantile(data$tenure, probs = seq(0, 1, 0.01), na.rm = TRUE)
percentiles_tenure

# Dispersion Measures

# Range
range_tenure <- range(data$tenure, na.rm = TRUE)
range_tenure

# InterQuartile Range
iqr_tenure <- IQR(data$tenure, na.rm = TRUE)
iqr_tenure

# Interdecile Range
decile_range <- quantile(data$tenure, probs = c(0.9, 0.1), na.rm = TRUE)
decile_range

#Mean Deviation
mean_deviation <- mean(abs(data$tenure - mean_tenure), na.rm = TRUE)
mean_deviation

# Standard Deviation
std_dev_tenure <- sd(data$tenure, na.rm = TRUE)
std_dev_tenure

# skewness
library(e1071)
skewness_tenure <- skewness(data$tenure, na.rm = TRUE)
skewness_tenure
# kurtosis
kurtosis_tenure <- kurtosis(data$tenure, na.rm = TRUE)
kurtosis_tenure

# Frequency Distribution
frequency_tenure <- table(data$tenure)
frequency_tenure

# histogram
hist(data$tenure, main = "Histogram of Tenure", xlab = "Tenure", col = "lightblue", border = "black")

# Relative Frequency Distribution
relative_freq_tenure <- prop.table(frequency_tenure)
relative_freq_tenure

# Cumulative Frequency Distribution
cumulative_freq_tenure <- cumsum(frequency_tenure)
cumulative_freq_tenure
plot(cumulative_freq_tenure, type = "o", main = "Cumulative Frequency Distribution", xlab = "Tenure", ylab = "Cumulative Frequency")

# Stacked Bar Plot for Partner and Churn
library(ggplot2)
ggplot(data, aes(x = Partner, fill = Churn)) + 
  geom_bar(position = "fill") + 
  labs(title = "Stacked Bar Plot of Partner vs Churn", y = "Proportion") + 
  scale_y_continuous(labels = scales::percent)

