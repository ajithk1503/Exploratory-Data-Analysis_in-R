library(dplyr)
library(ggplot2)
library(cluster)

# Load the dataset
data <- read.csv('telco customer churn (2).csv')

# Select relevant columns and clean data
data <- data %>% 
  select(tenure, TotalCharges) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  na.omit()

# Create a matrix or data frame for clustering
X <- data

# Initialize WCSS vector
wcss <- numeric(10)

# Perform K-means clustering for k = 1 to 10
for (i in 1:10) {
  kmeans_model <- kmeans(X, centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot WCSS values to visualize the Elbow Method
plot(1:10, wcss, type = "b", pch = 19, col = "blue", 
     xlab = "Number of Clusters (k)", 
     ylab = "WCSS", 
     main = "Elbow Method for Optimal k")

# Maximum change in slope is observed in plot at 2 clusters
set.seed(123)
kmeans_model <- kmeans(X, centers = 2, nstart = 10)
cat("Number of clusters:", length(unique(kmeans_model$cluster)), "\n")

# Plot the clusters without labels
clusplot(X,
         kmeans_model$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = 'Clusters of Customers',
         xlab = 'Total Charges',
         ylab = 'Tenure')

# hierarchial clustering
data <- data[1:100, ]
data$tenure <- as.numeric(as.character(data$tenure))
data <- na.omit(data)
distance_matrix <- dist(data$tenure, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D")
plot(hc, main = "Dendrogram of First 100 Telecom Customers (Tenure)", xlab = "Customers", ylab = "Height")

# perform the PCA for dimensionality reduction.
data <- read.csv('telco customer churn (2).csv')
data$tenure <- as.numeric(as.character(data$tenure))
data$MonthlyCharges <- as.numeric(data$MonthlyCharges)
data$TotalCharges <- as.numeric(data$TotalCharges)
data <- na.omit(data)
numeric_data <- data[, c("tenure", "MonthlyCharges", "TotalCharges")]
scaled_data <- scale(numeric_data)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)
plot(pca_result$x[,1:2], col="blue", pch=16, xlab="PC1", ylab="PC2", main="PCA")
