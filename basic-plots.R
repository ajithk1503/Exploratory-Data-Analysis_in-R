data<- read.csv('Mall_Customers.csv')
# Apply Data Pre-processing and data cleaning
# (Assuming further preprocessing steps like handling missing values, outliers, etc.)

# Apply Statistical summary
summary(data)

# Store the Annual Income and Spending score in an object 'dataset'
dataset <-  data.frame(AnnualIncome=data$Annual.Income..k..,SpendingScore=data$Spending.Score..1.100.)

# Compute the distance matrix using dist() - apply the euclidean method
dist_euclidean <- dist(dataset, method = "euclidean")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_euclidean))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_euclidean <- hclust(dist_euclidean, method = "ward.D")

# Using basic plot function to visualize dendrogram (Euclidean method)
plot(hc_euclidean, cex = 0.6, hang = -1)

# Compute the distance matrix using dist() - apply the manhattan method
dist_manhattan <- dist(dataset, method = "manhattan")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_manhattan))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_manhattan <- hclust(dist_manhattan, method = "ward.D")

# Using basic plot function to visualize dendrogram (Manhattan method)
plot(hc_manhattan, cex = 0.6, hang = -1)

# Compute the distance matrix using dist() - apply the maximum method
dist_maximum <- dist(dataset, method = "maximum")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_maximum))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_maximum <- hclust(dist_maximum, method = "ward.D")

# Using basic plot function to visualize dendrogram (Maximum method)
plot(hc_maximum, cex = 0.6, hang = -1)

# Compute the distance matrix using dist() - apply the canberra distance method
dist_canberra <- dist(dataset, method = "canberra")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_canberra))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_canberra <- hclust(dist_canberra, method = "ward.D")

# Using basic plot function to visualize dendrogram (Canberra method)
plot(hc_canberra, cex = 0.6, hang = -1)

# Compute the distance matrix using dist() - apply the binary distance method
dist_binary <- dist(dataset, method = "binary")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_binary))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_binary <- hclust(dist_binary, method = "ward.D")

# Using basic plot function to visualize dendrogram (Binary method)
plot(hc_binary, cex = 0.6, hang = -1)

# Compute the distance matrix using dist() - apply the Minkowski distance method
dist_minkowski <- dist(dataset, method = "minkowski")

# Display the Matrix table (optional: comment out if too large to display)
# print(as.matrix(dist_minkowski))

# Using the dendrogram to find the optimal cluster - use hclust(). To minimize within the cluster use method as 'ward.D'
hc_minkowski <- hclust(dist_minkowski, method = "ward.D")

# Using basic plot function to visualize dendrogram (Minkowski method)
plot(hc_minkowski, cex = 0.6, hang = -1)
