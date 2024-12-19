library(readr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# 2. Load the dataset
data <- read_csv("Mall_Customers.csv")

# 3. Perform PCA on numeric columns using PCA() from FactoMineR
pca_result <- PCA(data[, sapply(data, is.numeric)], graph = FALSE)

# 4. Print PCA results, including eigenvalues and component loadings
print(pca_result$eig) 
print(pca_result$var$coord) 

# 5. Visualize PCA components using plot() and biplot()
plot(pca_result, choix = "var") 
plot(pca_result, choix = "ind")

# 5. Visualize PCA components using factoextra's biplot
fviz_pca_biplot(pca_result, repel = TRUE)


# 6. Create a scree plot to determine the optimal number of principal components
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# 7. Select the desired number of components (n_comp) based on the scree plot
n_comp <- 2 

# 8. Perform PCA with the selected components
pca_selected <- PCA(data[, sapply(data, is.numeric)], ncp = n_comp, graph = FALSE)

# 9. Plot the selected PCA components
plot(pca_selected, choix = "var")
plot(pca_selected, choix = "ind")

# 10. Interpret the results using summary()
summary(pca_selected)
