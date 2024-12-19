# 1. Load the titanic dataset from R.
data <- read.csv("titanic.csv")
# 2. Data Preparation and data cleaning of titanic dataset
str(data)
data <- na.omit(data)
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
# 3. Create a 2 way contingency table - all the possibility of Categorical-Categorical, numerical-numerical, Categorical-numerical variables.
table_cat_cat <- table(data$Survived, data$Sex)
table_cat_cat
table_num_num <- table(cut(data$Age, breaks=seq(0, 80, by=10)), 
                       cut(data$Fare, breaks=seq(0, 500, by=50)))
table_num_num
table_cat_num <- table(data$Pclass, cut(data$Age, breaks=seq(0, 80, by=10)))
table_cat_num
# 4. Create a 3 way contingency table for the Q3.
table_3way <- table(data$Survived, data$Pclass, data$Sex)
table_3way
# 5. For Q3 any one contingency table, apply row profile dataset, column profile dataset, relative frequency dataset, Chi-square dataset.
row_profile <- prop.table(table_3way, margin = 1)
row_profile
col_profile <- prop.table(table_3way, margin = 2)
col_profile
rel_freq <- prop.table(table_3way)
rel_freq
if (all(table_3way > 0)) {  # Check for non-zero counts
  chi_sq <- chisq.test(as.matrix(table_3way))
} else {
  stop("Table contains zero counts, cannot perform Chi-square test.")
}
chi_sq
# 6. Display the scatter plot using ggplot.
library(ggplot2)

ggplot(data, aes(x=Age, y=Fare)) +
  geom_point() +
  theme_minimal() +
  labs(title="Scatter Plot of Age vs Fare", x="Age", y="Fare")
# 7. Display the scatter plot for 3 variables using ggplot. (load the library 'scatterplot3d')
library(scatterplot3d)

s3d <- scatterplot3d(data$Age, data$Fare, data$Survived, 
                     pch=19, color=data$Survived, 
                     main="3D Scatter Plot of Age, Fare, and Survival")
# 8. For Q3, apply the possibility of changing color, shape, add bars (type='h') horizontal.
ggplot(data, aes(x=Age, y=Fare, color=Survived, shape=Sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed", color = "gray") +
  theme_minimal() +
  labs(title="Scatter Plot with Color and Shape", x="Age", y="Fare")
# 9. Display 3D barplot
library(plotly)
library(dplyr)  
library(RColorBrewer) 

summary_data <- data %>%
  group_by(Pclass, Survived) %>%
  summarise(Count = n(), .groups = 'drop')

plot_ly(data = summary_data, 
        x = ~Pclass, 
        y = ~Count, 
        z = ~Survived, 
        type = "bar", 
        color = ~Survived, 
        colors = brewer.pal(3, "Set1")) %>%
  layout(title = "Counts of Pclass by Survival",
         scene = list(
           xaxis = list(title = "Pclass"),
           yaxis = list(title = "Count"),
           zaxis = list(title = "Survived")
         ))

# 10. Display 2D boxplot (one variable as Categorical)
ggplot(data, aes(x=Survived, y=Age)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Boxplot of Age by Survival", x="Survived", y="Age")
# 11. Load the library file 'fmsb' and display the sunray plot (in R use radarchart())
library(fmsb)

data_radar <- as.data.frame(matrix(0, nrow=3, ncol=5))
colnames(data_radar) <- c("Criteria1", "Criteria2", "Criteria3", "Criteria4", "Criteria5")
data_radar[1,] <- c(1, 1, 1, 1, 1)
data_radar[2,] <- c(0.5, 0.5, 0.5, 0.5, 0.5)
data_radar[3,] <- c(0, 0, 0, 0, 0)

radarchart(data_radar)






