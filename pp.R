# Install and load required packages
install.packages(c("tidyverse", "cluster", "ggplot2"))
library(tidyverse)
library(cluster)
library(ggplot2)

# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)
customer_data <- read.csv("/Users/shubhankardutta/Downloads/Mall_Customers.csv")

# Explore the dataset (optional)
summary(customer_data)
str(customer_data)

# Remove leading and trailing spaces from column names
colnames(customer_data) <- trimws(colnames(customer_data))
# Print the column names
print(colnames(customer_data))

# Select relevant columns for clustering (adjust based on your dataset)
selected_columns <- customer_data[, c("Age", "Annual.Income..k..", "Spending.Score..1.100."), drop = FALSE]


# Standardize the data (important for k-means)
standardized_data <- scale(selected_columns)

# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k) sum(kmeans(standardized_data, centers = k)$withinss))
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Based on the plot, choose the optimal number of clusters
k_optimal <- 3  # Adjust as per the elbow point in your specific case

# Perform k-means clustering
kmeans_result <- kmeans(standardized_data, centers = k_optimal)

# Add the cluster assignments to the original dataset
customer_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters in a scatter plot
ggplot(customer_data, aes(x = `Annual.Income..k..`, y = `Spending.Score..1.100.`, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation",
       x = "Annual Income (k$)",
       y = "Spending Score (1-100)",
       color = "Cluster") +
  theme_minimal()

# Analyze each segment's characteristics
segment_characteristics <- customer_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Age = mean(Age),
    Avg_Annual_Income = mean(`Annual.Income..k..`),
    Avg_Spending_Score = mean(`Spending.Score..1.100.`),
    Total_Customers = n()
  )

# Display the characteristics of each segment
print(segment_characteristics)

# Tailor marketing strategies based on the characteristics
# This is just a basic example; you may need to customize based on your analysis
for (i in 1:k_optimal) {
  cat("Marketing Strategy for Cluster", i, ":\n")
  if (segment_characteristics$Avg_Annual_Income[i] > 0) {
    cat("   - Focus on customers with higher annual income\n")
  } else {
    cat("   - Encourage higher annual spending for this segment\n")
  }
  
  if (segment_characteristics$Avg_Spending_Score[i] > 0) {
    cat("   - Target customers with higher spending scores\n")
  } else {
    cat("   - Incentivize increased spending for this segment\n")
  }
  
  cat("\n")
}
