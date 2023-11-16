# Load required library
library(cluster)

# Read the dataset
heart_data <- read.csv("Heart.csv")

# Data Cleaning

# Check for missing values
missing_values <- colSums(is.na(heart_data))
print(missing_values)

# Handle missing values if any (e.g., imputation)

# Data Analysis

# Summary statistics
summary(heart_data)

# Explore the distribution of variables
hist(heart_data$age, col = "lightblue", main = "Distribution of Age", xlab = "Age")

# Explore the relationship between variables
plot(heart_data$age, heart_data$chol, col = heart_data$output, pch = 16,
     main = "Age vs. Cholesterol by Output", xlab = "Age", ylab = "Cholesterol")
legend("topright", legend = c("Less chance", "More chance"), col = c(1, 2), pch = 16)

# Explore categorical variables
table(heart_data$sex)
table(heart_data$exng)

# Perform additional data cleaning and analysis as needed

# Data Mining (Clustering)

# Prepare the data for clustering (e.g., feature encoding, normalization)

# Perform K-means clustering
k <- 3  # number of clusters
cluster_result <- kmeans(heart_data[, -14], centers = k)

# Cluster Analysis

# Analyze the cluster results
print(cluster_result)

# Visualize the clusters
plot(heart_data$age, heart_data$chol, col = cluster_result$cluster, pch = 16,
     main = "Age vs. Cholesterol by Clusters", xlab = "Age", ylab = "Cholesterol")
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 16)

# Further cluster analysis and refinement as needed

# Perform additional data mining tasks (e.g., dimensionality reduction, cluster validation)

# Finalize the clustering results

# Optional: Export the cleaned and analyzed dataset for future use
# write.csv(cleaned_data, "cleaned_heart_data.csv")