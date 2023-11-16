# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

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
ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Age", y = "Count", title = "Distribution of Age")

# Explore the relationship between variables
ggplot(heart_data, aes(x = age, y = chol, color = output)) +
  geom_point() +
  labs(x = "Age", y = "Cholesterol", title = "Age vs. Cholesterol by Output")

# Explore categorical variables
table(heart_data$sex)
table(heart_data$exng)

# Perform additional data cleaning and analysis as needed

# Data Mining (Model Building)

# Prepare the data for modeling (e.g., feature encoding, normalization)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(heart_data), 0.7 * nrow(heart_data))
train_data <- heart_data[train_indices, ]
test_data <- heart_data[-train_indices, ]

# Build a predictive model (e.g., logistic regression, decision tree, random forest)
# Example using logistic regression
model <- glm(output ~ age + sex + exng + caa + cp + trtbps + chol + fbs + restecg +
               thalachh + slp + oldpeak + thall, data = train_data, family = "binomial")

# Model Evaluation
predicted_output <- predict(model, newdata = test_data, type = "response")
predicted_labels <- ifelse(predicted_output > 0.5, 1, 0)
accuracy <- mean(predicted_labels == test_data$output)
print(paste("Accuracy:", accuracy))

# Further model evaluation and refinement as needed

# Perform additional data mining tasks (e.g., feature selection, model comparison)

# Finalize the model for deployment

# Optional: Export the cleaned and analyzed dataset for future use
# write.csv(cleaned_data, "cleaned_heart_data.csv")