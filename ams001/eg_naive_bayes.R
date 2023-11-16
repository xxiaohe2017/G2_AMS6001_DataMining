# Load required library
library(e1071)

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

# Data Mining (Model Building)

# Prepare the data for modeling (e.g., feature encoding, normalization)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(heart_data), 0.7 * nrow(heart_data))
train_data <- heart_data[train_indices, ]
test_data <- heart_data[-train_indices, ]

# Build a Naive Bayes model
model <- naiveBayes(output ~ age + sex + exng + caa + cp + trtbps + chol + fbs + restecg +
                      thalachh + slp + oldpeak + thall, data = train_data)

# Model Evaluation
predicted_output <- predict(model, newdata = test_data)
accuracy <- sum(predicted_output == test_data$output) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# Further model evaluation and refinement as needed

# Perform additional data mining tasks (e.g., feature selection, model comparison)

# Finalize the model for deployment

# Optional: Export the cleaned and analyzed dataset for future use
# write.csv(cleaned_data, "cleaned_heart_data.csv")