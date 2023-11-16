# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
#regsubsets for BACKWARD selection
install.packages("leaps")
library(leaps)
# Lasso
install.packages("glmnet")
library(ISLR)
library(glmnet)

# Read the dataset
heartdata <- read.csv("Heart.csv")

# Data Cleaning

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)  # no missing value

# Handle missing values if any (e.g., imputation)
#heartdata<- na.omit(heartdata)
#df <- subset(heartdata, !(rowSums(heartdata == 0) == ncol(heartdata)))
# Data Analysis

# Summary statistics
summary(heartdata)

# Data Mining (Model Building)

# Split the data into training and testing sets
set.seed(12345) # for reproducibility
train_indices <- sample(1:nrow(heartdata), 0.7 * nrow(heartdata))
train_data <- heartdata[train_indices, ]
test_data <- heartdata[-train_indices, ]

# Build a predictive model (e.g., logistic regression, decision tree, random forest)
# Example using logistic regression use all predictors
model_LG <- glm(output ~ age + sex + exng + caa + cp + trtbps + chol + fbs + restecg +
               thalachh + slp + oldpeak + thall, data = train_data, family = "binomial")

# Model Evaluation
predicted_output <- predict(model_LG, newdata = test_data, type = "response")
predicted_labels <- ifelse(predicted_output > 0.5, 1, 0)
accuracy <- mean(predicted_labels == test_data$output)
print(paste("Accuracy:", accuracy))
# Accuracy: 0.8462


# Perform additional data mining tasks (e.g., feature selection, model comparison)
#Select a model with BACKWARD selection approach.
result.all <-regsubsets(output ~ age + sex + exng + caa + cp + trtbps + chol + fbs + restecg +
     thalachh + slp + oldpeak + thall, data = train_data, method = "backward")
summary(result.all)
summary(result.all)$bic

plot( summary(result.all)$bic, type="l", ylab="BIC")
# select 7 predictors when BIC is smallest
model_bw <-glm(output ~  sex + exng + caa + cp + thalachh + oldpeak 
               + thall, data = train_data, family = "binomial")
predicted_bw <- predict(model_bw, newdata = test_data, type = "response")
predictedbw_labels <- ifelse(predicted_bw > 0.5, 1, 0)
accuracy_bw <- mean(predictedbw_labels == test_data$output)
# accuracy_bw  0.8131

# LASSO regression cross-validation 10 folds
x=model.matrix(output~., train_data)[,-1]       # Get the design matrix. Dummy variables for categorical data are added. The column of ones (corresponding to the intercept term) is removed.
y=train_data$output

cv.out = cv.glmnet(x,y,alpha=1,nfolds = 10)
bestlam = cv.out$lambda.min
coef = predict(cv.out ,type="coefficients",s=bestlam)
coef 
# remove chol or fbs 
sub_model <- glm(output ~  age + sex + exng + cp + trtbps +restecg + thalachh + oldpeak +fbs 
                 +slp +caa + thall , data = train_data, family = "binomial")
predicted_LS <- predict(sub_model, newdata = test_data, type = "response")
predictedLS_labels <- ifelse(predicted_LS > 0.5, 1, 0)
accuracy_LS <- mean(predictedLS_labels == test_data$output)
# accuracy_LS   0.8462

# try standardizing X before using cv.glmnet to do LASSO
x1=scale(x)     

cv_s.out = cv.glmnet(x1,y,alpha=1,nfolds = 10)
bestlam_s = cv_s.out$lambda.min
coef_s = predict(cv_s.out ,type="coefficients",s=bestlam_s)
coef_s
# also remove chol or fbs,  no improvement



# Finalize the model for deployment
final_model <- glm(output ~  age + sex + exng + cp + trtbps +restecg + thalachh + oldpeak +fbs 
                 +slp +caa + thall , data = train_data, family = "binomial")

# Optional: Export the cleaned and analyzed dataset for future use
# write.csv(cleaned_data, "cleaned_heart_data.csv")