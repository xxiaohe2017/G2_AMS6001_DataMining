#######################################
#       AMS6001 Group Project     
#
#       Group 2: FUNG Kam Man /LIU Tong/ 
#                TIAN Donghao/ XIAO Xin/ 
#                HE Xiaoxiao
#             Nov 2023
#######################################

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(mlbench)
library(caret)
library(rpart)

#######################################
#     I. Data Preparation     
#######################################

# Load data 
setwd("~/Projects/HSUHK/AMS6001/Proj/Data/")
heart <- read.csv("heart.csv")
summary(heart)

# Remove missing and duplicate data
clean.data <- heart %>% drop_na() %>% unique()
summary(clean.data)

#######################################
#     II. EDA     
#######################################

## 1. Univariate distribution

# Set up the grid of subplots
par(mar = c(2, 4, 2, 1))
par(mfrow = c(4, 4))  # Creates a 4x4 grid of subplots

# Loop through each variable and plot its distribution
for (i in 1:ncol(clean.data)) {
  hist(clean.data[, i], main = colnames(clean.data)[i], col = "lightblue")
}

# Reset the plotting parameters to default
par(mfrow = c(1, 1))

## 2. Bivariate relationship between numerical attributes

# Select a subset of variables for the scatter plot matrix
# Create the scatter plot matrix
selected_vars <- c("age", "chol", "trtbps", "thalachh", "oldpeak")
pairs(heart_data[selected_vars], pch = 19)

selected_vars <- c("age", "chol", "trtbps", "thalachh", "oldpeak","output")
heart_tibble <- as_tibble(heart_data[selected_vars])
heart_tibble$output <- as.factor(heart_tibble$output)
ggpairs(heart_tibble, aes(color = output))

