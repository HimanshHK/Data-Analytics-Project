# Loading necessary libraries
library(tidyverse)
library(corrplot)

# Data Pre-processing
# Loading the dataset
heart_data <- read.csv("heart.csv")

# Checking the first few rows of the dataset
head(heart_data)

# Checking for any missing values
null_val <- sum(is.na(heart_data))
print(paste("Number of rows with missing values: ", null_val))

# Removing rows with missing values (if any)
heart_data <- na.omit(heart_data)

# Accessing the required 4 fields.
heart_data <- heart_data[, c("trtbps", "chol", "cp", "restecg", "output")]

# Checking the first few rows of the new dataset
head(heart_data)

##Data Exploration
# Visualizing the distribution of selected attributes
ggplot(heart_data, aes(x = trtbps)) + geom_histogram(binwidth = 10, fill = "blue", color = "white") + labs(title = "Distribution of Resting Blood Pressure", x = "Resting Blood Pressure", y = "Frequency")
ggplot(heart_data, aes(x = chol)) + geom_histogram(binwidth = 10, fill = "red", color = "white") + labs(title = "Distribution of Cholesterol", x = "Cholesterol", y = "Frequency")
ggplot(heart_data, aes(x = as.factor(cp))) + geom_bar(fill = "green") + labs(title = "Distribution of Chest Pain Type", x = "Chest Pain Type", y = "Frequency")
ggplot(heart_data, aes(x = as.factor(restecg))) + geom_bar(fill = "purple") + labs(title = "Distribution of Resting Electrocardiographic Results", x = "Resting Electrocardiographic Results", y = "Frequency")

# Correlation 
correlation_matrix <- cor(heart_data)

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Simple Linear Regression
# Names of the variables in the dataset
print(names(heart_data))

# Simple Linear Regression for Resting Blood Pressure
model_trtbps <- lm(output ~ trtbps, data = heart_data)
summary(model_trtbps)

# Simple Linear Regression for Cholesterol
model_chol <- lm(output ~ chol, data = heart_data)
summary(model_chol)

# Simple Linear Regression for Chest Pain Type
model_cp <- lm(output ~ as.factor(cp), data = heart_data)
summary(model_cp)

# Simple Linear Regression for Resting Electrocardiographic Results
model_restecg <- lm(output ~ as.factor(restecg), data = heart_data)
summary(model_restecg)

# Calculate R^2 values
# R^2 value for Resting Blood Pressure model
r_squared_trtbps <- summary(model_trtbps)$r.squared
print(paste("R^2 for Resting Blood Pressure: ", r_squared_trtbps))

# R^2 value for Cholesterol model
r_squared_chol <- summary(model_chol)$r.squared
print(paste("R^2 for Cholesterol: ", r_squared_chol))

# R^2 value for Chest Pain Type model
r_squared_cp <- summary(model_cp)$r.squared
print(paste("R^2 for Chest Pain Type: ", r_squared_cp))

# R^2 value for Resting Electrocardiographic Results model
r_squared_restecg <- summary(model_restecg)$r.squared
print(paste("R^2 for Resting Electrocardiographic Results: ", r_squared_restecg))
