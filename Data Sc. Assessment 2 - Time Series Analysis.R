# Installing the necessary libraries 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("forecast")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("randomForest")
install.packages("tseries")
install.packages("e1071")
install.packages("dplyr")
install.packages("outliers")
install.packages("DescTools")
install.packages("tidyr")
install.packages("imputeTS")

library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(tseries)
library(e1071)
library(dplyr)
library(outliers)
library(DescTools)
library(tidyr)
library(imputeTS)

#Create work directory using dataset

setwd("C:\\Users\\showict\\Desktop\\SCHOOL\\Data Science\\Second Semester\\")
mydf <- read.csv("Assessment2 Dataset.csv", header = FALSE)
mydf


##EXPLORATORY DATA ANALYSIS

#Checking the structure and summary of the data
str(mydf)

summary(mydf)

#Viewing the dataset
head(mydf)
head(colnames(mydf), 5)


#checking for missing values

na_values <- sum(is.na(mydf))
na_values

# Create a data frame with missing value count per column
na_data <- data.frame(
  variable = colnames(mydf),
  na_values = colSums(is.na(mydf))
)

### VISUALIZATION
# Create a barplot
ggplot(na_data, aes(x = variable, y = na_values)) +
  geom_point(stat = "identity", fill = "purple") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "NA Values by Variable", y = "NA Values by count")

### Replacing missing values by using interpolation
# Convert data frame to time series
ts_mydf <- ts(mydf, start = 1, frequency = 1)


# Replace missing values using linear interpolation
ts_mydf_int <- na_interpolation(ts_mydf, option = "linear")


# Convert time series back to data frame
mydf_int <- as.data.frame(ts_mydf_int)
sum(is.na(mydf_int))



#Detecting outliers using Zscore

mydf_outlier <- scale(mydf_int)
z_score <- abs(mydf_outlier)
threshold <- 2
outliers <- which(z_score >threshold,arr.ind=TRUE)
print(outliers)

### VISUALIZATION
# Total number of outliers
outliers_count <- length(outliers[,1])
cat("Total number of outliers detected:", outliers_count)


# Create the ggplot visualization for outliers

ggplot(data = data.frame(outliers), aes(x = row, y = col)) +
  geom_point(color = "green", size = 2) +
  labs(title = "Outliers Detected by Z-Score", x = "Row Index", y = "Column Index") +
  theme_minimal()


# Handling outliers using Winsorization

winsorized_data <- apply(mydf_int,2,Winsorize,probs=c(0.05,0.95))
winsorized_data


# check the difference between original table and winsorized data

difference <- winsorized_data - mydf
difference

# Summary of original data
summary(mydf)

# Summary of the Winsorized data
summary(winsorized_data)



#Insert header
mydf_header<- read.csv("Assessment2 header.csv",header= FALSE)

#Merge both header and body
new_mydf <- rbind(mydf_header,winsorized_data)


#view final dataset
View(new_mydf)

###

#Assigning the first row as the header
names(new_mydf) <- as.matrix(new_mydf[1, ])
#removing the first row
new_mydf <- new_mydf[-1, ]
#convert columns to their appropriate data types
new_mydf[] <- lapply(new_mydf, function(x) type.convert(as.character(x)))
new_mydf


#Select row for latitude

lat_row <- new_mydf[161, ]
View(lat_row)

#Extract Soil moisture (SMOIS) columns for that row

smois_columns <- grep("SMOIS", names(lat_row), ignore.case = TRUE)
smois_values <- unlist(lat_row[smois_columns])


# Define the starting date and time
start_date <- as.POSIXct("2018-05-01 00:00:00", tz = "UTC")


# Calculate the total number of SMOIS measurements in the desired row
num_intervals <- length(smois_values)

# Generate a sequence of 3-hour intervals
time_seq <- seq(from = start_date, by = "3 hours", length.out = num_intervals)



# Create a new data frame with date and time intervals and SMOIS values
mydf_final <- data.frame(DateTime = time_seq, SMOIS = smois_values)
View(mydf_final)


#Create time stamp column
mydf_final$TimeStamp <- seq_len(nrow(mydf_final))
print(mydf_final)
View(mydf_final)


### VISUALIZATION
#Time Series Plot

ggplot(mydf_final, aes(x = DateTime, y = SMOIS)) +
  geom_line() +
  labs(title = "Soil Moisture (SMOIS) Over Time",
       x = "Date and Time",
       y = "Soil Moisture (SMOIS)") +
  theme_minimal()

# Univariate Analysis

summary(mydf_final$SMOIS)

#histogram
ggplot(mydf_final, aes(x = SMOIS)) +
  geom_histogram(binwidth = 2, color = "black", fill = "pink") +
  labs(title = "Histogram of Soil Moisture (SMOIS)",
       x = "Soil Moisture (SMOIS)",
       y = "Frequency") +
  theme_minimal()

#barplot
ggplot(mydf_final, aes(x = SMOIS)) +
  geom_bar(binwidth = 2, color = "black", fill = "pink") +
  labs(title = "Bar Plot of Soil Moisture (SMOIS)",
       x = "Soil Moisture (SMOIS)",
       y = "Frequency") +
  theme_minimal()

#boxplot
ggplot(mydf_final, aes(x = "", y = SMOIS)) +
  geom_boxplot(color = "black", fill = "pink") +
  labs(title = "Box Plot of Soil Moisture (SMOIS)",
       x = "",
       y = "Soil Moisture (SMOIS)") +
  theme_minimal()




####STATISTICAL AND MACHINE LEARNING MODELS
#Divide data into train and test

n_rows <- nrow(mydf_final)
train_size <- floor(0.8 * n_rows)
set.seed(123) # Set a seed for reproducibility
index <- sample(seq_len(n_rows), size = train_size)
train_data <- mydf_final[index, ]
test_data <- mydf_final[-index, ]
str(train_data)
str(test_data)

##ARIMA MODEL
# Fit the ARIMA model
arima_model <- auto.arima(train_data$SMOIS, seasonal = TRUE, stepwise = TRUE)


# Forecast using the ARIMA model
arima_forecast <- forecast(arima_model, h=length(train_data$SMOIS))

print(arima_model)

#Calculate Root Mean Squared Error and display performance metrics
rmse_arima <- sqrt(mean((test_data$SMOIS - arima_forecast$mean)^2))
cat("ARIMA RMSE:", rmse_arima)



###LINERA REGRESSION
train_model <- lm(SMOIS ~TimeStamp, data = train_data)
summary(train_model)

# Predict SMOIS values for the test set

predictions <- predict(train_model, newdata = test_data)


# Calculate the Root Mean Squared Error (RMSE)

linear_rmse <- sqrt(mean((test_data$SMOIS - predictions)^2))
cat("LINEAR RMSE:", linear_rmse)


# Plot Actual vs Predicted values

ggplot() +
  geom_point(data = test_data, aes(x = SMOIS, y = predictions), color = "pink") +
  geom_abline(slope = 1, intercept = 0, color = "purple") +
  labs(title = "Actual vs. Predicted SMOIS ",
       x = "Actual SMOIS ",
       y = "Predicted SMOIS ") +
  theme_minimal()


###SUPORT VECTOR REGRESSION (SVR)

##SVR - RADIAL
svr_model <- svm(SMOIS ~ TimeStamp, data = train_data, kernel = "radial")
#Display the SVR model summary (summary takes a long time, skip till presentation)
summary(svr_model) 

#Model Evaluation and Comparison:

#Predict SMOIS values for the test set using the SVR model
svr_predictions <- predict(svr_model, newdata = test_data)

#Calculate the Root Mean Squared Error (RMSE) for the SVR model
svr_rmse_radial <- sqrt(mean((test_data$SMOIS - svr_predictions)^2))
cat("SVR_RADIAL RMSE:", svr_rmse_radial)


##SVR - POLY

# Fit an SVR model on the training set
svr_model2 <- svm(SMOIS ~ TimeStamp, data = train_data, kernel = "poly")
# Display the SVR model summary (summary takes a long time, skip till presentation)
summary(svr_model2) 


# Predict SMOIS values for the test set using the SVR model
svr_predictions2 <- predict(svr_model2, newdata = test_data)

#Calculate the Root Mean Squared Error (RMSE) for the SVR model
svr_rmse_poly <- sqrt(mean((test_data$SMOIS - svr_predictions2)^2))
cat("SVR_POLY RMSE:", svr_rmse_poly)



##SVR - LINEAR

# Fit an SVR model on the training set
svr_model3 <- svm(SMOIS ~ TimeStamp, data = train_data, kernel = "linear")
# Display the SVR model summary (summary takes a long time, skip till presentation)
summary(svr_model3)


# Predict SMOIS values for the test set using the SVR model
svr_predictions3 <- predict(svr_model3, newdata = test_data)

#Calculate the Root Mean Squared Error (RMSE) for the SVR model
svr_rmse_linear <- sqrt(mean((test_data$SMOIS - svr_predictions3)^2))
cat("SVR_LINEAR RMSE:", svr_rmse_linear)



###RANDOM FOREST

## ntree = 100

# Fit Random Forest model on the training set ntree =100
rf_model_100 <- randomForest(SMOIS ~ TimeStamp, data = train_data, ntree = 100)
# Display the Random Forest model summary
summary(rf_model_100)
# Predict SMOIS values for the test set using the Random Forest model
rf_predictions_100 <- predict(rf_model_100, newdata = test_data)


# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_100 <- sqrt(mean((test_data$SMOIS - rf_predictions_100)^2))
cat("Random Forest RMSE (ntree = 100):", rf_rmse_100)



## ntree = 200

# Fit a Random Forest model on the training set ntree =100
rf_model_200 <- randomForest(SMOIS ~ TimeStamp, data = train_data, ntree = 200)
# Display the Random Forest model summary
summary(rf_model_200)
# Predict TSK values for the test set using the Random Forest model
rf_predictions_200 <- predict(rf_model_200, newdata = test_data)


# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_200 <- sqrt(mean((test_data$SMOIS - rf_predictions_200)^2))
cat("Random Forest RMSE (ntree = 200):", rf_rmse_200)



## ntree = 500

# Fit a Random Forest model on the training set ntree =100
rf_model_500 <- randomForest(SMOIS ~ TimeStamp, data = train_data, ntree = 500)
# Display the Random Forest model summary
summary(rf_model_500)
# Predict TSK values for the test set using the Random Forest model
rf_predictions_500 <- predict(rf_model_500, newdata = test_data)


# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_500 <- sqrt(mean((test_data$SMOIS - rf_predictions_500)^2))
cat("Random Forest RMSE (ntree = 500):", rf_rmse_500)



# Compare the RMSE values of the Linear Regression, SVR, and Random Forest models
cat("\nLINEAR RMSE:", linear_rmse)
cat("\nSVR_RADIAL RMSE:", svr_rmse_radial)
cat("\nRandom Forest RMSE (ntree = 500):", rf_rmse_500)




# Plot the actual vs. predicted values for the Linear Regression, SVR, and Random Forest models
p1 <- ggplot() +
  geom_point(data = test_data, aes(x = SMOIS, y = predictions), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "cyan") +
  labs(title = "Linear Regression: Actual vs. Predicted SMOIS ",
       x = "Actual SMOIS ",
       y = "Predicted SMOIS ") +
  theme_minimal()

p1



p2 <- ggplot() +
  geom_point(data = test_data, aes(x = SMOIS, y = svr_predictions), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "SVR_RADIAL: Actual vs. Predicted SMOIS ",
       x = "Actual SMOIS ",
       y = "Predicted SMOIS ") +
  theme_minimal()

p2



p3 <- ggplot() +
  geom_point(data = test_data, aes(x = SMOIS, y = rf_predictions_500), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  labs(title = "Random Forest(ntree = 500): Actual vs. Predicted SMOIS ",
       x = "Actual SMOIS ",
       y = "Predicted SMOIS ") +
  theme_minimal()

p3



grid.arrange(p1, p2, p3, ncol = 3)




# Create a dataframe with the RMSE values for ARIMA, Linear Regression, SVR Radial, and Random Forest (ntree=500)

rmse_comparison <- data.frame(
  
  Model = c("ARIMA", "Linear Regression", "SVR Radial", "SVR Poly", "SVR Linear", 
            "Random Forest (ntree=100)", "Random Forest (ntree=200)", "Random Forest (ntree=500)"),
  
  RMSE = c(rmse_arima, linear_rmse, svr_rmse_radial, svr_rmse_poly, svr_rmse_linear,
           rf_rmse_100, rf_rmse_200, rf_rmse_500)
  
)

rmse_comparison



# Bar chart to visualize the comparison

ggplot(rmse_comparison, aes(x = Model, y = RMSE, fill = Model)) +
  
  geom_bar(stat = "identity", width = 0.5) +
  
  labs(title = "Root Mean Squared Error for Selected Models",
       
       x = "Model",
       
       y = "Root Mean Squared Error") +
  
  theme_minimal() +
  
  scale_fill_manual(values = c("ARIMA" = "blue", "Linear Regression" = "green", 
                               "SVR Radial" = "red", 
                               "Random Forest (ntree=100)" = "purple")) +
  
  theme(legend.position = "none")



####













