# install libraries

# install.packages("matlib")
# install.packages(c("ggplot2", "rsample"))

#import libraries
library(matlib)
library(ggplot2)
library(rsample)


# import data file
DATA_CSV =read.csv(file="E:/Softwarica/Stat/Assignment/data.csv",header = T)
DATA_CSV

# Task 1

# Task 1.1 Time Series Plot
# Time Series Plot
colnames(DATA_CSV) <- c('time', 'x1', 'x2', 'x3', 'x4', 'x5')

#separate columns from CSV
time <- DATA_CSV$time
x1 <- DATA_CSV$x1
x2 <-DATA_CSV$x2
x3 <-DATA_CSV$x3
x4 <-DATA_CSV$x4
x5 <-DATA_CSV$x5

ts_x1 <- ts(x1, start = min(time), frequency = 1/(time[2] - time[1]))
ts_x2 <- ts(x2, start = min(time), frequency = 1/(time[2] - time[1]))
ts_x3 <- ts(x3, start = min(time), frequency = 1/(time[2] - time[1]))
ts_x4 <- ts(x4, start = min(time), frequency = 1/(time[2] - time[1]))
ts_x5 <- ts(x5, start = min(time), frequency = 1/(time[2] - time[1]))

ts_data <- data.frame(
  Time = time,
  x1 = as.numeric(ts_x1),
  x2 = as.numeric(ts_x2),
  x3 = as.numeric(ts_x3),
  x4 = as.numeric(ts_x4),
  x5 = as.numeric(ts_x5)
)

# Time Against gene x1
plot(ts_x1, type = "l", col = "black", ylim = range(ts_data[,-1]), ylab = "X1 Values", xlab = "Time (min)")


# Time Against gene x2
plot(ts_x2, type = "l", col = "black", ylim = range(ts_data[,-1]), ylab = "X2 Values", xlab = "Time (min)")


# Time Against gene x3
plot(ts_x3, type = "l", col = "black", ylim = range(ts_data[,-1]), ylab = "X3 Values", xlab = "Time (min)")


# Time Against gene x4
plot(ts_x4, type = "l", col = "black", ylim = range(ts_data[,-1]), ylab = "X4 Values", xlab = "Time (min)")


# Time Against gene x5
plot(ts_x5, type = "l", col = "black", ylim = range(ts_data[,-1]), ylab = "X5 Values", xlab = "Time (min)")

# Histograms

# Histogram for x1
# Plot histogram with density
hist(x1, freq = FALSE, main = "Histogram of x1 with Density Plot", xlab = "x1", , ylim = c(0, 1.1))

# Plot mean line 
abline(v = mean(x1), lwd = 2, col = "blue")

# Add text annotations for mean and standard deviation
text(mean(x1), max(hist(x1, plot = FALSE)$density) * 0.8, paste("Mean =", round(mean(x1), 2)), adj = c(1.2, -3))
text(mean(x1), max(hist(x1, plot = FALSE)$density) * 0.7, paste("S.D =", round(sd(x1), 2)), adj = c(1.2, -2.8))

# Plot density line
lines(density(x1), lwd = 2, col = "red")

# Histogram for x2
# Plot histogram with density
hist(x2, freq = FALSE, main = "Histogram of x2 with Density Plot", xlab = "x2", , ylim = c(0, 1.1))

# Plot mean line 
abline(v = mean(x2), lwd = 2, col = "blue")

# Add text annotations for mean and standard deviation
text(mean(x2), max(hist(x2, plot = FALSE)$density) * 0.8, paste("Mean =", round(mean(x2), 2)), adj = c(1.05, -3))
text(mean(x2), max(hist(x2, plot = FALSE)$density) * 0.7, paste("S.D =", round(sd(x2), 2)), adj = c(1.2, -2.8))

# Plot density line
lines(density(x2), lwd = 2, col = "red")

# Histogram for x3
# Plot histogram with density
hist(x3, freq = FALSE, main = "Histogram of x3 with Density Plot", xlab = "x3", , ylim = c(0, 1.5))

# Plot mean line 
abline(v = mean(x3), lwd = 2, col = "blue")

# Add text annotations for mean and standard deviation
text(mean(x3), max(hist(x3, plot = FALSE)$density) * 0.8, paste("Mean =", round(mean(x3), 2)), adj = c(-1, -3))
text(mean(x3), max(hist(x3, plot = FALSE)$density) * 0.7, paste("S.D =", round(sd(x3), 2)), adj = c(-1.2, -2.8))

# Plot density line
lines(density(x3), lwd = 2, col = "red")

# Histogram for x4
# Plot histogram with density
hist(x4, freq = FALSE, main = "Histogram of x4 with Density Plot", xlab = "x4", , ylim = c(0, 1.5))

# Plot mean line 
abline(v = mean(x4), lwd = 2, col = "blue")

# Add text annotations for mean and standard deviation
text(mean(x4), max(hist(x4, plot = FALSE)$density) * 0.8, paste("Mean =", round(mean(x4), 2)), adj = c(3, -3))
text(mean(x4), max(hist(x4, plot = FALSE)$density) * 0.7, paste("S.D =", round(sd(x4), 2)), adj = c(3.8, -2.8))

# Plot density line
lines(density(x4), lwd = 2, col = "red")

# Histogram for x5
# Plot histogram with density
hist(x5, freq = FALSE, main = "Histogram of x5 with Density Plot", xlab = "x5", , ylim = c(0, 1.5))

# Plot mean line 
abline(v = mean(x5), lwd = 2, col = "blue")

# Add text annotations for mean and standard deviation
text(mean(x5), max(hist(x5, plot = FALSE)$density) * 0.8, paste("Mean =", round(mean(x5), 2)), adj = c(3, -3))
text(mean(x5), max(hist(x5, plot = FALSE)$density) * 0.7, paste("S.D =", round(sd(x5), 2)), adj = c(3.1, -2.8))

# Plot density line
lines(density(x5), lwd = 2, col = "red")

# Correlation Plots
# Correlation between x1 and x2
ggplot(DATA_CSV, aes(x = x1, y = x2)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x1", y = "x2", title = "Correlation Plot of x1 and x2")

# Correlation between x1 and x3
ggplot(DATA_CSV, aes(x = x1, y = x3)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x1", y = "x3", title = "Correlation Plot of x1 and x3")

# Correlation between x1 and x4
ggplot(DATA_CSV, aes(x = x1, y = x4)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x1", y = "x4", title = "Correlation Plot of x1 and x4")

# Correlation between x1 and x5
ggplot(DATA_CSV, aes(x = x1, y = x5)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x1", y = "x5", title = "Correlation Plot of x1 and x5")

# Correlation between x2 and x3
ggplot(DATA_CSV, aes(x = x2, y = x3)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x2", y = "x5", title = "Correlation Plot of x2 and x3")

# Correlation between x2 and x4
ggplot(DATA_CSV, aes(x = x2, y = x4)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x2", y = "x4", title = "Correlation Plot of x2 and x4")

# Correlation between x2 and x5
ggplot(DATA_CSV, aes(x = x2, y = x5)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x2", y = "x5", title = "Correlation Plot of x2 and x5")

# Correlation between x3 and x4
ggplot(DATA_CSV, aes(x = x3, y = x4)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x3", y = "x4", title = "Correlation Plot of x3 and x4")

# Correlation between x3 and x5
ggplot(DATA_CSV, aes(x = x3, y = x5)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x3", y = "x5", title = "Correlation Plot of x3 and x5")

# Correlation between x4 and x5
ggplot(DATA_CSV, aes(x = x4, y = x5)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x4", y = "x5", title = "Correlation Plot of x4 and x5")

# Task 1 End

# Task 2
# Task 2.1 Least Square
onesMatrix <- matrix(1, length(x1), 1)
y <- DATA_CSV$x2

# Model 1
xModel1 <- cbind(onesMatrix, x4, x3 ^ 2)
thetaHatModel1 <- solve(t(xModel1) %*% xModel1) %*% t(xModel1) %*% y

print(thetaHatModel1)

# Model 2
xModel2 <- cbind(onesMatrix, x4, x3^2, x5)
thetaHatModel2 <- solve(t(xModel2) %*% xModel2) %*% t(xModel2) %*% y

print(thetaHatModel2)

# Model 3
xModel3 <- cbind(onesMatrix, x3, x4, x5 ^ 3)
thetaHatModel3 <- solve(t(xModel3) %*% xModel3) %*% t(xModel3) %*% y

print(thetaHatModel3)

# Model 4
xModel4 <- cbind(onesMatrix, x4, x3 ^ 2, x5 ^ 3)
thetaHatModel4 <- solve(t(xModel4) %*% xModel4) %*% t(xModel4) %*% y

print(thetaHatModel4)

# Model 5
xModel5 <- cbind(onesMatrix, x4, x1 ^ 2, x3 ^ 2)
thetaHatModel5 <- solve(t(xModel5) %*% xModel5) %*% t(xModel5) %*% y

print(thetaHatModel5)

# Task 2.2 RSS
# RSS Model 1
yHatModel1 = xModel1 %*% thetaHatModel1
RSSModel1 = sum((yHatModel1) ^ 2)

print(RSSModel1)

# RSS Model 2
yHatModel2 = xModel2 %*% thetaHatModel2
RSSModel2 = sum((yHatModel2) ^ 2)

print(RSSModel2)

# RSS Model 3
yHatModel3 = xModel3 %*% thetaHatModel3
RSSModel3 = sum((yHatModel3) ^ 2)

print(RSSModel3)

# RSS Model 4
yHatModel4 = xModel4 %*% thetaHatModel4
RSSModel4 = sum((yHatModel4) ^ 2)

print(RSSModel4)

# RSS Model 5
yHatModel5 = xModel5 %*% thetaHatModel5
RSSModel5 = sum((yHatModel5) ^ 2)

print(RSSModel5)

# Task 2.3 Log Likelihood Function

# Calculate Variance of all models
n <- length(y)
print(n)

varianceModel1 <- RSSModel1 / (n - 1)
print(varianceModel1)

varianceModel2 <- RSSModel2 / (n - 1)
print(varianceModel2)

varianceModel3 <- RSSModel3 / (n - 1)
print(varianceModel3)

varianceModel4 <- RSSModel4 / (n - 1)
print(varianceModel4)

varianceModel5 <- RSSModel5 / (n - 1)
print(varianceModel5)

# Log Likelihood of Model 1
likelihoodModel1 <- -(n / 2) *(log(2 * pi)) - (n / 2) * (log(varianceModel1)) - (1 / (2 * varianceModel1)) * RSSModel1
print(likelihoodModel1)

# Log Likelihood of Model 2
likelihoodModel2 <- -(n / 2) *(log(2 * pi)) - (n / 2) * (log(varianceModel2)) - (1 / (2 * varianceModel2)) * RSSModel2
print(likelihoodModel2)

# Log Likelihood of Model 3
likelihoodModel3 <- -(n / 2) *(log(2 * pi)) - (n / 2) * (log(varianceModel3)) - (1 / (2 * varianceModel3)) * RSSModel3
print(likelihoodModel3)

# Log Likelihood of Model 4
likelihoodModel4 <- -(n / 2) *(log(2 * pi)) - (n / 2) * (log(varianceModel4)) - (1 / (2 * varianceModel4)) * RSSModel4
print(likelihoodModel4)

# Log Likelihood of Model 5
likelihoodModel5 <- -(n / 2) *(log(2 * pi)) - (n / 2) * (log(varianceModel5)) - (1 / (2 * varianceModel5)) * RSSModel5
print(likelihoodModel5)

# Task 2.4 AIC and BIC
# AIC and BIC of Model 1
kModel1 <- length(thetaHatModel1)
print(kModel1)
AICModel1 <- 2 * kModel1 - 2 * likelihoodModel1
print(AICModel1)

BICModel1 <- kModel1 * log(n) - 2 * likelihoodModel1
print(BICModel1)

# AIC and BIC of Model 2
kModel2 <- length(thetaHatModel2)
AICModel2 <- 2 * kModel2 - 2 * likelihoodModel2
print(AICModel2)

BICModel2 <- kModel2 * log(n) - 2 * likelihoodModel2
print(BICModel2)

# AIC and BIC of Model 3
kModel3 <- length(thetaHatModel3)
AICModel3 <- 2 * kModel3 - 2 * likelihoodModel3
print(AICModel3)

BICModel3 <- kModel3 * log(n) - 2 * likelihoodModel3
print(BICModel3)

# AIC and BIC of Model 4
kModel4 <- length(thetaHatModel4)
AICModel4 <- 2 * kModel4 - 2 * likelihoodModel4
print(AICModel4)

BICModel4 <- kModel4 * log(n) - 2 * likelihoodModel4
print(BICModel4)

# AIC and BIC of Model 5
kModel5 <- length(thetaHatModel5)
AICModel5 <- 2 * kModel5 - 2 * likelihoodModel5
print(AICModel5)

BICModel5 <- kModel5 * log(n) - 2 * likelihoodModel5
print(BICModel5)

# Task 2.5 Distribution of Model Prediction errors using QQ Plot
#Model1
# Error in Model 1
errorInModel1 <- y - yHatModel1

# QQ plot and QQ line of Model 1
qqnorm(errorInModel1, col = "blue", main = "QQ Plot of Model 1")
qqline(errorInModel1, col = "red", lwd = 2)

# Error in Model 2
errorInModel2 <- y - yHatModel2

# QQ plot and QQ line of Model 2
qqnorm(errorInModel2, col = "blue", main = "QQ Plot of Model 2")
qqline(errorInModel2, col = "red", lwd = 2)

# Error in Model 3
errorInModel3 <- y - yHatModel3

# QQ plot and QQ line of Model 3
qqnorm(errorInModel3, col = "blue", main = "QQ Plot of Model 3")
qqline(errorInModel3, col = "red", lwd = 2)

# Error in Model 4
errorInModel4 <- y - yHatModel4

# QQ plot and QQ line of Model 4
qqnorm(errorInModel4, col = "blue", main = "QQ Plot of Model 4")
qqline(errorInModel4, col = "red", lwd = 2)

# Error in Model 5
errorInModel5 <- y - yHatModel5

# QQ plot and QQ line of Model 5
qqnorm(errorInModel5, col = "blue", main = "QQ Plot of Model 5")
qqline(errorInModel5, col = "red", lwd = 2)


# Task 2.7
# 1 Dividing data into training and testing sets
x <- data.frame(DATA_CSV$time, DATA_CSV$x2, DATA_CSV$x1,  DATA_CSV$x3, DATA_CSV$x4, DATA_CSV$x5)
set.seed(123)

splitX <- initial_split(data = as.data.frame(x), prop = 0.7)

print(splitX)

xTrainingSet <- training(splitX)
xTestingSet <- testing(splitX)

yTrainingSet <- xTrainingSet$DATA_CSV.x2

# Printing training and testing set of x
print(xTrainingSet)
print(xTestingSet)

# 2 Create design matrix for Selected Model

trainingOnes <- matrix(1, nrow = nrow(xTrainingSet), ncol = 1)

trainingX1 <- xTrainingSet$DATA_CSV.x1        
trainingX4 <- xTrainingSet$DATA_CSV.x4 
trainingX3 <- xTrainingSet$DATA_CSV.x3

xTrainingModel <- cbind(trainingOnes, trainingX4, trainingX1 ^ 2, trainingX3 ^ 2)

thetaHat <- solve(t(xTrainingModel) %*% xTrainingModel) %*% t(xTrainingModel) %*% yTrainingSet

print(thetaHat)

# Calculating RSS of Selected Model
testingX <- as.matrix(xTestingSet)
print(testingX)
testingX <- testingX[, -1] 
testingX <- testingX[, -1] 

testingX1 <- xTestingSet$DATA_CSV.x1        
testingX4 <- xTestingSet$DATA_CSV.x4 
testingX3 <- xTestingSet$DATA_CSV.x3

xTestingModel <-  cbind(1, testingX4, testingX1 ^ 2, testingX3 ^ 2)

testingYHat <- xTestingModel %*% thetaHat
testingY <- xTestingSet$DATA_CSV.x2

print(testingY)

# Compute the residuals and RSS for the testing data
residuals_test <- testingY - testingYHat
print(residuals_test)
RSS_test <- sum(residuals_test^2)
print(RSS_test)
n_test <- length(testingY)
p <- length(thetaHat)
print(p)
sigma2 <- RSS_test / (n_test - p)
print(sigma2)

t_critical <- qt(0.95, df = n_test - p)
se_fit <- sqrt(diag(xTestingModel %*% solve(t(xTestingModel) %*% xTestingModel) %*% t(xTestingModel)) * sigma2)
ci_upper <- testingYHat + t_critical * se_fit
ci_lower <- testingYHat - t_critical * se_fit


print(testingYHat)
print(yTestingSet)
# Create a data frame for plotting
plot_data <- data.frame(
  Time = xTestingSet$DATA_CSV.time,
  Observed = testingY,
  Predicted = testingYHat,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

print(plot_data)

# Plot the observed values, predicted values, and confidence intervals with a legend
ggplot(plot_data, aes(x = Time)) +
  geom_point(aes(y = Observed, color = "Observed"), size = 2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.2) +
  labs(title = "Model Predictions with 95% Confidence Intervals",
       x = "Time",
       y = "x2 Expression Level") +
  scale_color_manual(name = "Legend", values = c("Observed" = "black", "Predicted" = "blue", "95% CI" = "red")) +
  theme_minimal()
 
# Task 3 ABC 
# Step 1: Extract the 2 parameters with the largest absolute values from Task 2.1

coefficients <- abs(thetaHat)
sorted_indices <- order(coefficients, decreasing = TRUE)
top_2_indices <- sorted_indices[1:2]
fixed_indices <- sorted_indices[-(1:2)]
top_2_parameters <- thetaHat[top_2_indices]
fixed_parameters <- thetaHat[fixed_indices]

print(thetaHat[fixed_indices])

# Step 2: Define the range of the Uniform prior distribution for the top 2 parameters
prior_range <- 0.2 * top_2_parameters
lower_bound <- top_2_parameters - prior_range
upper_bound <- top_2_parameters + prior_range

# Step 3: Perform rejection ABC
set.seed(123) # For reproducibility

# Number of samples
num_samples <- 10000
num_accept <- 1000

# Draw samples from Uniform prior
prior_samples <- matrix(runif(num_samples * 2, lower_bound, upper_bound), ncol = 2)

# Define the acceptance criterion
acceptance_criterion <- function(sample, fixed_parameters, xTrainingSet, yTrainingSet, top_2_indices) {
  
  x_train <- cbind(1, trainingX4, trainingX1 ^ 2, trainingX3 ^ 2)
  
  coefficients <- fixed_parameters
  coefficients <- append(coefficients, sample, after = top_2_indices[1] - 1)
  
  y_pred <- x_train %*% coefficients
  sum((yTrainingSet - y_pred)^2)
}

# Compute the residuals for all samples
residuals <- apply(prior_samples, 1, acceptance_criterion, fixed_parameters, xTrainingSet, yTrainingSet, top_2_indices)

# Accept samples with the smallest residuals
accepted_samples <- prior_samples[order(residuals)[1:num_accept], ]

# Step 4: Plot the joint and marginal posterior distributions
accepted_df <- data.frame(Parameter1 = accepted_samples[, 1], Parameter2 = accepted_samples[, 2])

# Joint posterior distribution
ggplot(accepted_df, aes(x = Parameter1, y = Parameter2)) +
  geom_point(alpha = 0.5) +
  labs(title = "Joint Posterior Distribution of Top 2 Parameters",
       x = "Parameter 1",
       y = "Parameter 2") +
  theme_minimal()

# Marginal posterior distribution for Parameter 2
ggplot(accepted_df, aes(x = Parameter1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "blue") +
  labs(title = "Marginal Posterior Distribution of Parameter 1",
       x = "Parameter 1",
       y = "Density") +
  theme_minimal()

ggplot(accepted_df, aes(x = Parameter2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "blue") +
  labs(title = "Marginal Posterior Distribution of Parameter 2",
       x = "Parameter 2",
       y = "Density") +
  theme_minimal()

# Task 3 End