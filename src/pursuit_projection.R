# Setup for pursuit projection
# Perform and visualize the training
# Perform predictions

# ---- Load data and libraries from Setup.R file -------------------------------
source("src/setup.R")

# ---- Import libraries --------------------------------------------------------

# ---- Perform pursuit projection ----------------------------------------------

# Tune number of terms

max_terms <- 15

# Create a vector of MSEs
mse <- rep(NA, max_terms)

# Create training and validation sets
train_size <- floor(0.8 * nrow(train))
train_indices <- sample(seq_len(nrow(train)), size = train_size)

tuning_train <- train[train_indices, ]
tuning_validation <- train[-train_indices, ]

# Loop over the number of terms
for (i in 1:max_terms) {
  # Fit the model
  ppr.obj <- ppr(formula, data = tuning_train, nterms = i)

  # Predict on the validation set
  val.ppr <- predict(ppr.obj, newdata = tuning_validation)

  # Compute the validation MSE
  mse[i] <- mean((tuning_validation$quality - val.ppr)^2)
}

# Plot the MSEs
plot(1:max_terms, mse, type = "b", xlab = "Number of terms", ylab = "MSE")

# Highlight the minimum MSE
points(which.min(mse), mse[which.min(mse)], col = "red", pch = 19)

best_mse <- mse[which.min(mse)]
best_nterms <- which.min(mse)

# Print the minimum MSE
print(paste("Minimum MSE with nterms = ", best_nterms, ": ", best_mse))

# ---- Test final model --------------------------------------------------------

# Fit the model with the best number of terms
ppr.obj <- ppr(formula, data = train, nterms = best_nterms)

# Predict on the test set
predicted_values <- predict(ppr.obj, newdata = test)

actual_values <- test$quality

# Create a scatter plot of the actual values vs the predicted values
plot(actual_values, predicted_values, xlab = "Actual Values",
     ylab = "Predicted Values", main = "Actual vs Predicted Values",
     xlim = c(3, 8), ylim = c(3, 8))

# Add a line showing where the predicted values equal the actual values
abline(a = 0, b = 1, col = "red")

# Create residiual plot
residuals <- actual_values - predicted_values
plot(predicted_values, residuals)
abline(h = 0)  # Adds a horizontal line at 0 for reference

# Maybe: The model is predicting in range between 5 and 6 often times
# for the quality, probably since more than 80% of are data are 5 or 6.
# -> Overfitting to middle area

# ---- Clean up ----------------------------------------------------------------
rm(list = ls())