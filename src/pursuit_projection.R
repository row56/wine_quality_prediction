# ---- Load data and libraries from Setup.R file -------------------------------
rm(list = ls(all.names = TRUE))

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

# ---- Define functions --------------------------------------------------------

build_weights <- function(data) {
    # Create a matrix of weights
    weights <- c(rep(1, nrow(data)))

    counts <- table(data$quality)
    total <- sum(counts)

    # Loop over the weights and data
    for (i in seq_along(weights)) {
        # Higher weight for quality that differs from the mean
        weights[i] <- total / counts[as.character(data[i, "quality"])]
    }

    # Return the weights
    return(weights)
}

evaluate_model <- function(model, data) {
    # Predict on the data
    predicted_values <- predict(model, newdata = data)

    actual_values <- data$quality

    # Create a scatter plot of the actual values vs the predicted values
    plot(actual_values, predicted_values, xlab = "Actual Values",
        ylab = "Predicted Values", main = "Actual vs Predicted Values",
        xlim = c(3, 8), ylim = c(3, 8))

    # Add a line showing where the predicted values equal the actual values
    abline(a = 0, b = 1, col = "red")

    # Compute the MSE
    mse <- mean((actual_values - predicted_values)^2)

    # Print the MSE
    print(paste("Test MSE: ", mse))

    # Return the Predicted Values and MSE
    return(list(mse = mse, predicted_values = predicted_values))
}

tune_nterms <- function(formula, train_data, val_data, max_terms = 15,
    weights = NULL) {

    # Set the weights to 1 if not provided
    if (is.null(weights)) {
        weights <- as.vector(rep(1, nrow(train_data)))
    }

    # Store the weights in the environment so ppr() can access them
    weights_in_env <- NULL
    weights_in_env <<- weights

    # Create a vector of MSEs
    mse <- rep(0, max_terms)

    # Loop over the number of terms
    for (i in 1:max_terms) {

        # Fit the model
        ppr_obj <- ppr(formula, train_data, weights = weights_in_env,
            nterms = i)

        # Predict on the validation set
        val_ppr <- predict(ppr_obj, newdata = val_data)

        # Compute the validation MSE
        mse[i] <- mean((val_data$quality - val_ppr)^2)
    }

    if (exists("weights_in_env", envir = .GlobalEnv)) {
        rm(weights_in_env, pos = ".GlobalEnv")
    }

    # Plot the MSEs
    plot(1:max_terms, mse, type = "b", xlab = "Number of terms", ylab = "MSE")

    # Highlight the minimum MSE
    points(which.min(mse), mse[which.min(mse)], col = "red", pch = 19)

    best_mse <- mse[which.min(mse)]
    best_nterms <- which.min(mse)

    # Print the minimum MSE
    print(paste("Minimum MSE with nterms = ", best_nterms, ": ", best_mse))

    # Return the best MSE and number of terms
    return(list(best_mse = best_mse, best_nterms = best_nterms, all_mse = mse))
}

# ---- Simple pursuit projection -----------------------------------------------

formula <- quality ~ .

tuning_result <- tune_nterms(formula, train, validation)

# Fit the model with the best number of terms
ppr_simple <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_simple, test)

# Maybe: The model is predicting in range between 5 and 6 often times
# for the quality, probably since more than 80% of are data are 5 or 6.
# -> Overfitting to middle area

# ---- Pursuit projection regression with weights ------------------------------

# Formula and weights
formula <- quality ~ .

# Tune number of terms
tuning_result <- tune_nterms(formula, train, validation,
    weights = build_weights(train))

# Fit the model with the best number of terms
ppr_weighted <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_weighted, test)

# Higher MSE, but the model is a little bit better in predicting the minority
# classes

# ---- Pursuit projection regression with stratified sampling ------------------

# TODO

# ---- Clean up ----------------------------------------------------------------
if (clean_up) {
    rm(list = ls())
}






