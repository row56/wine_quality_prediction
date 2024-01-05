# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

library(smotefamily)
library(caret)

# ---- Define functions --------------------------------------------------------

balance_classes_with_smote <- function(df) {
    features <- df[, !names(df) %in% "quality"]
    target <- df[["quality"]]

    # Determine the target size (size of the largest class)
    target_size <- max(table(target))

    # Initialize an empty dataframe for the balanced dataset
    balanced_df <- df[0, ]

    # Loop through each class
    for (class in unique(target)) {
        class_data <- df[target == class, ]

        if (nrow(class_data) < target_size) {
            # Calculate the number of synthetic samples needed
            dup_size <- ((target_size - nrow(class_data))
                / nrow(class_data))

            # Apply SMOTE
            oversampled_data <- SMOTE(features[target == class, ],
                target[target == class], K = 2, dup_size = dup_size)$data

            colnames(oversampled_data) <- colnames(balanced_df)

            # Combine the oversampled data
            balanced_df <- rbind(balanced_df, oversampled_data)
        } else {
            balanced_df <- rbind(balanced_df, class_data)
        }
    }

    balanced_df <- data.frame(lapply(balanced_df, as.numeric))

    return(balanced_df)
}

balance_classes_by_undersampling <- function(df, target_size = 0) {
    target <- df[["quality"]]

    # Determine the target size (size of the smallest class)
    if (target_size == 0) {
        target_size <- min(table(target))
    }

    # Initialize an empty dataframe for the balanced dataset
    balanced_df <- df[0, ] # Creates an empty dataframe with same columns as df

    # Loop through each class
    for(class in unique(target)) {
        class_data <- df[target == class, ]

        # Randomly sample instances if the class is larger than the target size
        if (nrow(class_data) > target_size) {
            undersampled_data <- class_data[sample(nrow(class_data),
                                             target_size), ]
        } else {
            undersampled_data <- class_data
        }

        balanced_df <- rbind(balanced_df, undersampled_data)
    }

    return(balanced_df)
}

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
    weights_in_globenv <- NULL
    weights_in_globenv <<- weights

    # Create a vector of MSEs
    mse <- rep(0, max_terms)

    # Loop over the number of terms
    for (i in 1:max_terms) {

        # Fit the model
        ppr_obj <- ppr(formula, train_data, weights = weights_in_globenv,
            nterms = i)

        # Predict on the validation set
        val_ppr <- predict(ppr_obj, newdata = val_data)

        # Compute the validation MSE
        mse[i] <- mean((val_data$quality - val_ppr)^2)
    }

    # Remove the weights from the environment
    if (exists("weights_in_globenv", envir = .GlobalEnv)) {
        rm(weights_in_globenv, pos = ".GlobalEnv")
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

# ---- Pursuit projection regression with oversampling using SMOTE--------------

# Apply SMOTE oversampling per class on the training set
balanced_train <- balance_classes_with_smote(train)

# Show the class distribution
table(balanced_train$quality)

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation)

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test)

# ---- Pursuit projection regression with undersampling ------------------------

# Apply SMOTE oversampling per class on the training set
balanced_train <- balance_classes_by_undersampling(train)

# Show the class distribution
table(balanced_train$quality)

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation)

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test)

# ---- Pursuit projection regression with mixed sampling (under -> over) -------

# Apply SMOTE oversampling per class on the training set
undersampled_train <- balance_classes_by_undersampling(train, target_size = 300)

balanced_train <- balance_classes_with_smote(undersampled_train)

# Show the class distribution
table(balanced_train$quality)

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation)

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test)

# ---- Clean up ----------------------------------------------------------------
if (clean_up) {
    rm(list = ls())
}
