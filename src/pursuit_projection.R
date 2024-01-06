# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

library(yardstick) # For huber loss

# ---- Define functions --------------------------------------------------------

source("src/helper_functions.R")

tune_nterms <- function(formula, train_data, val_data, max_terms = 20,
    weights = NULL, title_suffix = "") {

    # Set the weights to 1 if not provided
    if (is.null(weights)) {
        weights <- as.vector(rep(1, nrow(train_data)))
    }

    # Store the weights in the environment so ppr() can access them
    weights_in_globenv <- NULL
    weights_in_globenv <<- weights

    # Create a vector for Huber losses
    huber <- rep(0, max_terms)

    # Loop over the number of terms
    for (i in 1:max_terms) {

        # Fit the model
        ppr_obj <- ppr(formula, train_data, weights = weights_in_globenv,
            nterms = i)

        # Predict on the validation set
        val_ppr <- predict(ppr_obj, newdata = val_data)

        # Compute huber loss
        huber[i] <- huber_loss_vec(val_data$quality, val_ppr)
    }

    # Remove the weights from the environment
    if (exists("weights_in_globenv", envir = .GlobalEnv)) {
        rm(weights_in_globenv, pos = ".GlobalEnv")
    }

    # Plot the Huber losses
    plot(1:max_terms, huber, type = "b", xlab = "Number of terms",
        ylab = "Huber Loss", main = paste("Huber Loss vs Number of terms (",
        title_suffix, ")"))

    # Highlight the minimum Huber loss
    points(which.min(huber), huber[which.min(huber)], col = "red", pch = 19)

    best_huber <- huber[which.min(huber)]
    best_nterms <- which.min(huber)

    # Print the minimum Huber loss
    print(paste("Minimum Huber with nterms = ", best_nterms, ": ", best_huber))

    # Return the best Huber loss and number of terms
    return(list(best_huber = best_huber, best_nterms = best_nterms,
        all_huber = huber))
}

# ---- Simple pursuit projection -----------------------------------------------

formula <- quality ~ .

tuning_result <- tune_nterms(formula, train, validation,
    title_suffix = "Simple")

# Fit the model with the best number of terms
ppr_simple <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_simple, test, title_suffix = "Simple")

# Maybe: The model is predicting in range between 5 and 6 often times
# for the quality, probably since more than 80% of are data are 5 or 6.
# -> Overfitting to middle area

# ---- Pursuit projection regression with weights ------------------------------

# Formula and weights
formula <- quality ~ .

# Tune number of terms
tuning_result <- tune_nterms(formula, train, validation,
    weights = build_weights(train), title_suffix = "weighted")

# Fit the model with the best number of terms
ppr_weighted <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_weighted, test,
    title_suffix = "Weighted")

# Higher MSE, but the model is a little bit better in predicting the minority
# classes

# ---- Pursuit projection regression with oversampling using SMOTE--------------

# Apply SMOTE oversampling per class on the training set
balanced_train <- balance_classes_with_smote(train)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "SMOTE Oversampling")

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test,
    title_suffix = "SMOTE Oversampling")

# ---- Pursuit projection regression with undersampling ------------------------

# Apply SMOTE oversampling per class on the training set
balanced_train <- balance_classes_by_undersampling(train)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Undersampling")

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test, title_suffix = "Undersampling")

# ---- Pursuit projection regression with mixed sampling (under -> over) -------

# Apply mixed sampling per class on the training set (first under, than
# oversampling)
balanced_train <- balance_classes_mixed_sampling(train, target_size = 150)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Mixed Sampling")

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test, title_suffix = "Mixed Sampling")

# ---- Pursuit projection regression with mixed sampling (under -> over) -------

# Apply SMOTE oversampling per class on the training set
undersampled_train <- balance_classes_by_undersampling(train, target_size = 150)

balanced_train <- balance_classes_with_smote(undersampled_train)

weights <- build_weights(balanced_train)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Mixed Sampling with weights", weights = weights)

# Fit the model with the best number of terms
ppr_smote <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms, weights = weights)

# Evaluate the model on the test set
test_results <- evaluate_model(ppr_smote, test,
    title_suffix = "Mixed Sampling with weights")

# ---- Clean up ----------------------------------------------------------------
if (clean_up) {
    rm(list = ls())
}