# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

library(yardstick) # For huber loss

# ---- Define functions --------------------------------------------------------

source("src/helper_functions.R")

tune_nterms <- function(formula, train_data, val_data, max_terms = 30,
    weights = NULL, title_suffix = "") {

    # Set the weights to 1 if not provided
    if (is.null(weights)) {
        weights <- as.vector(rep(1, nrow(train_data)))
    }

    # Create a vector for Huber losses
    huber <- rep(0, max_terms)

    # Loop over the number of terms
    for (i in 1:max_terms) {
        args <- list(formula = formula, data = train_data,
            weights = weights, nterms = i)

        # Fit the model
        ppr_obj <- do.call(ppr, args, quote = TRUE)

        # Predict on the validation set
        val_ppr <- predict(ppr_obj, newdata = val_data)

        # Compute huber loss
        huber[i] <- huber_loss_vec(val_data$quality, val_ppr)
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
test_results_simple <- evaluate_model(ppr_simple, test,
    title = "Simple PPR with HPO on nterms")

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
test_results_weighted <- evaluate_model(ppr_weighted, test,
    title = "PPR with inverse frequency weights")

# Higher MSE, but the model is a little bit better in predicting the minority
# classes

# ---- Pursuit projection regression with oversampling using SMOTE--------------

formula <- quality ~ .

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
test_results_smote <- evaluate_model(ppr_smote, test,
    title = "PPR with SMOTE Oversampling")

# ---- Pursuit projection regression with undersampling ------------------------

formula <- quality ~ .

# Apply undersampling per class on the training set
balanced_train <- balance_classes_by_undersampling(train)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Undersampling")

# Fit the model with the best number of terms
ppr_under <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results_under <- evaluate_model(ppr_under, test,
    title = "PPR with Undersampling")

# ---- Pursuit projection regression with mixed sampling (under -> over) -------

formula <- quality ~ .

# Apply mixed sampling per class on the training set (first under, than
# oversampling)
balanced_train <- balance_classes_mixed_sampling(train, target_size = 200)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Mixed Sampling")

# Fit the model with the best number of terms
ppr_mixed <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
test_results_mixed <- evaluate_model(ppr_mixed, test,
    title = "PPR with Mixed Sampling")

# ---- Pursuit projection regression with mixed sampling and weights -----------

formula <- quality ~ .

# Apply mixed sampling per class on the training set (first under,
# than oversampling)
balanced_train <- balance_classes_mixed_sampling(train,
    target_size = 200)

weights <- build_weights(balanced_train)

# Show the class distribution
print(table(balanced_train$quality))

# Tune number of terms
tuning_result <- tune_nterms(formula, balanced_train, validation,
    title_suffix = "Mixed Sampling with weights", weights = weights)

# Fit the model with the best number of terms
ppr_mixed_weighted <- ppr(formula, data = balanced_train,
    nterms = tuning_result$best_nterms, weights = weights)

# Evaluate the model on the test set
test_results_mixed_weighted <- evaluate_model(ppr_mixed_weighted, test,
    title = "PPR with Mixed Sampling and Weights")

# ---- Create a table with the results -----------------------------------------

# Create a dataframe with the results
results <- data.frame(
    Model = c("Simple", "Weighted", "SMOTE", "Undersampling", "Mixed",
        "Mixed Weighted"),
    MSE = c(test_results_simple$mse, test_results_weighted$mse,
        test_results_smote$mse, test_results_under$mse,
        test_results_mixed$mse, test_results_mixed_weighted$mse),
    Huber = c(test_results_simple$huber, test_results_weighted$huber,
        test_results_smote$huber, test_results_under$huber,
        test_results_mixed$huber, test_results_mixed_weighted$huber)
)

print(results)

# ---- Clean up ----------------------------------------------------------------

if (clean_up) {
    rm(list = ls())
}