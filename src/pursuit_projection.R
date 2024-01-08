# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- TRUE

# ---- Define functions --------------------------------------------------------

source("src/helper_functions.R")

tune_nterms <- function(formula, train_data, val_data, max_terms = 30,
    weights = NULL, title = "") {

    # Set the weights to 1 if not provided
    if (is.null(weights)) {
        weights <- as.vector(rep(1, nrow(train_data)))
    }

    # Create a vector for MSE losses
    mse <- rep(0, max_terms)

    # Loop over the number of terms
    for (i in 1:max_terms) {
        args <- list(formula = formula, data = train_data,
            weights = weights, nterms = i)

        # Fit the model
        ppr_obj <- do.call(ppr, args, quote = TRUE)

        # Predict on the validation set
        val_ppr <- predict(ppr_obj, newdata = val_data)

        # Compute mse loss
        mse[i] <- mean((val_ppr - val_data$quality)^2)
    }

    # Set plot margin
    par(mar = c(5, 4, 4, 2) + 1)

    # Plot the MSE losses
    plot(1:max_terms, mse, type = "b", xlab = "Number of terms",
        ylab = "MSE Loss", main = paste("HPO -", title),
        cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.1)

    # Highlight the minimum MSE loss
    points(which.min(mse), mse[which.min(mse)], col = "red", pch = 19)

    best_mse <- mse[which.min(mse)]
    best_nterms <- which.min(mse)

    # Print the minimum MSE loss
    print(paste("Minimum MSE with nterms = ", best_nterms, ": ", best_mse, sep = ""))

    # Return the best MSE loss and number of terms
    return(list(best_mse = best_mse, best_nterms = best_nterms,
        all_mse = mse))
}

# ---- Define hybrid sampled data ----------------------------------------------

target_size <- 400
# Apply sampling techniques to the training and validation sets
train_hybrid_sampled <- balance_classes_hybrid_sampling(train, target_size)
# 67 because we want to keep the same ratio between train and validation
validation_hybrid_sampled <- balance_classes_hybrid_sampling(validation, round(target_size / 3))
# Dont sample the test set

# ---- Check the class distribution ---------------------------------------------

table(train_hybrid_sampled$quality)
table(validation_hybrid_sampled$quality)

# ---- Simple pursuit projection -----------------------------------------------

formula <- quality ~ .

tuning_result <- tune_nterms(formula, train, validation,
    title = "Simple PPR")

# Fit the model with the best number of terms
ppr_simple <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
val_results_simple <- evaluate_model(ppr_simple, validation,
    title = "Simple PPR with HPO on nterms")

# Maybe: The model is predicting in range between 5 and 6 often times
# for the quality, probably since more than 80% of are data are 5 or 6.
# -> Overfitting to middle area

# ---- Pursuit projection regression with weights ------------------------------

# Formula and weights
formula <- quality ~ .

# Tune number of terms
tuning_result <- tune_nterms(formula, train, validation,
    weights = build_weights(train), title = "Inverse Frequency Weighted PPR")

# Fit the model with the best number of terms
ppr_weighted <- ppr(formula, data = train, nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
val_results_weighted <- evaluate_model(ppr_weighted, validation,
    title = "Inverse Frequency Weighted PPR")

# Higher MSE, but the model is a little bit better in predicting the minority
# classes

# ---- Pursuit projection regression with balanced data (hybrid sampled) -------

formula <- quality ~ .

# Tune number of terms
tuning_result <- tune_nterms(formula, train_hybrid_sampled, validation_hybrid_sampled,
    title = "PPR with balanced data (hybrid sampled)")

# Fit the model with the best number of terms
ppr_mixed <- ppr(formula, data = train_hybrid_sampled,
    nterms = tuning_result$best_nterms)

# Evaluate the model on the test set
val_results_mixed <- evaluate_model(ppr_mixed, validation,
    title = "PPR with balanced data (hybrid sampled)")

# ---- Pursuit projection regression with hybrid sampling and weights -----------

formula <- quality ~ .

weights <- build_weights(train_hybrid_sampled)

# Tune number of terms
tuning_result <- tune_nterms(formula, train_hybrid_sampled, validation_hybrid_sampled,
    title = "PPR with balanced data (hybrid sampled) and weights", weights = weights)

# Fit the model with the best number of terms
ppr_mixed_weighted <- ppr(formula, data = train_hybrid_sampled,
    nterms = tuning_result$best_nterms, weights = weights)

# Evaluate the model on the test set
val_results_mixed_weighted <- evaluate_model(ppr_mixed_weighted, validation,
    title = "PPR with balanced data (hybrid sampled) and weights")

# ---- Create a tables with the results ----------------------------------------

# Create a list with the models
models <- list(
    "PPR Simple" = ppr_simple,
    "PPR Weighted" = ppr_weighted,
    "PPR Hybrid Sampled" = ppr_mixed,
    "PPR Hybrid Sampled Weighted" = ppr_mixed_weighted
)

# Create a dataframe with the results
class_mse_vectors <- list(
    "PPR Simple" = val_results_simple$mse_per_class,
    "PPR Weighted" = val_results_weighted$mse_per_class,
    "PPR Hybrid Sampled" = val_results_mixed$mse_per_class,
    "PPR Hybrid Sampled Weighted" = val_results_mixed_weighted$mse_per_class
)
val_results <- do.call(rbind, lapply(class_mse_vectors, function(x) as.data.frame(t(x))))
rownames(val_results) <- names(class_mse_vectors)
val_results <- cbind(val_results,
                "Mean MSE over classes" = c(
                    mean(val_results_simple$mse_per_class),
                    mean(val_results_weighted$mse_per_class),
                    mean(val_results_mixed$mse_per_class),
                    mean(val_results_mixed_weighted$mse_per_class)),
                "Total MSE" = c(
                    val_results_simple$mse,
                    val_results_weighted$mse,
                    val_results_mixed$mse,
                    val_results_mixed_weighted$mse)
                )
print(val_results)

# ---- Model selection ---------------------------------------------------------

# Select the best model according to MSE over classes
min_idx <- which.min(val_results$"Mean MSE over classes")
final_model_name <- rownames(val_results)[min_idx]
final_model <- models[[final_model_name]]

# ---- Clean up ----------------------------------------------------------------

if (clean_up) {
    rm(list = setdiff(ls(), keep_vars))
}