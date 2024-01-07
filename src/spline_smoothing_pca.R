# TODOs:
# HPO with validation set

# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

library(yardstick) # For huber loss

# ---- Define functions --------------------------------------------------------

source("src/helper_functions.R")

get_pca_transformed_data <- function(data, pca_transform = NULL) {
    # Get the predictors
    predictors <- data[, !colnames(data) %in% "quality"]

    is_transformed <- TRUE
    # If pca_transform is not provided, fit PCA on the data
    if (is.null(pca_transform)) {
        pca_transform <- prcomp(predictors, scale. = TRUE)
        is_transformed <- FALSE
    }

    # Transform the data
    pc_scores <- as.data.frame(predict(pca_transform, predictors))

    # Rename the columns
    colnames <- list()
    for (i in 1:ncol(pc_scores)) { # nolint
        colnames[i] <- paste("PC", i, sep = "")
    }

    pca_data <- cbind(pc_scores, quality = data$quality)
    proportion_of_var <- summary(pca_transform)$importance["Proportion of Variance", ] # nolint

    if (!is_transformed) {
        # Plot the proportion of variance
        plot(proportion_of_var, type = "b", main = "Explained Variance",
            xlab = "Principal Component", ylab = "Proportion of Variance",
            cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.1)
    }

    # Return the transformed data and the PCA transformation
    return(list(data = pca_data,
            transform = pca_transform,
            proportion_of_var = proportion_of_var))
}

# ---- Perform PCA on the dataset ----------------------------------------------

pca_result <- get_pca_transformed_data(train)

train_pca <- pca_result$data
validation_pca <- get_pca_transformed_data(validation, pca_result$transform)$data # nolint
test_pca <- get_pca_transformed_data(test, pca_result$transform)$data

# Plot PC1 vs Quality to see relationship
plot <- create_violin_plot(
            train_pca$quality,
            train_pca$PC1,
            "PC1 vs Quality",
            show_loss = FALSE,
            show_abline = FALSE,
            xlab = "Quality",
            ylab = "PC1")
print(plot)

# ---- Perform Spline smoothing simple only with HPO ---------------------------

tuning_result <- tune_spline_df(train_pca, validation_pca, "PC1",
    title = "Simple Spline with HPO on degrees of freedom")

spline_simple <- smooth.spline(train_pca$PC1, train_pca$quality,
                df = tuning_result$best_df)
plot_spline_curve(spline_simple,
    train_pca$quality,
    train_pca$PC1,
    title = "Simple Spline with HPO on degrees of freedom",
    xlab = "PC1",
    ylab = "Quality")

test_result_simple <- evaluate_model(spline_simple,
    test_pca[, c("PC1", "quality")], title = "Simple Spline with HPO on degrees of freedom")

# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train_pca)
tuning_result <- tune_spline_df(train_pca, validation_pca, "PC1",
    weights = weights,
    title = "Weighted spline smoothing")

spline_weighted <- smooth.spline(train_pca$PC1,
                                 train_pca$quality,
                                 w = weights,
                                 df = tuning_result$best_df)
plot_spline_curve(spline_weighted,
    train_pca$quality,
    train_pca$PC1,
    title = "Weighted spline smoothing",
    xlab = "PC1",
    ylab = "Quality")

test_result_weighted <- evaluate_model(spline_weighted,
    test_pca[, c("PC1", "quality")], title = "Weighted spline smoothing")

# ---- Perform Spline smoothing with mixed sampling ----------------------------

mixed_sampled_train <- balance_classes_mixed_sampling(train_pca, 200)

tuning_result <- tune_spline_df(mixed_sampled_train, validation_pca, "PC1",
    title = "Spline smoothing with mixed sampling")

spline_mixed_sampling <- smooth.spline(mixed_sampled_train$PC1,
                                       mixed_sampled_train$quality,
                                       df = tuning_result$best_df)
plot_spline_curve(spline_mixed_sampling,
    train_pca$quality,
    train_pca$PC1,
    title = "Spline smoothing with mixed sampling ",
    xlab = "PC1",
    ylab = "Quality")

test_result_mixed <- evaluate_model(spline_mixed_sampling,
    test_pca[, c("PC1", "quality")], title = "Spline smoothing with mixed sampling")

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

mixed_sampled_train <- balance_classes_mixed_sampling(train_pca, 200)
weights <- build_weights(mixed_sampled_train)

tuning_result <- tune_spline_df(mixed_sampled_train, validation_pca, "PC1",
    weights = weights,
    title = "Weighted spline smoothing with mixed sampling")

spline_mixed_sampling_weighted <- smooth.spline(mixed_sampled_train$PC1,
                                                mixed_sampled_train$quality,
                                                w = weights,
                                                df = tuning_result$best_df)
plot_spline_curve(spline_mixed_sampling_weighted,
    train_pca$quality,
    train_pca$PC1,
    title = "Weighted spline smoothing with mixed sampling",
    xlab = "PC1",
    ylab = "Quality")
test_result_mixed_weighted <- evaluate_model(spline_mixed_sampling_weighted,
    test_pca[, c("PC1", "quality")], title = "Weighted spline smoothing with mixed sampling")

# ---- Create a table with the results -----------------------------------------

# Create a dataframe with the results
results <- data.frame(
    Model = c("Simple", "Weighted", "Mixed", "Mixed Weighted"),
    MSE = c(test_result_simple$mse, test_result_weighted$mse,
        test_result_mixed$mse, test_result_mixed_weighted$mse),
    Huber = c(test_result_simple$huber, test_result_weighted$huber,
        test_result_mixed$huber, test_result_mixed_weighted$huber)
)

print(results)

# ---- Clean up ----------------------------------------------------------------

if (clean_up) {
    rm(list = ls())
}