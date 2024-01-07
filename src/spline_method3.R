# TODOs:
# HPO with validation set

# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

source("src/helper_functions.R")

# ---- Import libraries --------------------------------------------------------

library(yardstick) # For huber loss

# Plot pH vs Quality to see relationship
plot <- create_violin_plot(
            train$quality,
            train$pH,
            "pH vs Quality",
            show_loss = FALSE,
            show_abline = FALSE,
            xlab = "Quality",
            ylab = "pH")
print(plot)

# ---- Perform Spline smoothing simple only with HPO ---------------------------

spline_simple <- smooth.spline(train$pH, train$quality)

plot_spline_curve(spline_simple,
    train$quality,
    train$pH,
    title = "Simple Spline",
    xlab = "pH",
    ylab = "Quality")

test_result_simple <- evaluate_model(spline_simple,
    test[, c("pH", "quality")])

# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train)

spline_weighted <- smooth.spline(train$pH,
                                 train$quality,
                                 w = weights)
plot_spline_curve(spline_weighted,
    train$quality,
    train$pH,
    title = "Weighted Spline",
    xlab = "pH",
    ylab = "Quality")

test_result_weighted <- evaluate_model(spline_weighted,
    test[, c("pH", "quality")])

# ---- Perform Spline smoothing with mixed sampling ----------------------------

mixed_sampled_train <- balance_classes_mixed_sampling(train, 200)

spline_mixed_sampling <- smooth.spline(mixed_sampled_train$pH,
                                       mixed_sampled_train$quality)
plot_spline_curve(spline_mixed_sampling,
    train$quality,
    train$pH,
    title = "Mixed Sampling Spline",
    xlab = "pH",
    ylab = "Quality")

test_result_mixed <- evaluate_model(spline_mixed_sampling,
    test[, c("pH", "quality")])

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

mixed_sampled_train <- balance_classes_mixed_sampling(train, 200)
weights <- build_weights(mixed_sampled_train)

spline_mixed_weighted <- smooth.spline(mixed_sampled_train$pH,
                                    mixed_sampled_train$quality,
                                    w = weights)
plot_spline_curve(spline_mixed_weighted,
    train$quality,
    train$pH,
    title = "Mixed Sampling and Weighted Spline",
    xlab = "pH",
    ylab = "Quality")
test_result_mixed_weighted <- evaluate_model(spline_mixed_weighted,
    test[, c("pH", "quality")])

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