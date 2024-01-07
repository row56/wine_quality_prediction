# ---- Load data and libraries, functions from other files ---------------------

source("src/setup.R")
clean_up <- FALSE

source("src/helper_functions.R")

# names(train):
# "fixed.acidity"        "volatile.acidity"     "citric.acid"
# "residual.sugar"       "chlorides"            "free.sulfur.dioxide"
# "total.sulfur.dioxide" "density"              "pH"
# "sulphates"            "alcohol"              "quality"


# Interesting predictors:
# volatile.acidity
# density
# pH
# alcohol
# residual.sugar

# ---- Define hybrid sampled data ----------------------------------------------

target_size <- 400
# Apply sampling techniques to the training and validation sets
train_hybrid_sampled <- balance_classes_hybrid_sampling(train, target_size)
# 67 because we want to keep the same ratio between train and validation
validation_hybrid_sampled <- balance_classes_hybrid_sampling(validation, round(target_size / 3))
# Dont sample the test set

# ---- Set global variables and slice data frame --------------------------------

predictor <- "alcohol"
target <- "quality"

train <- train[c(predictor, target)]
validation <- validation[c(predictor, target)]
test <- test[c(predictor, target)]

# ---- Check the class distribution ---------------------------------------------

table(train_hybrid_sampled$quality)
table(validation_hybrid_sampled$quality)

# ---- Plot Predictor vs Target to see relationship ------------------------------
plot <- create_violin_plot(
            train[[target]],
            train[[predictor]],
            paste(predictor, " vs ", target),
            show_loss = FALSE,
            show_abline = FALSE,
            xlab = target,
            ylab = predictor)
print(plot)

# ---- Perform Spline smoothing simple only with HPO ---------------------------

tuning_result <- tune_spline_df(train, validation, predictor,
    title = "Simple Spline with HPO on degrees of freedom")

# Fit the model with the best number of df
spline_simple <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df)
plot_spline_curve(spline_simple,
    train[[target]],
    train[[predictor]],
    title = "Simple Spline with HPO on degrees of freedom",
    xlab = predictor,
    ylab = "Quality")

val_result_simple <- evaluate_model(spline_simple, validation,
    title = "Simple Spline with HPO on degrees of freedom")

# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train)

tuning_result <- tune_spline_df(train, validation, predictor, weights,
    title = "Weighted spline smoothing")

spline_weighted <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df, w = weights)
plot_spline_curve(spline_weighted,
    train[[target]],
    train[[predictor]],
    title = "Weighted spline smoothing",
    xlab = predictor,
    ylab = "Quality")

val_result_weighted <- evaluate_model(spline_weighted, validation,
    title = "Weighted spline smoothing")

# ---- Perform Spline smoothing with mixed sampling ----------------------------

tuning_result <- tune_spline_df(train_hybrid_sampled, validation_hybrid_sampled, predictor,
    title = "Spline smoothing with hybrid sampled data")

spline_mixed_sampling <- smooth.spline(train_hybrid_sampled[[predictor]], train_hybrid_sampled[[target]], df=tuning_result$best_df)
plot_spline_curve(spline_mixed_sampling,
    train[[target]],
    train[[predictor]],
    title = "Spline smoothing with hybrid sampled data",
    xlab = predictor,
    ylab = "Quality")

val_result_mixed <- evaluate_model(spline_mixed_sampling, validation,
    title = "Spline smoothing with hybrid sampled data")

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

weights <- build_weights(train_hybrid_sampled)

tuning_result <- tune_spline_df(train_hybrid_sampled, validation_hybrid_sampled, predictor, weights,
    title = "Weighted spline smoothing with hybrid sampled data")

spline_mixed_weighted <- smooth.spline(train_hybrid_sampled[[predictor]], train_hybrid_sampled[[target]], df=tuning_result$best_df, w = weights)
plot_spline_curve(spline_mixed_weighted,
    train[[target]],
    train[[predictor]],
    title = "Weighted spline smoothing with hybrid sampled data",
    xlab = predictor,
    ylab = "Quality")

val_result_mixed_weighted <- evaluate_model(spline_mixed_weighted, validation,
    title = "Weighted spline smoothing with hybrid sampled data")

# ---- Create a table with the results -----------------------------------------

# Create a dataframe with the results
results <- data.frame(
    Model = c("Spline Simple", "Spline Weighted", "Spline Hybrid Sampled", "Spline Hybrid Sampled Weighted"),
    MSE = c(val_result_simple$mse, val_result_weighted$mse,
        val_result_mixed$mse, val_result_mixed_weighted$mse)
)

print(results)

# ---- Clean up ----------------------------------------------------------------

if (clean_up) {
    rm(list = ls())
}