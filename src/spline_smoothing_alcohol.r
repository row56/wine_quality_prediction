# ---- Load data and libraries, functions from other files ---------------------

source("src/setup.R")
clean_up <- TRUE

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

val_results_simple <- evaluate_model(spline_simple, validation,
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

val_results_weighted <- evaluate_model(spline_weighted, validation,
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

val_results_mixed <- evaluate_model(spline_mixed_sampling, validation,
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

val_results_mixed_weighted <- evaluate_model(spline_mixed_weighted, validation,
    title = "Weighted spline smoothing with hybrid sampled data")

# ---- Create a tables with the results ----------------------------------------

# Create a list with the models
models <- list(
    "Spline Alcohol Simple" = spline_simple,
    "Spline Alcohol Weighted" = spline_weighted,
    "Spline Alcohol Hybrid Sampled" = spline_mixed_sampling,
    "Spline Alcohol Hybrid Sampled Weighted" = spline_mixed_weighted
)

# Create a dataframe with the results
class_mse_vectors <- list(
    "Spline Alcohol Simple" = val_results_simple$mse_per_class,
    "Spline Alcohol Weighted" = val_results_weighted$mse_per_class,
    "Spline Alcohol Hybrid Sampled" = val_results_mixed$mse_per_class,
    "Spline Alcohol Hybrid Sampled Weighted" = val_results_mixed_weighted$mse_per_class
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