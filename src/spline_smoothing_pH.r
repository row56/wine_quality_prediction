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

target_size <- 150
# Apply sampling techniques to the training and validation sets
train_hybrid_sampled <- balance_classes_hybrid_sampling(train, target_size)
# 67 because we want to keep the same ratio between train and validation
validation_hybrid_sampled <- balance_classes_hybrid_sampling(validation, round(target_size / 3))
# Dont sample the test set

# ---- Set global variables and slice data frame --------------------------------

predictor <- "pH"
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
    title = paste("Simple Spline on", predictor))

simple_spline_name <- paste("Simple Spline on", predictor, "with HPO")
# Fit the model with the best number of df
spline_simple <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df)
plot_spline_curve(spline_simple,
    train[[target]],
    train[[predictor]],
    title = simple_spline_name,
    xlab = predictor,
    ylab = "Quality")

val_results_simple <- evaluate_model(spline_simple, validation,
    title = simple_spline_name)

# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train)

tuning_result <- tune_spline_df(train, validation, predictor, weights,
    title = paste("Inverse Frequency Weighted Spline on", predictor))

weighted_spline_name <- paste("Inverse Frequency Weighted Spline on", predictor, "with HPO")
spline_weighted <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df, w = weights)
plot_spline_curve(spline_weighted,
    train[[target]],
    train[[predictor]],
    title = weighted_spline_name,
    xlab = predictor,
    ylab = "Quality")

val_results_weighted <- evaluate_model(spline_weighted, validation,
    title = weighted_spline_name)

# ---- Perform Spline smoothing with mixed sampling ----------------------------

tuning_result <- tune_spline_df(train_hybrid_sampled, validation_hybrid_sampled, predictor,
    title = paste("Spline on", predictor, "with balanced data (hybrid sampled)"))

hybrid_sampled_spline_name <- paste("Spline on", predictor, "with balanced data (hybrid sampled) and HPO")
spline_mixed_sampling <- smooth.spline(train_hybrid_sampled[[predictor]], train_hybrid_sampled[[target]], df=tuning_result$best_df)
plot_spline_curve(spline_mixed_sampling,
    train[[target]],
    train[[predictor]],
    title = hybrid_sampled_spline_name,
    xlab = predictor,
    ylab = "Quality")

val_results_mixed <- evaluate_model(spline_mixed_sampling, validation,
    title = hybrid_sampled_spline_name)

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

weights <- build_weights(train_hybrid_sampled)

tuning_result <- tune_spline_df(train_hybrid_sampled, validation_hybrid_sampled, predictor, weights,
    title = paste("Weighted spline", predictor, "with balanced data (hybrid sampled)"))

hybrid_sampled_weighted_spline_name <- paste("Weighted spline on", predictor, "with balanced data (hybrid sampled) and HPO")
spline_mixed_weighted <- smooth.spline(train_hybrid_sampled[[predictor]], train_hybrid_sampled[[target]], df=tuning_result$best_df, w = weights)
plot_spline_curve(spline_mixed_weighted,
    train[[target]],
    train[[predictor]],
    title = hybrid_sampled_weighted_spline_name,
    xlab = predictor,
    ylab = "Quality")

val_results_mixed_weighted <- evaluate_model(spline_mixed_weighted, validation,
    title = hybrid_sampled_weighted_spline_name)

# ---- Create a tables with the results ----------------------------------------

# Create a list with names
model_names <- list(
    simple_spline_name,
    weighted_spline_name,
    hybrid_sampled_spline_name,
    hybrid_sampled_weighted_spline_name
)

# Create a list with the models
models <- list(
    spline_simple,
    spline_weighted,
    spline_mixed_sampling,
    spline_mixed_weighted
)
names(models) <- model_names

# Create a dataframe with the results
class_mse_vectors <- list(
    val_results_simple$mse_per_class,
    val_results_weighted$mse_per_class,
    val_results_mixed$mse_per_class,
    val_results_mixed_weighted$mse_per_class
)
names(class_mse_vectors) <- model_names

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