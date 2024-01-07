# ---- Load data and libraries from other files -------------------------------

source("src/setup.R")
clean_up <- FALSE

source("src/helper_functions.R")

library(yardstick) # For huber loss


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

# ---- Set global variables and slice data frame --------------------------------
predictor <- "residual.sugar"
target <- "quality"

mixed_sampled_train <- balance_classes_mixed_sampling(train, 200)
mixed_sampled_train <- mixed_sampled_train[c(predictor, target)]


train <- train[c(predictor, target)]
validation <- validation[c(predictor, target)]
test <- test[c(predictor, target)]

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
    title = "Simple")

# Fit the model with the best number of df
spline_simple <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df)

test_result_simple <- evaluate_model(spline_simple, test,
    title = "Simple spline with HPO on df")


# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train)

tuning_result <- tune_spline_df(train, validation, predictor, weights,
    title = "Weighted")

spline_weighted <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df, w = weights)


test_result_weighted <- evaluate_model(spline_weighted, test,
    title = "Weighted spline with HPO on df")

# ---- Perform Spline smoothing with mixed sampling ----------------------------

tuning_result <- tune_spline_df(mixed_sampled_train, validation, predictor,
    title = "Mixed sampled")

spline_mixed_sampling <- smooth.spline(mixed_sampled_train[[predictor]], mixed_sampled_train[[target]], df=tuning_result$best_df)

test_result_mixed <- evaluate_model(spline_mixed_sampling, test,
    title = "Mixed sampled spline with HPO on df")

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

weights <- build_weights(mixed_sampled_train)

tuning_result <- tune_spline_df(mixed_sampled_train, validation, predictor, weights,
    title = "Mixed sampled and weighted")

spline_mixed_weighted <- smooth.spline(mixed_sampled_train[[predictor]], mixed_sampled_train[[target]], df=tuning_result$best_df, w = weights)


test_result_mixed_weighted <- evaluate_model(spline_mixed_weighted, test,
    title = "Mixed sampled and weighted spline with HPO on df")

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