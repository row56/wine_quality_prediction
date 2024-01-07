# TODOs:
# HPO with validation set

# ---- Load data and libraries from other files -------------------------------

source("src/setup.R")
clean_up <- FALSE

source("src/helper_functions.R")

library(yardstick) # For huber loss


# ---- Set global variables and slice data frame --------------------------------
predictor <- "alcohol"
target <- "quality"

mixed_sampled_train <- balance_classes_mixed_sampling(train, 200)
mixed_sampled_train <- mixed_sampled_train[c(predictor, target)]

train <- train[c(predictor, target)]
validation <- validation[c(predictor, target)]
test <- test[c(predictor, target)]


# ---- Plot Predictor vs Target to see relationship ------------------------------
plot <- create_violin_plot(
            train$quality,
            train$alcohol,
            paste(predictor, " vs ", target),
            show_loss = FALSE,
            show_abline = FALSE,
            xlab = target,
            ylab = predictor)
print(plot)


# -------- HPO on degrees of freedom by Huber loss -------------------------------

tune_spline_df <- function(train_data, val_data, predictor, weights = NULL, title_suffix = "") {

    # Get the predictors
    train_predictor <- train_data[, !names(train_data) %in% "quality", drop = FALSE]
    val_predictor <- val_data[, !names(train_data) %in% "quality", drop = FALSE]

    print(train_predictor[["alcohol"]])

    if (length(train_predictor[[predictor]]) != nrow(train_data) || length(val_predictor[[predictor]]) != nrow(val_data)) {
        stop("Length of predictor and quality columns -do not match.")
    }

    if (ncol(train_predictor) > 1) {
        stop("More than one predictor found, not supported for spline_smoothing.
            Subset the data to one predictor and quality column.")
    }

    df_values <- c(2:length(unique(train$alcohol)))

    # Create a vector for Huber losses
    huber <- rep(0, length(df_values))

    # Loop over the param_values
    for (i in seq_along(df_values)) {

        if (is.null(weights)) {
            args <- list(
                x = train_predictor[[predictor]],
                y = train_data$quality,
                df = df_values[i]
        )
        } else {
            args <- list(
                x = train_predictor[[predictor]],
                y = train_data$quality,
                w = weights,
                df = df_values[i]
            )
        }

        # Fit the model
        spline_obj <- do.call(smooth.spline, args, quote = TRUE)

        # Predict on the validation set
        val_spline <- as.vector(predict(spline_obj, val_predictor[[predictor]])$y)

        # Compute huber loss
        huber[i] <- huber_loss_vec(val_data$quality, val_spline)
    }

    # Plot the Huber losses
    plot(seq_along(df_values), huber, type = "b", xlab = "df",
        ylab = "Huber Loss", main = paste("Huber Loss vs df (",
        title_suffix, ")"))

    # Highlight the minimum Huber loss
    points(which.min(huber), huber[which.min(huber)], col = "red", pch = 19)

    best_huber <- huber[which.min(huber)]
    best_df <- df_values[which.min(huber)]

    # Print the minimum Huber loss
    print(paste("Minimum Huber with df = ", best_df, ": ", best_huber))

    # Return the best Huber loss and df
    return(list(best_huber = best_huber, best_df = best_df,
        all_huber = huber))
}

# ---- Perform Spline smoothing simple only with HPO ---------------------------

tuning_result <- tune_spline_df(train, validation, predictor,
    title_suffix = "Simple")

# Fit the model with the best number of df
spline_simple <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df)

test_result_simple <- evaluate_model(spline_simple, test,
    title = "Simple spline with HPO on df")


# ---- Perform Spline smoothing weighted ---------------------------------------

weights <- build_weights(train)

tuning_result <- tune_spline_df(train, validation, predictor, weights,
    title_suffix = "Weighted")

spline_weighted <- smooth.spline(train[[predictor]], train[[target]], df=tuning_result$best_df, w = weights)


test_result_weighted <- evaluate_model(spline_weighted, test,
    title = "Weighted spline with HPO on df")

# ---- Perform Spline smoothing with mixed sampling ----------------------------

tuning_result <- tune_spline_df(mixed_sampled_train, validation, predictor,
    title_suffix = "Mixed sampled")

spline_mixed_sampling <- smooth.spline(mixed_sampled_train[[predictor]], mixed_sampled_train[[target]], df=tuning_result$best_df)

test_result_mixed <- evaluate_model(spline_mixed_sampling, test,
    title = "Mixed sampled spline with HPO on df")

# ---- Perform Spline smoothing with mixed sampling and weights ----------------

weights <- build_weights(mixed_sampled_train)

tuning_result <- tune_spline_df(mixed_sampled_train, validation, predictor, weights,
    title_suffix = "Mixed sampled and weighted")

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