# ---- Load data and libraries from Setup.R file -------------------------------

source("src/setup.R")
clean_up <- FALSE

# ---- Import libraries --------------------------------------------------------

library(gridExtra)
library(splines)
library(caret)
library(smotefamily)
library(yardstick) # For huber loss

# ---- Define functions --------------------------------------------------------

source("src/helper_functions.R")

get_pca_transformed_data <- function(data, pca_transform = NULL) {
    # Get the predictors
    predictors <- data[, !colnames(data) %in% "quality"]

    # If pca_transform is not provided, fit PCA on the data
    if (is.null(pca_transform)) {
        pca_transform <- prcomp(predictors, scale. = TRUE)
    }

    # Transform the data
    pc_scores <- as.data.frame(predict(pca_transform, predictors))

    # Rename the columns
    colnames <- list()
    for (i in 1:ncol(pc_scores)) {
        colnames[i] <- paste("PC", i, sep = "")
    }

    pca_data <- cbind(pc_scores, quality = data$quality)
    proportion_of_var <- summary(pca_transform)$importance["Proportion of Variance", ]

    plot(proportion_of_var, type = "b", main = "Explained Variance")

    # Return the transformed data and the PCA transformation
    return(list(data = pca_data,
            transform = pca_transform,
            proportion_of_var = proportion_of_var))
}

# ---- Perform PCA on the dataset ----------------------------------------------

pca_result <- get_pca_transformed_data(train)

train_pca <- pca_result$data
validation_pca <- get_pca_transformed_data(validation, pca_result$transform)$data
test_pca <- get_pca_transformed_data(test, pca_result$transform)$data

# ---- Perform Spline smoothing with mixed sampling ----------------------------

mixed_sampled_train <- balance_classes_mixed_sampling(train_pca, 200)

# ---- Plot the predictor "PC1" vs quality as violin ---------------------------

plot <- create_violin_plot(
            train_pca$quality,
            train_pca$PC1,
            "PC1 vs Quality",
            show_loss = FALSE,
            show_abline = FALSE,
            xlab = "Quality",
            ylab = "PC1")
print(plot)

#------------------------------------------------------------------------
# 4. Fit smoothing spline models on "PC1" vs. quality
# weigthed or unweigthed
spline_unweighted <- smooth.spline(mixed_sampled_train$PC1,
                                   mixed_sampled_train$quality)

weights <- build_weights(train_pca)
spline_weighted <- smooth.spline(train_pca$PC1,
                                 train$quality,
                                 w = weights)

# 5. Visualize data and prediction of model(s)
#---- for unweighted model
model <- spline_unweighted
name <- "spline unweighted"
x_vals <- seq(min(train_pca$PC1), max(train_pca$PC1), length.out = 100)
y_vals <- predict(model, x_vals)$y

scatter_plot <-
  ggplot(train_pca, aes(x = PC1, y = quality, color = factor(quality))) +
  geom_point(shape = 16) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Scatter Plot of PC1 by Quality", x = "PC1", y = "Quality") +
  geom_line(data = data.frame(PC1 = x_vals, quality = y_vals),
            aes(x = PC1, y = quality), color = "red") +
  labs(title = paste(c(name, ", df= ", round(model$df, digits = 2),
                       "lambda=", round(model$lambda, digits = 2)), collapse = " "))

print(scatter_plot)


#
#---- Plot for weighted model
model <- spline_weighted
name <- "spline weighted"
x_vals <- seq(min(train_pca$PC1), max(train_pca$PC1), length.out = 100)
y_vals <- predict(model, x_vals)$y

# Create a scatter plot of PC1 and 'quality'
scatter_plot <-
  ggplot(train_pca, aes(x = PC1, y = quality, color = factor(quality))) +
  geom_point(shape = 16) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Scatter Plot of PC1 by Quality", x = "PC1", y = "Quality") +
  geom_line(data = data.frame(PC1 = x_vals, quality = y_vals),
            aes(x = PC1, y = quality), color = "red") +
  labs(title = paste(c(name, ", df= ", round(model$df, digits = 2),
                       "lambda=", round(model$lambda, digits = 2)), collapse = " "))

print(scatter_plot)



# check the model performance on the validation set
vals <- validation[, !colnames(wine_data) %in% "quality"]

standardized_predictors_validation <- scale(
  vals,
  center = pca_result$center,
  scale = pca_result$scale)

pc_scores_validation <- as.data.frame(predict(pca_result,
                                              newdata = standardized_predictors_validation)[, 1])
colnames(pc_scores_validation) <- c("PC1")
data_pca_validation <- cbind(pc_scores_validation, quality = validation$quality)

# 2. Apply the smoothing spline model to the validation set
spline_weighted_predictions <- predict(spline_weighted, data_pca_validation$PC1)


plot(data_pca_validation$quality, spline_weighted_predictions$y, xlab = "Actual Values",
        ylab = "Predicted Values", main = "Actual vs Predicted Values",
        xlim = c(3, 8), ylim = c(3, 8))

# trying out evaluation method
evaluate_model(spline_unweighted, data_pca_validation[, c("PC1", "quality")])

evaluate_model(spline_weighted, data_pca_validation[, c("PC1", "quality")])
