# Setup for spline smoothing method

# To Do:
# clean code
# implement weights

# use bootstrapping to gain confidence
setwd("/Users/pwecker/dev/mltwo-project")
source("src/setup.R")
source("src/helper_functions.R")
# 1. import libraries, source data and helper functions----
library(ggplot2)
library(gridExtra)
library(splines)
library(caret)
library(smotefamily)




# 2. Load data, use PCA to construct predictor variable "PC1"----
predictors <- train[, !colnames(wine_data) %in% "quality"]
standardized_predictors <- scale(predictors)
pca_result <- prcomp(standardized_predictors, scale. = TRUE)
pc_score <- as.data.frame(pca_result$x)
colnames(pc_score) <- c("PC1", "PC2", "PC3","PC4", "PC5", "PC6","PC7", "PC8", "PC9","PC10", "PC11" )
train_pca <- cbind(pc_score, quality = train$quality)

#-------use sampling methods here
mixed_sampled_train <- balance_classes_mixed_sampling(train_pca, 150)
table(mixed_sampled_train$quality)





# 3. Plot the predictor "PC1" vs quality as violin
# (This part of the code might be moved to some other place!)
pc_to_plot <- colnames(pc_score)

violin_plot <- ggplot(train_pca,
                      aes(x = factor(quality),
                          y = get(pc_to_plot),
                          fill = factor(quality))) +
                geom_violin(trim = FALSE) +
                scale_fill_brewer(palette = "Set2") +
                labs(title = paste("Violin Plot of",
                                   pc_to_plot,
                                   "by Quality"),
                     x = "Quality",
                     y = pc_to_plot)

print(violin_plot)

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
