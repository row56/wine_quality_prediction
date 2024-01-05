# Setup for spline smoothing method

# To Do:
# clean code
# use bootstrapping to gain confidence


# ---- Load data and libraries from Setup.R file -------------------------------
source("/Users/pwecker/dev/mltwo-project/src/setup.R")
library(ggplot2)
library(gridExtra)
library(splines)
library(caret)

wine_data <- read.csv("mltwo-project/data/winequality-red.csv", sep = ";")

# Extract predictor variables (excluding 'quality')
predictors <- train[, !colnames(wine_data) %in% "quality"] # 11 vars

# Standardize the predictor variables and apply PCA
standardized_predictors <- scale(predictors) # 11 cars
pca_result <- prcomp(standardized_predictors, scale. = TRUE) # 11 vars

# Extract the scores for the first three principal components
pc_scores <- as.data.frame(pca_result$x[, 1:3])
colnames(pc_scores) <- c("PC1", "PC2", "PC3")

# Combine PC scores with 'quality'
data_pca <- cbind(pc_scores, quality = train$quality)

# List of PCs to plot
pcs_to_plot <- colnames(pc_scores)

# Create a list of violin plots for each PC
pc_violin_plots <- lapply(pcs_to_plot, function(pc) {
  ggplot(data_pca,
         aes(x = factor(quality), y = get(pc), fill = factor(quality))) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set2") +  # Adjust the color palette as needed
    labs(title = paste("Violin Plot of", pc, "by Quality"),
         x = "Quality",
         y = pc)
})

# Arrange and display the plots on the screen
grid.arrange(grobs = pc_violin_plots, ncol = 2)

# ----Modelling 3rd Order Polynomial----


# Fit a smoothing spline model using standard df (CV) on the first PC
spline_model <- smooth.spline(data_pca$PC1,
                              data_pca$quality)

# ----visualize and investigate model----
# Create a scatter plot of PC1 and 'quality'
scatter_plot <-
  ggplot(data_pca, aes(x = PC1, y = quality, color = factor(quality))) +
  geom_point(shape = 16) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Scatter Plot of PC1 by Quality", x = "PC1", y = "Quality")

# Generate x values for the smoothing spline curve
x_vals <- seq(min(data_pca$PC1), max(data_pca$PC1), length.out = 100)

# Predict y values using the smoothing spline
y_vals <- predict(spline_model, x_vals)$y

# Add the smoothing spline curve to the scatter plot
scatter_plot + 
  geom_line(data = data.frame(PC1 = x_vals, quality = y_vals),
            aes(x = PC1, y = quality), color = "red") +
  labs(title = "Scatter Plot with Smoothing Spline of PC1 by Quality")

# Access the degrees of freedom
df <- spline_model$df
cat("Degrees of Freedom:", df, "\n")

# Print the result
spline_model

# check the model performance on the validation set
vals <- validation[, !colnames(wine_data) %in% "quality"]

standardized_predictors_validation <- scale(
  vals,
  center = pca_result$center,
  scale = pca_result$scale)

pc_scores_validation <- as.data.frame(predict(pca_result, newdata = standardized_predictors_validation)[, 1:3])
colnames(pc_scores_validation) <- c("PC1", "PC2", "PC3")
data_pca_validation <- cbind(pc_scores_validation, quality = validation$quality)

# 2. Apply the smoothing spline model to the validation set
spline_predictions <- predict(spline_model, data_pca_validation$PC1)


plot(data_pca_validation$quality, spline_predictions$y, xlab = "Actual Values",
        ylab = "Predicted Values", main = "Actual vs Predicted Values",
        xlim = c(3, 8), ylim = c(3, 8))

