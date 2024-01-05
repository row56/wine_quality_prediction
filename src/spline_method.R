# Setup for spline smoothing method
# Perform and visualize the training
# Perform predictions

# ---- Load data and libraries from Setup.R file -------------------------------
source("src/setup.R")

wine_data <- read.csv("mltwo-project/data/winequality-red.csv", sep = ";")

# Comment from Downie:
# The spline smoothing uses just one predictor variable.
# I would like you to investigate at least 3 spline models using each time a
# different variable. Perhaps it makes sense to choose variables you think (in
# advance) will have the biggest non-linear effect on wine quality.
# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Assuming 'wine_data' is your dataset with predictor variables and 'quality'
# Adjust column names as needed based on your actual dataset

# Extract predictor variables (excluding 'quality')
predictors <- wine_data[, !colnames(wine_data) %in% "quality"]

# Standardize the predictor variables (optional but often recommended for PCA)
standardized_predictors <- scale(predictors)

# Apply PCA
pca_result <- prcomp(standardized_predictors, scale. = TRUE)

# Extract the scores for the first three principal components
pc_scores <- as.data.frame(pca_result$x[, 1:3])
colnames(pc_scores) <- c("PC1", "PC2", "PC3")

# Combine PC scores with 'quality'
data_pca <- cbind(pc_scores, quality = wine_data$quality)

# List of PCs to plot
pcs_to_plot <- colnames(pc_scores)

# Create a list of violin plots for each PC
pc_violin_plots <- lapply(pcs_to_plot, function(pc) {
  ggplot(data_pca, aes(x = factor(quality), y = get(pc), fill = factor(quality))) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set2") +  # Adjust the color palette as needed
    labs(title = paste("Violin Plot of", pc, "by Quality"),
         x = "Quality",
         y = pc)
})

# Arrange and display the plots on the screen
grid.arrange(grobs = pc_violin_plots, ncol = 2)  # You can adjust the number of columns as needed
