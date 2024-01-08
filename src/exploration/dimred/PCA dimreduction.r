
# Comment from Downie:
# The spline smoothing uses just one predictor variable.
# I would like you to investigate at least 3 spline models using each time a
# different variable. Perhaps it makes sense to choose variables you think (in
# advance) will have the biggest non-linear effect on wine quality.


wine_data <- read.csv("mltwo-project/data/winequality-red.csv", sep = ";")


library(ggplot2)
library(gridExtra)


# Extract predictor variables
predictors <- wine_data[, !colnames(wine_data) %in% "quality"]

# Standardize the predictor variables
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
  ggplot(data_pca,
         aes(x = factor(quality), y = get(pc), fill = factor(quality))) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set2") +  # Adjust the color palette as needed
    labs(title = paste("Violin Plot of", pc, "by Quality"),
         x = "Quality",
         y = pc)
})

# Arrange and display the plots on the screen
grid.arrange(grobs = pc_violin_plots,
             ncol = 2)

# ----Modelling 3rd Order Polynomial----
library(splines)
library(ggplot2)

# Fit a smoothing spline model using standard df (CV)
spline_model <- smooth.spline(data_pca$PC1,
                              data_pca$quality)

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
print(quality_counts)
spline_model


#----Play with Bootstrapping
# Install and load the 'boot' package if not already installed
if (!requireNamespace("boot", quietly = TRUE)) {
  install.packages("boot")
}
library(boot)

# Fit the initial model
initial_model <- smooth.spline(x = data_pca$PC1, y = data_pca$quality)

# Custom function for bootstrapping and prediction
boot_fn <- function(data, indices) {
  boot_sample <- data[indices, ]
  boot_model <<- smooth.spline(x = boot_sample$PC1, y = boot_sample$quality)
  predict(boot_model, newdata = data.frame(x = data_pca$PC1))$y
}

# Perform Bootstrapping
num_bootstraps <- 1000
bootstrap_results <- boot(data_pca, boot_fn, R = num_bootstraps)

# Calculate Confidence Intervals
lower_ci <- apply(bootstrap_results$t, 2, quantile, probs = 0.025)
upper_ci <- apply(bootstrap_results$t, 2, quantile, probs = 0.975)

# Visualize Results
plot(data_pca$PC1, data_pca$quality, col = "black", pch = 16, main = "Smoothing Spline with Bootstrap CI")
lines(initial_model, col = "blue", lwd = 2)
for (i in 1:length(data_pca$PC1)) {
  lines(rep(data_pca$PC1[i], 2), c(lower_ci[i], upper_ci[i]), col = "red", lwd = 2)
}
