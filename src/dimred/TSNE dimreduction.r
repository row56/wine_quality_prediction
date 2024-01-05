# Example using Rtsne package
# install.packages("Rtsne")
library(Rtsne)
library(ggplot2)

# Assuming 'wine_data' is your dataset with predictor variables and 'quality'
# Adjust column names as needed based on your actual dataset

# Remove duplicates
wine_data_unique <- unique(wine_data)

# Extract predictor variables (excluding 'quality')
predictors <- wine_data_unique[, !colnames(wine_data_unique) %in% "quality"]

# Standardize the predictor variables (optional but often recommended)
standardized_predictors <- scale(predictors)

# Apply t-SNE
tsne_result <- Rtsne(standardized_predictors)

# Convert t-SNE results to a data frame
tsne_data <- data.frame(Quality = wine_data_unique$quality, Axis1 = tsne_result$Y[, 1])

# Create a violin plot
ggplot(tsne_data, aes(x = factor(Quality), y = Axis1, fill = factor(Quality))) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Violin Plot of t-SNE Axis 1 by Quality", x = "Quality", y = "t-SNE Axis 1")

