# Example using cmdscale function in base R
library(ggplot2)

# Assuming 'wine_data' is your dataset with predictor variables and 'quality'
# Adjust column names as needed based on your actual dataset

# Remove duplicates
wine_data_unique <- unique(wine_data)

# Extract predictor variables (excluding 'quality')
predictors <- wine_data_unique[, !colnames(wine_data_unique) %in% "quality"]

# Standardize the predictor variables (optional but often recommended)
standardized_predictors <- scale(predictors)

# Apply MDS
mds_result <- cmdscale(dist(standardized_predictors))

# Convert MDS results to a data frame
mds_data <- data.frame(Quality = wine_data_unique$quality, Axis1 = mds_result[, 1])

# Create a violin plot
ggplot(mds_data, aes(x = factor(Quality), y = Axis1, fill = factor(Quality))) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Violin Plot of MDS Axis 1 by Quality", x = "Quality", y = "MDS Axis 1")
