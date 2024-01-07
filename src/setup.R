# Set seed for reproducibility
set.seed(8)

# Load necessary libraries
library(dplyr)

# Load the wine quality data
wine_data <- read.csv("data/winequality-red.csv", sep = ";")

# Remove duplicate rows for data consistency
wine_data <- unique(wine_data)

# Add a row index for tracking original row positions
wine_data <- wine_data %>%
    mutate(row_index = row_number())

# Stratified sampling to split data into training, validation, and test sets

# Create a 60% training set
train_set_percentage <- 0.6
sampled_data <- wine_data %>%
    group_by(quality) %>%
    slice_sample(prop = train_set_percentage) %>%
    ungroup() %>%
    select(row_index)
train_indices <- sampled_data$row_index

# Split the remaining data (40%) into validation and test sets

# Prepare remaining data for splitting into validation and test
remaining_data <- wine_data[!wine_data$row_index %in% train_indices, ]

# Create a 20% validation set (50% of the remaining 40%)
validation_set_percentage <- 0.5
validation_data <- remaining_data %>%
    group_by(quality) %>%
    slice_sample(prop = validation_set_percentage) %>%
    ungroup()
val_indices <- validation_data$row_index

# Create a 20% test set (remaining 50% of the 40%)
test_data <- remaining_data[!remaining_data$row_index %in% val_indices, ] %>%
    group_by(quality) %>%
    ungroup()
test_indices <- test_data$row_index

# Construct final datasets
train <- wine_data[train_indices, ]
validation <- wine_data[val_indices, ]
test <- wine_data[test_indices, ]

# Clean up datasets by removing the row index column
train$row_index <- NULL
validation$row_index <- NULL
test$row_index <- NULL
wine_data$row_index <- NULL

# Clean the environment of variables not needed further
keep_vars <- c("train", "validation", "test", "wine_data")
rm(list = setdiff(ls(), keep_vars))