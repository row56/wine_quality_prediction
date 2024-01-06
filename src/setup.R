# Set seed for reproducibility
set.seed(8)
library(dplyr)

# Load the data
wine_data <- read.csv("data/winequality-red.csv", sep = ";")
# Remove duplicate rows
wine_data <- unique(wine_data)

# Add a new column with the row index
wine_data <- wine_data %>%
    mutate(row_index = row_number())

# Stratified sampling of a 60% training set split
sampled_data <- wine_data %>%
    group_by(quality) %>%
    slice_sample(prop = 0.6) %>%
    ungroup() %>%
    select(row_index)

train_indices <- sampled_data$row_index

# Stratified sampling of a 20% validation set split (50% of 40%)
remaining_data <- wine_data[!wine_data$row_index %in% train_indices, ]

validation_data <- remaining_data %>%
    group_by(quality) %>%
    slice_sample(prop = 0.5) %>%
    ungroup()

val_indices <- validation_data$row_index

# Stratified sampling of a 20% test set split (50% of 40%)
# Group and ungroup for easier investigation of the sets
test_data <- remaining_data[!remaining_data$row_index %in% val_indices, ] %>%
    group_by(quality) %>%
    ungroup()

test_indices <- test_data$row_index

# Create the datasets
train <- wine_data[train_indices, ]
validation <- wine_data[val_indices, ]
test <- wine_data[test_indices, ]

# Remove the row index column
train$row_index <- NULL
validation$row_index <- NULL
test$row_index <- NULL
wine_data$row_index <- NULL

# Also clean the environment variables not needed further
keep_vars <- c("train", "validation", "test", "wine_data")

rm(list = setdiff(ls(), keep_vars))