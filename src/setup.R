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




# Set the sizes for each dataset
#train_size <- floor(0.6 * nrow(wine_data))
#val_size <- floor(0.2 * nrow(wine_data))

# Generate indices for the train set
#train_indices <- sample(seq_len(nrow(wine_data)), size = train_size)

# Remove the train indices from the data indices
#remaining_indices <- setdiff(seq_len(nrow(wine_data)), train_indices)

# Generate indices for the validation set from the remaining indices
#val_indices <- sample(remaining_indices, size = val_size)#

# The test indices are the remaining indices after removing the validation
# indices
#test_indices <- setdiff(remaining_indices, val_indices)





# Create the datasets
train <- wine_data[train_indices, ]
validation <- wine_data[val_indices, ]
test <- wine_data[test_indices, ]

validation
test

# Also clean the environment variables not needed further
keep_vars <- c("train", "validation", "test", "wine_data")

rm(list = setdiff(ls(), keep_vars))
