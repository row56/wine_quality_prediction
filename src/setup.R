# Set seed for reproducibility
set.seed(123)

# Load the data
wine_data <- read.csv("data/winequality-red.csv", sep = ";")

# Set the sizes for each dataset
train_size <- floor(0.6 * nrow(wine_data))
val_size <- floor(0.2 * nrow(wine_data))

# Generate indices for the train set
train_indices <- sample(seq_len(nrow(wine_data)), size = train_size)

# Remove the train indices from the data indices
remaining_indices <- setdiff(seq_len(nrow(wine_data)), train_indices)

# Generate indices for the validation set from the remaining indices
val_indices <- sample(remaining_indices, size = val_size)

# The test indices are the remaining indices after removing the validation
# indices
test_indices <- setdiff(remaining_indices, val_indices)

# Create the datasets
train <- wine_data[train_indices, ]
validation <- wine_data[val_indices, ]
test <- wine_data[test_indices, ]

# Also clean the environment variables not needed further
keep_vars <- c("train", "validation", "test", "wine_data")

rm(list = setdiff(ls(), keep_vars))