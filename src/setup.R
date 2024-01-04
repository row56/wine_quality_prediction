# Set seed for reproducibility
set.seed(123)

# Load the data
wine_data <- read.csv("data/winequality-red.csv", sep = ";")

# Split the data
train_size <- floor(0.8 * nrow(wine_data))
train_indices <- sample(seq_len(nrow(wine_data)), size = train_size)

train <- wine_data[train_indices, ]
test <- wine_data[-train_indices, ]

# Also clean the environment variables not needed further
keep_vars <- c("train", "test", "wine_data")

rm(list = setdiff(ls(), keep_vars))