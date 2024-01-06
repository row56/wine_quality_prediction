library(smotefamily)
library(caret)
library(ggplot2)
library(yardstick)

balance_classes_with_smote <- function(df) {
    features <- df[, !names(df) %in% "quality"]
    target <- df[["quality"]]

    # Determine the target size (size of the largest class)
    target_size <- max(table(target))

    # Initialize an empty dataframe for the balanced dataset
    balanced_df <- df[0, ]

    # Loop through each class
    for (class in unique(target)) {
        class_data <- df[target == class, ]

        if (nrow(class_data) < target_size) {
            # Calculate the number of synthetic samples needed
            dup_size <- ((target_size - nrow(class_data))
                / nrow(class_data))

            # Apply SMOTE
            oversampled_data <- SMOTE(features[target == class, ],
                target[target == class], K = 2, dup_size = dup_size)$data

            colnames(oversampled_data) <- colnames(balanced_df)

            # Combine the oversampled data
            balanced_df <- rbind(balanced_df, oversampled_data)
        } else {
            balanced_df <- rbind(balanced_df, class_data)
        }
    }

    balanced_df <- data.frame(lapply(balanced_df, as.numeric))

    return(balanced_df)
}

balance_classes_by_undersampling <- function(df, target_size = 0) { # nolint
    target <- df[["quality"]]

    # Determine the target size (size of the smallest class)
    if (target_size == 0) {
        target_size <- min(table(target))
    }

    # Initialize an empty dataframe for the balanced dataset
    balanced_df <- df[0, ] # Creates an empty dataframe with same columns as df

    # Loop through each class
    for (class in unique(target)) {
        class_data <- df[target == class, ]

        # Randomly sample instances if the class is larger than the target size
        if (nrow(class_data) > target_size) {
            undersampled_data <- class_data[sample(nrow(class_data),
                                             target_size), ]
        } else {
            undersampled_data <- class_data
        }

        balanced_df <- rbind(balanced_df, undersampled_data)
    }

    return(balanced_df)
}

build_weights <- function(data) {
    # Create a matrix of weights
    weights <- c(rep(1, nrow(data)))

    counts <- table(data$quality)
    total <- sum(counts)

    # Loop over the weights and data
    for (i in seq_along(weights)) {
        # Higher weight for quality that differs from the mean
        weights[i] <- total / counts[as.character(data[i, "quality"])]
    }

    # Return the weights
    return(weights)
}

evaluate_model <- function(model, data, title_suffix = "") {
    # Predict on the data
    predicted_values <- predict(model, newdata = data)

    # Assuming 'original_data' is your original dataset with the discrete target
    # and 'predictions' are your PPR predictions
    combined_data <- data.frame(Actual = data$quality,
                                Predicted = predicted_values)

    # Create a violin plot
    plot <- ggplot(combined_data,
            aes(x = factor(Actual), y = Predicted, fill = factor(Actual))) + # nolint
            geom_violin(trim = FALSE) +
            geom_abline(slope = 1, intercept = min(combined_data$Actual) - 1,
                color = "red") +
            scale_fill_brewer(palette = "Set2") +
            labs(title = paste("Violin plot of predicted test values (",
                title_suffix, ")"),
                x = "Actual Qualites",
                y = "Predicted Values") +
            theme(plot.title = element_text(hjust = 0.5, size = 20))

    # Show the plot
    print(plot)

    # Compute the MSE
    mse <- mean((data$quality - predicted_values)^2)

    # Print the MSE
    print(paste("Test MSE (", title_suffix, "): ", mse, sep = ""))

    # Compute Huber Loss
    huber_loss <- huber_loss_vec(data$quality, predicted_values)

    # Print the Huber Loss
    print(paste("Test Huber Loss (", title_suffix, "): ", huber_loss, sep = ""))

    # Return the Predicted Values, MSE and Huber Losses
    return(list(mse = mse, huber_loss = huber_loss,
        predicted_values = predicted_values))
}