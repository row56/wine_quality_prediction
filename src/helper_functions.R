library(smotefamily)
library(caret)
library(ggplot2)
library(yardstick)
library(dplyr)

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

balance_classes_mixed_sampling <- function(df, target_size) {
    # Undersample the dataset
    undersampled_df <- balance_classes_by_undersampling(df, target_size)

    # Oversample the dataset
    balanced_df <- balance_classes_with_smote(undersampled_df)

    return(balanced_df)
}

build_weights <- function(data) {
    # Inverse frequency weightinghow 

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

evaluate_model <- function(model, data, title = "") {

    # Get the predictors
    predictors <- data[, !names(data) %in% "quality"]

    # Predict on the data
    predicted_values <- data.frame(predict(model, newdata = predictors))

    if (exists("y", where = predicted_values)) {
        # Case for smooth.spline
        predicted_values <- predicted_values[, "y"]
    }

    # Assuming 'original_data' is your original dataset with the discrete target
    # and 'predictions' are your PPR predictions
    combined_data <- data.frame(Actual = data$quality,
        Predicted = as.numeric(predicted_values[[1]]))

    data_count <- combined_data %>%
        group_by(Actual) %>%
        summarize(count = n())

    # Calculate the median for each group
    medians <- combined_data %>%
        group_by(Actual) %>%
        summarize(median_value = median(Predicted))

    combined_data <- merge(merge(combined_data, data_count, by = "Actual")
                            , medians, by = "Actual")

    # Compute the MSE
    mse <- mean((combined_data$Actual - combined_data$Predicted)^2)

    # Print the MSE
    print(paste("Test MSE (", title, "): ", mse, sep = ""))

    # Compute Huber Loss
    huber_loss <- huber_loss_vec(combined_data$Actual, combined_data$Predicted)

    # Print the Huber Loss
    print(paste("Test Huber Loss (", title, "): ", huber_loss, sep = ""))

    # Create a violin plot
    plot <- ggplot(
                combined_data,
                aes(x = factor(Actual), y = Predicted, fill = factor(Actual))
            ) +
            geom_violin(
                trim = FALSE,
                scale = "width",
                show.legend = FALSE
            ) +
            geom_point(
                position = position_jitter(width = 0.2),
                size = 1.5,
                alpha = 0.9,
                show.legend = FALSE
            ) +
            geom_abline(
                slope = 1,
                intercept = min(combined_data$Actual) - 1,
                color = "black",
                linewidth = 2.25,
            ) +
            geom_abline(
                slope = 1,
                intercept = min(combined_data$Actual) - 1,
                color = "skyblue",
                linewidth = 1.25,
            ) +
            geom_text(
                aes(label = count, y = median_value, hjust = 0.5),
                size = 7,
                color = "white"
            ) +
            scale_fill_brewer(palette = "Dark2"
            ) +
            labs(
                title = paste(title,
                        "\nMSE: ", round(mse, digits = 2),
                        "- Huber Loss: ", round(huber_loss, digits = 2)),
                x = "Actual Qualites",
                y = "Predicted Qualities"
            ) +
            theme(
                axis.title.x = element_text(
                    margin = margin(t = 15),
                    size = 18),
                axis.title.y = element_text(
                    margin = margin(r = 15),
                    size = 18),
                axis.text.x = element_text(
                    margin = margin(t = 10),
                    size = 14),
                axis.text.y = element_text(
                    margin = margin(r = 10),
                    size = 14),
                plot.title = element_text(
                    margin = margin(b = 20),
                    hjust = 0.5,
                    size = 22),
                plot.margin = margin(0.75, 0.75, 0.75, 0.75, "cm")
            )

    # Show the plot
    print(plot)

    # Return the Predicted Values, MSE and Huber Losses
    invisible(list(mse = mse, huber_loss = huber_loss,
        predicted_values = combined_data$Predicted))
}
