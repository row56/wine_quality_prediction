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
        weights[i] <- (total / counts[as.character(data[i, "quality"])])
    }

    # Return the weights
    return(weights)
}

evaluate_model <- function(model, data, title = "") {

    # Get the predictors
    predictors <- data[, !names(data) %in% "quality", drop = FALSE]

    if ("smooth.spline" %in% class(model)) {
        # Case for smooth.spline
        predicted_values <- as.vector(predict(model, predictors)$y)
    } else {
        # Case for ppr
        predicted_values <- data.frame(predict(model, newdata = predictors))
    }

    actual <- data$quality
    predicted <- as.numeric(predicted_values[[1]])

    # Compute the MSE
    mse <- mean((actual - predicted)^2)

    # Print the MSE
    print(paste("Test MSE (", title, "): ", mse, sep = ""))

    # Compute Huber Loss
    huber_loss <- huber_loss_vec(actual, predicted)

    # Print the Huber Loss
    print(paste("Test Huber Loss (", title, "): ", huber_loss, sep = ""))

    # Create a violin plot
    plot <- create_violin_plot(actual, predicted, title)

    # Show the plot
    print(plot)

    # Return the Predicted Values, MSE and Huber Losses
    invisible(list(mse = mse, huber_loss = huber_loss,
        predicted_values = predicted))
}

create_violin_plot <- function(
    actual,
    predicted,
    title = "",
    show_loss = TRUE,
    show_abline = TRUE,
    xlab = "Actual Qualities",
    ylab = "Predicted Qualities") {

    combined_data <- data.frame(Actual = actual,
        Predicted = predicted)

    data_count <- combined_data %>%
        group_by(Actual) %>%
        summarize(count = n())

    # Calculate the median for each group
    medians <- combined_data %>%
        group_by(Actual) %>%
        summarize(median_value = median(Predicted))

    combined_data <- merge(merge(combined_data, data_count, by = "Actual"),
                            medians, by = "Actual")

    if (show_loss) {
        # Compute Huber Loss
        huber_loss <- huber_loss_vec(actual, predicted)
        # Compute the MSE
        mse <- mean((actual - predicted)^2)

        plt_title <- paste(title,
                        "\nMSE: ", round(mse, digits = 2),
                        "- Huber Loss: ", round(huber_loss, digits = 2))
    } else {
        plt_title <- title
    }

    # Create the violin plot
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
            )

    if (show_abline) {
        plot <- plot +
                geom_abline(
                    slope = 1,
                    intercept = min(combined_data$Actual) - 1,
                    color = "black",
                    linewidth = 2.25,
                ) +
                geom_abline(
                    slope = 1,
                    intercept = min(combined_data$Actual) - 1,
                    color = "#3DDC84",
                    linewidth = 1.25,
                )
    }

    plot <- plot +
            geom_text(
                aes(label = count, y = median_value, hjust = 0.5),
                size = 7,
                color = "white"
            ) +
            scale_fill_brewer(palette = "Dark2"
            ) +
            labs(
                title = plt_title,
                x = xlab,
                y = ylab
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

    return(plot)
}

plot_spline_curve <- function(spline_obj, quality, predictor, title = "", xlab = "", ylab = "") { # nolint
    
    combined_data <- data.frame(
        Quality = quality,
        Predictor = predictor
    )

    x_values <- seq(min(predictor), max(predictor), length.out = 300)
    spline_pred <- as.vector(predict(spline_obj, x_values)$y)

    # Calculate the median for each group
    medians <- combined_data %>%
        group_by(Quality) %>%
        summarize(median_value = median(Predictor))

    count <- combined_data %>%
        group_by(Quality) %>%
        summarize(count = n())

    combined_data <- merge(combined_data, medians, by = "Quality")
    combined_data <- merge(combined_data, count, by = "Quality")

    # Create the violin plot
    plot <- ggplot(
            ) +
            geom_violin(
                data = combined_data,
                aes(x = Predictor, y = Quality, fill = factor(Quality)),
                trim = FALSE,
                scale = "width",
                show.legend = FALSE
            ) +
            geom_point(
                data = combined_data,
                aes(x = Predictor, y = Quality, fill = factor(Quality)),
                position = position_jitter(height = 0.2),
                size = 1.5,
                alpha = 0.9,
                show.legend = FALSE
            ) +
            geom_line(
                aes(x = x_values, y = spline_pred),
                color = "black",
                linewidth = 2.25
            ) +
            geom_line(
                aes(x = x_values, y = spline_pred),
                color = "skyblue",
                linewidth = 1.25
            ) +
            geom_text(
                data = combined_data,
                aes(label = count, x = median_value, y = Quality, vjust = 0.5),
                size = 7,
                color = "white"
            ) +
            scale_fill_brewer(
                palette = "Dark2"
            ) +
            labs(
                data = combined_data,
                aes(x = Predictor, y = Quality, fill = factor(Quality)),
                title = title,
                x = xlab,
                y = ylab
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

    print(plot)
    return(plot)
}

tune_spline_PARAMNAMEHERE <- function(train_data, val_data, weights = NULL, title_suffix = "") {

    # Get the predictors
    train_predictor <- train_data[, !names(train_data) %in% "quality", drop = FALSE]
    val_predictor <- val_data[, !names(train_data) %in% "quality", drop = FALSE]

    if (ncol(train_predictor) > 1) {
        stop("More than one predictor found, not supported for spline_smoothing.
            Subset the data to one predictor and quality column.")
    }

    PARAMNAMEHERE_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) # TODO: Insert param_values here

    # Create a vector for Huber losses
    huber <- rep(0, length(PARAMNAMEHERE_values))

    # Loop over the param_values
    for (i in seq_along(PARAMNAMEHERE_values)) {

        if (is.null(weights)) {
            args <- list(
                x = train_predictor,
                y = train_data$quality,
                PARAMNAMEHERE = param_values[i]
        )
        } else {
            args <- list(
                x = train_predictor,
                y = train_data$quality,
                w = weights,
                PARAMNAMEHERE = param_values[i]
            )
        }

        # Fit the model
        spline_obj <- do.call(smooth.spline, args, quote = TRUE)

        # Predict on the validation set
        val_spline <- as.vector(predict(spline_obj, val_predictor)$y)

        # Compute huber loss
        huber[i] <- huber_loss_vec(val_data$quality, val_spline)
    }

    # Plot the Huber losses
    plot(1:max_terms, huber, type = "b", xlab = "PARAMNAMEHERE",
        ylab = "Huber Loss", main = paste("Huber Loss vs PARAMNAMEHERE (",
        title_suffix, ")"))

    # Highlight the minimum Huber loss
    points(which.min(huber), huber[which.min(huber)], col = "red", pch = 19)

    best_huber <- huber[which.min(huber)]
    best_PARAMNAMEHERE <- which.min(huber)

    # Print the minimum Huber loss
    print(paste("Minimum Huber with PARAMNAMEHERE = ", best_PARAMNAMEHERE, ": ", best_huber))

    # Return the best Huber loss and PARAMNAMEHERE
    return(list(best_huber = best_huber, best_PARAMNAMEHERE = best_PARAMNAMEHERE,
        all_huber = huber))
}