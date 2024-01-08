# ---- Get models --------------------------------------------------------------

source("src/pursuit_projection.R")
ppr_model <- final_model
ppr_model_name <- final_model_name

source("src/final_spline.R")
spline_model <- final_model
spline_model_name <- final_model_name
if (grepl("PCA", spline_model_name)) {
    pca_transform <- final_transform
}

rm(final_model, final_model_name)

# ---- Get data and functions --------------------------------------------------

source("src/setup.R")
source("src/helper_functions.R")

# ---- Predict models on TEST --------------------------------------------------

# Evaluate the pursuit projection regression model on the test set
ppr_results <- evaluate_model(ppr_model, test,
    title = "Pursuit Projection Regression")

# Get the correct test subset for the spline model
if (grepl("PCA", spline_model_name)) {
    test_pca <- get_pca_transformed_data(test, pca_transform)$data # nolint
    spline_test_set <- test_pca[, c("PC1", "quality")]
} else if (grepl("Alcohol", spline_model_name)) {
    spline_test_set <- test[, c("alcohol", "quality")]
} else if (grepl("Density", spline_model_name)) {
    spline_test_set <- test[, c("density", "quality")]
} else if (grepl("pH", spline_model_name)) {
    spline_test_set <- test[, c("pH", "quality")]
} else if (grepl("Residual.Sugar", spline_model_name)) {
    spline_test_set <- test[, c("residual.sugar", "quality")]
} else if (grepl("Volatile.Acidity", spline_model_name)) {
    spline_test_set <- test[, c("volatile.acidity", "quality")]
} else {
   stop("Unknown spline model name")
}

# Evaluate the spline model on the test set
spline_results <- evaluate_model(spline_model, spline_test_set,
        title = spline_model_name)

# ---- Compare models on validation --------------------------------------------

model_names <- list(ppr_model_name,
    spline_model_name
)

# Create a list with the models
models <- list(ppr_model,
    spline_model
)
names(models) <- model_names

# Create a dataframe with the results
class_mse_vectors <- list(ppr_results$mse_per_class,
    spline_results$mse_per_class
)
names(class_mse_vectors) <- model_names

test_results <- do.call(rbind, lapply(class_mse_vectors, function(x) as.data.frame(t(x))))
rownames(test_results) <- names(class_mse_vectors)
test_results <- cbind(test_results,
                "Mean MSE over classes" = c(
                    mean(ppr_results$mse_per_class),
                    mean(spline_results$mse_per_class)
                ),
                "Total MSE" = c(
                    ppr_results$mse,
                    spline_results$mse
                ))
print(test_results)

# ---- Model selection ---------------------------------------------------------

# Select the best model according to MSE over classes
min_idx <- which.min(test_results$"Mean MSE over classes")
final_model_name <- rownames(test_results)[min_idx]
final_model <- models[[final_model_name]]

if (grepl("PCA", final_model_name)) {
    final_transform <- pca_transform
}

print("------------FINISHED------------")
print(paste("Final model:", final_model_name))