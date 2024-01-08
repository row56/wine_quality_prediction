# ---- Get models --------------------------------------------------------------

# Lock the environment to prevent accidental changes
source("src/spline_smoothing_pca.R")
spline_pca_model <- final_model
spline_pca_model_name <- final_model_name
pca_transform <- final_transform

source("src/spline_smoothing_alcohol.R")
spline_alcohol_model <- final_model
spline_alcohol_model_name <- final_model_name

source("src/spline_smoothing_density.R")
spline_density_model <- final_model
spline_density_model_name <- final_model_name

source("src/spline_smoothing_pH.R")
spline_pH_model <- final_model
spline_pH_model_name <- final_model_name

source("src/spline_smoothing_residualsugar.R")
spline_residualsugar_model <- final_model
spline_residualsugar_model_name <- final_model_name

source("src/spline_smoothing_volacidity.R")
spline_volacidity_model <- final_model
spline_volacidity_model_name <- final_model_name

rm(final_model, final_model_name, final_transform)

# ---- Get data and functions ---------------------------------------------------

source("src/setup.R")
source("src/helper_functions.R")

# ---- Predict models on validation --------------------------------------------

validation_pca <- get_pca_transformed_data(validation, pca_transform)$data # nolint
pca_results <- evaluate_model(spline_pca_model, validation_pca[, c("PC1", "quality")],
    title = spline_pca_model_name, show_plot = FALSE)

alcohol_results <- evaluate_model(spline_alcohol_model, validation[, c("alcohol", "quality")],
    title = spline_alcohol_model_name, show_plot = FALSE)


density_results <- evaluate_model(spline_density_model, validation[, c("density", "quality")],
    title = spline_density_model_name, show_plot = FALSE)

pH_results <- evaluate_model(spline_pH_model, validation[, c("pH", "quality")],
    title = spline_pH_model_name, show_plot = FALSE)

residualsugar_results <- evaluate_model(spline_residualsugar_model, 
    validation[, c("residual.sugar", "quality")],
    title = spline_residualsugar_model_name, show_plot = FALSE)

volacidity_results <- evaluate_model(spline_volacidity_model,
    validation[, c("volatile.acidity", "quality")],
    title = spline_volacidity_model_name, show_plot = FALSE)

# ---- Compare models on validation --------------------------------------------

model_names <- list(spline_pca_model_name,
    spline_alcohol_model_name,
    spline_density_model_name,
    spline_pH_model_name,
    spline_residualsugar_model_name,
    spline_volacidity_model_name
)

# Create a list with the models
models <- list(spline_pca_model,
    spline_alcohol_model,
    spline_density_model,
    spline_pH_model,
    spline_residualsugar_model,
    spline_volacidity_model
)
names(models) <- model_names

# Create a dataframe with the results
class_mse_vectors <- list(pca_results$mse_per_class,
    alcohol_results$mse_per_class,
    density_results$mse_per_class,
    pH_results$mse_per_class,
    residualsugar_results$mse_per_class,
    volacidity_results$mse_per_class
)
names(class_mse_vectors) <- model_names

val_results <- do.call(rbind, lapply(class_mse_vectors, function(x) as.data.frame(t(x))))
rownames(val_results) <- names(class_mse_vectors)
val_results <- cbind(val_results,
                "Mean MSE over classes" = c(
                    mean(pca_results$mse_per_class),
                    mean(alcohol_results$mse_per_class),
                    mean(density_results$mse_per_class),
                    mean(pH_results$mse_per_class),
                    mean(residualsugar_results$mse_per_class),
                    mean(volacidity_results$mse_per_class)),
                "Total MSE" = c(
                    pca_results$mse,
                    alcohol_results$mse,
                    density_results$mse,
                    pH_results$mse,
                    residualsugar_results$mse,
                    volacidity_results$mse)
                )
print(val_results)
# ---- Model selection ---------------------------------------------------------

# Select the best model according to MSE over classes
min_idx <- which.min(val_results$"Mean MSE over classes")
final_model_name <- rownames(val_results)[min_idx]
final_model <- models[[final_model_name]]

if (grepl("PCA", final_model_name)) {
    final_transform <- pca_transform
}

# ---- Clean up ----------------------------------------------------------------

rm(list = setdiff(ls(), keep_vars))