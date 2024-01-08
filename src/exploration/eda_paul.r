library(gridExtra)
library(corrplot)
library(ggplot2)

# EDA Paul

wine_data <- read.csv("data/winequality-red.csv", sep = ";")

#first glimpse of the data
head(wine_data, 5)

# check which variables we have
print(names(wine_data))

# check the distribution of target value "quality"
quality_counts <- table(wine_data$quality)
quality_counts

# plot quality:
hist(wine_data$quality, 
     main = "Histogram of Quality",
     xlab = "Quality",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")

# ----investigate some predictor variables
# fixed acidity
hist(wine_data$fixed.acidity, 
     main = "Histogram of Fixed Acidity",
     xlab = "Fixed Acidity",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

plot(wine_data$fixed.acidity, 
     wine_data$quality,
     main = "Scatter Plot of Fixed Acidity vs. Quality",
     xlab = "Fixed Acidity",
     ylab = "Quality",
     col = "blue",
     pch = 16)

# volatile.acidity
hist(wine_data$volatile.acidity, 
     main = "Histogram of Volatile Acidity",
     xlab = "Volatile Acidity",
     xlim = range(0, 2),
     ylim = range(0, 350),
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

plot(wine_data$volatile.acidity,
     wine_data$quality,
     main = "Scatter Plot of Volatile Acidity vs. Quality",
     xlab = "Volatile Acidity",
     ylab = "Quality",
     col = "blue",
     pch = 16)

# citric acidity
hist(wine_data$citric.acid, 
     main = "Histogram of citric acid",
     xlab = "Volatile Acidity",
     xlim = range(0, 1),
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

# Check correlation map
cor_matrix <- cor(wine_data)
corrplot(cor_matrix, method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7)

#--------------

# Set overall plot size
options(repr.plot.width = 8, repr.plot.height = 36)

# Single violin plot
single_plot <- ggplot(wine_data, aes(x = factor(quality), y = fixed.acidity, fill = factor(quality))) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Violin Plot of Fixed Acidity by Quality",
       x = "Quality",
       y = "Fixed Acidity") +
  theme_minimal()

single_plot


# create several violin plots
variables_to_plot <- colnames(wine_data)[!colnames(wine_data) %in% "quality"]

# Create a list of violin plots for each variable
violin_plots <- lapply(variables_to_plot, function(variable) {
  ggplot(wine_data, aes(x = factor(quality), y = get(variable), fill = factor(quality))) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set2") +  # Adjust the color palette as needed
    labs(title = paste("Violin Plot of", variable, "by Quality"),
         x = "Quality",
         y = variable)
})

# Arrange and print the plots
grid.arrange(grobs = violin_plots, ncol = 2, heights = c(4, 4, 4, 4, 4, 4))
