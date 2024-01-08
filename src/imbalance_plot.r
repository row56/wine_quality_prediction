source("src/setup.R")

library(ggplot2)
library(RColorBrewer)
library(dplyr)

plot_histogram <- function(quality, title = "", xlab = "", ylab = "") {
    custom_colors <- brewer.pal(6, "Dark2")

    plot <- ggplot(data.frame(Quality = quality), aes(x = Quality, fill = factor(Quality))) +
            geom_histogram(binwidth = 1, color = "black", show.legend = FALSE) +
            scale_fill_manual(values = custom_colors) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(table(quality)), by = 1)) + # Set x-axis breaks and remove expansion
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, max(table(quality)), by = 100)) + # Set y-axis breaks
            labs(
                title = title,
                x = xlab,
                y = ylab
            ) +
            theme_minimal(base_size = 14) + 
            theme(
                panel.border = element_blank(), 
                plot.background = element_rect(fill = "white", color = NA), 
                panel.background = element_rect(fill = "grey92", color = NA), 
                panel.grid.major.y = element_line(color = "white", size = 0.5), 
                panel.grid.minor.y = element_blank(), 
                panel.grid.major.x = element_blank(), 
                panel.grid.minor.x = element_line(color = "white", size = 0.5), 
                axis.ticks = element_line(color = "black"), 
                axis.text.x = element_text(color = "grey30", size = 14, vjust = 0.5),
                axis.text.y = element_text(color = "grey30", size = 14, hjust = 1),
                axis.line = element_blank(),
                axis.title.x = element_text(size = 18, face = NULL, margin = margin(t = 20)),
                axis.title.y = element_text(size = 18, face = NULL, margin = margin(r = 20)),
                plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 20)),
                axis.ticks.length = unit(0.1, "cm")) +
            scale_y_continuous(limits = c(0, 600))
    print(plot)
    return(plot)
}
plot_histogram(wine_data$quality, title = "Distribution of Wine Quality", xlab = "Quality", ylab = "Frequency")



