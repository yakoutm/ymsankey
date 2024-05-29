
#install.packages(c("ggplot2", "ggalluvial", "janitor", "dplyr"))
# Load libraries
library(ggplot2)
library(ggalluvial)
library(janitor)
library(dplyr)

#' Create Sankey Plot
#'
#' This function creates a Sankey plot for visualizing the contribution of various risk factors over time.
#'
#' @param sankey_data A data frame containing the melted data for the Sankey plot.
#' @param colors A named vector of colors for the different risk factors.
#' @param version An integer (1 or 2) specifying the version of the plot. Version 1 has vertical lines and horizontal white space, while version 2 has no white space.
#'
#' @return A ggplot object representing the Sankey plot.
#' @export
#'
#' @examples
#' colors <- c("Hypertension" = "#94B992",
#'             "Diabetes" = "#CA6C56",
#'             "Smoking" = "#87AAC2",
#'             "Hypercholesterolemia" = "#CD925F",
#'             "Obesity" = "#546C91")
#' load(system.file("data", "sankey_data.rda", package = "ymsankey"))
#' sankey_plot(sankey_data, colors, version = 1)
sankey_plot <- function(sankey_data, colors, version = 1) {
  line_width <- ifelse(version == 1, 1, 0)

  ggplot(data = sankey_data,
         aes(x = variable, y = offset_value,
             stratum = id,
             alluvium = risk_factors_for_stroke_in_blacks,
             fill = risk_factors_for_stroke_in_blacks,
             colour = id,
             label = sprintf("%.2f", value))) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback",
              color = "white", linewidth = line_width, alpha = 1, width = 0.5 ) +
    geom_stratum(color = "white", linewidth = line_width, width = 0.6, alpha = 1) +
    geom_text(position = position_stack(vjust = 0.5), color = "white", size = 3.2, fontface = "bold") +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_x_discrete(position = "top", labels = c('1990', '1995', '2000', '2005', '2010')) +
    coord_cartesian(ylim = c(-0.1, 1.6)) +
    labs(title = "Risk Factors For Stroke in Blacks", x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.65, face = "bold", size = 16),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    annotate("text", x = -0.7, y = 1.11, label = "Hypertension", hjust = 0, size = 3.8) +
    annotate("text", x = -0.7, y = 0.58, label = "Diabetes", hjust = 0, size = 3.8) +
    annotate("text", x = -0.7, y = 0.29, label = "Smoking", hjust = 0, size = 3.8) +
    annotate("text", x = -0.7, y = 0.12, label = "Hypercholesterolemia", hjust = 0, size = 3.8) +
    annotate("text", x = -0.7, y = 0.01, label = "Obesity", hjust = 0, size = 3.8)
}

