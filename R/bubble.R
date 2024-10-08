
#' Create a Bubble Plot for PCI Visualization
#'
#' This function generates a bubble plot to visualize the results of the PCI
#' calculation. It shows the mean action acceptability on the y-axis and the PCI
#' value as the size of the bubbles.
#'
#' @param df3 A data frame generated by the pci function, containing the PCI
#' values and other statistics.
#' @return A ggplot2 object representing the bubble plot.
#' @examples
#' df3 <- pci(df2)
#' p <- bubble(df3)
#' print(p)
#' @export
bubble <- function(df3) {
  ggplot(df3, aes(x = name, y = Mean, size = PCI)) +
    geom_hline(yintercept = 0, colour = 'black') +
    geom_point(color = 'gray', show.legend = TRUE) +
    geom_text(aes(label = after_stat(df3$PCI)),
              nudge_y = 0.35, nudge_x = 0.1, size = 5) +
    ylab('Action acceptability') + xlab('') +
    ylim(-1, 1) +
    scale_size_area(max_size = 14) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.line.x = element_line(colour = 'white'),
          axis.line.y = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'),
          legend.key.size = unit(1, 'cm'),
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14))
}

