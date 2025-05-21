
#' Create a Bubble Plot for PCI Visualization
#'
#' This function generates a bubble plot to visualize the results of the PCI calculation.
#' It shows the mean action acceptability on the y-axis and the PCI value as the size of the bubbles.
#'
#' @param data A data frame containing at least the columns 'name', 'Mean', and 'PCI'.
#' @param scale_type Type of scale used: 'bipolar_with_neutral', 'bipolar_without_neutral', or 'unipolar'.
#' @param ylim_range Integer range for y-axis in bipolar_with_neutral. Default is 4.
#' @param unipolar_ylim Numeric vector of length 2 specifying the y-axis limits for unipolar scale.
#' @param xlab Label for the x-axis. Default is empty.
#' @param ylab Label for the y-axis. Default is 'Action acceptability'.
#' @param title Optional title for the plot.
#' @param bubble_color Fill color of the bubbles. Default is 'gray80'.
#' @param bubble_stroke Border color of the bubbles. Default is 'black'.
#' @param x_line Value where to draw a horizontal reference line. Defaults to 0.
#'
#' @return A ggplot2 object representing the bubble plot.
#'
#' @details
#' This plot is useful for visualizing how polarized (PCI) and how acceptable (Mean) each action/item is.
#'
#' @examples
#' data <- data.frame(
#'   name = c('A', 'B', 'C'),
#'   Mean = c(0.5, -1, 1.2),
#'   PCI = c(0.2, 0.5, 0.8)
#' )
#' bubble(data, scale_type = 'bipolar_with_neutral')
#'
#' @import ggplot2
#' @export
bubble <- function(data,
                   scale_type = c('bipolar_with_neutral', 'bipolar_without_neutral', 'unipolar'),
                   ylim_range = 4,
                   unipolar_ylim = c(1, 9),
                   xlab = '',
                   ylab = 'Action acceptability',
                   title = NULL,
                   bubble_color = 'gray80',
                   bubble_stroke = 'black',
                   x_line = NULL) {

  scale_type <- match.arg(scale_type)

  # Define y-axis and reference line based on the scale type
  if (scale_type == 'bipolar_with_neutral') {
    y_limits <- c(-ylim_range / 2, ylim_range / 2)
    if (is.null(x_line)) x_line <- 0
    scale_y <- scale_y_continuous(limits = y_limits)

  } else if (scale_type == 'bipolar_without_neutral') {
    if (is.null(x_line)) x_line <- 0
    custom_y_labels <- function(x) ifelse(x == 0, "", x)
    scale_y <- scale_y_continuous(
      limits = c(-2, 2),
      breaks = c(-2, -1, 0, 1, 2),
      labels = custom_y_labels
    )

  } else if (scale_type == 'unipolar') {
    if (is.null(x_line)) x_line <- 0
    scale_y <- expand_limits(y = x_line)
  }

  # Create the plot
  p <- ggplot(data, aes(x = name, y = Mean, size = PCI)) +
    geom_hline(yintercept = x_line, colour = 'black') +
    geom_point(shape = 21, fill = bubble_color, color = bubble_stroke, show.legend = TRUE) +
    geom_text(aes(label = round(PCI, 2)), nudge_y = 0.35, nudge_x = 0.1, size = 5) +
    xlab(xlab) + ylab(ylab) +
    scale_size_area(max_size = 14) +
    scale_y +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
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
      legend.text = element_text(size = 14)
    )

  if (!is.null(title)) {
    p <- p + ggtitle(title) +
      theme(plot.title = element_text(size = 18, face = 'bold', hjust = 0.5))
  }

  return(p)
}

