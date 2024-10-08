
#' pcir Package Functions and Setup
#'
#' This file contains the basic setup and utility functions for the 'pcir' package.
#' The pcir package is designed for calculating, comparing, and visualizing the
#' Potential for Conflict Index (PCI).
#'
#' @section Functions:
#' - `counting()`: Summarize data by calculating counts, percentages, means, and standard deviations.
#' - `pci()`: Compute the Potential for Conflict Index (PCI) from summary data.
#' - `bubble()`: Create a bubble plot to visualize PCI results.
#'
#' @name pcir-package
#' @docType package
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import Hmisc
#' @export counting
#' @export pci
#' @export bubble
NULL

#' Counting Function
#'
#' @description Create a count table with percentages, mean, and standard deviation.
#' @param df1 A data frame containing the data to be processed.
#' @return A data frame with computed statistics.
#' @examples
#' df1 <- data.frame(A = c(-1, 2, 2, 3, -1), B = c(-1, 2, 3, -1, 2),
#'                   C = c(1, 2, -2, 3, -1), D = c(3, 2, 1, -1, -2), E = c(2, 3, 1, -1, -3))
#' counting(df1)
#' @export
counting <- function(df1) {
  df1 [,-1]%>%
    #select(2:6) %>%
    pivot_longer(everything()) %>%
    group_by(name, value) %>%
    summarise(Count = n()) %>%
    group_by(name) %>%
    mutate(`%` = 100 * (Count / sum(Count)),
           Mean = weighted.mean(value, Count),
           SD = sqrt(Hmisc::wtd.var(value, Count)),
           Total = sum(Count)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'value',
                names_sep = ' ',
                values_from = c('Count', '%'),
                names_vary = 'slowest')
}

#' PCI Function
#'
#' @description Calculate the Potential for Conflict Index (PCI).
#' @param df2 A data frame generated by the `counting` function.
#' @return A data frame with the calculated PCI values for each group.
#' @examples
#' df2 <- counting(df1)
#' pci(df2)
#' @export
pci <- function(df2) {
  df2$nu <- 1 * df2$`Count -1`
  df2$na <- 1 * df2$`Count 1`
  df2$xt <- df2$nu + df2$na
  df2$z <- 1 * df2$Total
  df2$PCI <- (1 - (df2$na / df2$xt - df2$nu / df2$xt)) * df2$xt / df2$z
  df2 <- df2 %>% mutate_if(is.numeric, round, digits = 2)
  return(df2)
}

#' Bubble Plot Function
#'
#' @description Create a bubble plot to visualize PCI results.
#' @param df3 A data frame generated by the `pci` function.
#' @return A ggplot2 object representing the bubble plot.
#' @examples
#' df3 <- pci(df2)
#' bubble(df3)
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

