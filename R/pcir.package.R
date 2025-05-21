
#' pcir Package Functions and Setup
#'
#' This file contains the basic setup and utility functions for the 'pcir' package.
#' The pcir package is designed for calculating, comparing, and visualizing the
#' Potential for Conflict Index (PCI).
#'
#' @section Functions:
#' - `counting()`: Summarize data by calculating counts, percentages, means, and standard deviations.
#' - `pci()`: Compute the Potential for Conflict Index (PCI2) from summary data.
#' - `bubble()`: Create a bubble plot to visualize PCI results.
#'
#' @name pcir-package
#' @keywords internal
#' @import Hmisc
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @import tidyr
#' @import devtools
#' @import roxygen2
#' @import rprojroot
#' @export counting
#' @export pci
#' @export bubble
NULL

#' Counting Function
#'
#' @description Create a count table with percentages, mean, and standard deviation.
#' This function takes a data frame, computes summary statistics like counts, percentages,
#' means, and standard deviations for each unique value in the selected columns.
#'
#' @param df1 A data frame containing the data to be processed.
#' @param cols A vector of column names to be included in the calculation.
#'
#' @return A data frame in wide format including:
#' \itemize{
#'   \item Count and percentage for each value per variable
#'   \item Weighted mean
#'   \item Weighted standard deviation (SD)
#'   \item Total number of observations per variable
#' }
#'
#' @examples
#' df1 <- data.frame(
#'   A = c(-1, -1, -1, 0, -1),
#'   B = c(-1, 1, 0, -1, 1),
#'   C = c(0, 0, 1, 0, -1),
#'   D = c(0, -1, 1, 1, 1),
#'   E = c(1, 1, 0, -1, -1)
#' )
#' counting(df1, cols = c('A', 'B', 'C', 'D', 'E'))
#'
#' @export
counting <- function(df1, cols) {
  df1 %>%
    dplyr::select(all_of(cols)) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::group_by(name, value) %>%
    dplyr::summarise(Count = n(), .groups = 'drop_last') %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      Percentage = round(100 * (Count / sum(Count)), 2),
      Mean = round(weighted.mean(value, Count), 2),
      SD = round(sqrt(Hmisc::wtd.var(value, Count)), 2),
      Total = sum(Count)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = 'value',
      names_sep = ' ',
      values_from = c('Count', 'Percentage'),
      names_vary = 'slowest'
    )
}

#' PCI Function
#'
#' @description Calculate the Potential for Conflict Index (PCI).
#' This function computes the PCI2 (a generalized Potential for Conflict Index) for each item in a
#' summarized data frame (typically from the `counting()` function), or for a single named numeric vector
#' of counts. It works with three types of scales: `bipolar_with_neutral`, `bipolar_without_neutral`, and `unipolar`.
#'
#' @param data A data frame with columns named like 'Count X' (where X is a scale value),
#' or a named numeric vector of counts.
#' @param scale_type Type of scale: `'bipolar_with_neutral'`, `'bipolar_without_neutral'`, or `'unipolar'`.
#' @param min_scale_value The minimum value of the scale (e.g., -2 or 1).
#' @param max_scale_value The maximum value of the scale (e.g., 2 or 9).
#' @param exponent A number to raise distances to. Use `1` for linear distance,
#' `2` for squared distance, etc. Default is `1`.
#'
#' @return If `data` is a data frame, it returns the same data frame with an added `PCI` column.
#' If `data` is a named numeric vector, it returns the PCI value directly.
#'
#' @details
#' For unipolar scales, PCI2 is based on all pairwise distances between categories.
#'
#' For bipolar scales, PCI2 considers distances between opposing poles, ignoring neutral (0) in
#' the case of `bipolar_without_neutral`. In `bipolar_with_neutral`, 0 is included in the scale
#' but not in the calculation of conflict.
#'
#' The maximum possible polarization (used for normalization) assumes a perfectly split distribution.
#'
#' @examples
#' df1 <- data.frame(
#'   A = c(-1, -1, -1, 0, -1),
#'   B = c(-1, 1, 0, -1, 1),
#'   C = c(0, 0, 1, 0, -1),
#'   D = c(0, -1, 1, 1, 1),
#'   E = c(1, 1, 0, -1, -1)
#' )
#' df2 <- counting(df1, cols = c('A', 'B', 'C', 'D', 'E'))
#' pci(df2, scale_type = 'bipolar_with_neutral', min_scale_value = -1, max_scale_value = 1)
#'
#' @importFrom dplyr mutate select all_of
#' @importFrom purrr pmap_dbl
#' @export
pci <- function(data,
                scale_type = c('bipolar_with_neutral', 'bipolar_without_neutral', 'unipolar'),
                min_scale_value,
                max_scale_value,
                exponent = 1) {

  scale_type <- match.arg(scale_type)

  # Internal function for unipolar scale
  calc_unipolar <- function(counts_vector) {
    counts_vector[is.na(counts_vector)] <- 0
    total_actual_distance <- 0
    for (i in min_scale_value:(max_scale_value - 1)) {
      for (j in (i + 1):max_scale_value) {
        count_i <- counts_vector[paste0('Count ', i)]
        count_j <- counts_vector[paste0('Count ', j)]
        distance <- (j - i)^exponent
        total_actual_distance <- total_actual_distance + 2 * count_i * count_j * distance
      }
    }
    total_responses <- sum(counts_vector)
    if (total_responses == 0) return(NA_real_)
    max_distance <- (max_scale_value - min_scale_value)^exponent
    if (total_responses %% 2 == 0) {
      max_total_distance <- max_distance * total_responses^2 / 2
    } else {
      max_total_distance <- max_distance * (total_responses + 1) * (total_responses - 1) / 2
    }
    pci2 <- total_actual_distance / max_total_distance
    return(pci2)
  }

  # Internal function for bipolar scale with neutral (e.g., -2 to 2)
  calc_bipolar_with_neutral <- function(counts_vector) {
    counts_vector[is.na(counts_vector)] <- 0
    scale_values <- as.numeric(gsub('Count ', '', names(counts_vector)))
    names(counts_vector) <- as.character(scale_values)
    scale_values <- sort(scale_values)
    total_actual_distance <- 0
    total_responses <- sum(counts_vector)
    if (total_responses == 0) return(NA_real_)
    negative_values <- scale_values[scale_values < 0]
    positive_values <- scale_values[scale_values > 0]
    for (i in negative_values) {
      for (j in positive_values) {
        count_i <- counts_vector[as.character(i)]
        count_j <- counts_vector[as.character(j)]
        distance <- (abs(i) + abs(j) - 1)^exponent
        total_actual_distance <- total_actual_distance + 2 * count_i * count_j * distance
      }
    }
    max_distance <- (abs(min_scale_value) + abs(max_scale_value) - 1)^exponent
    if (total_responses %% 2 == 0) {
      max_total_distance <- max_distance * total_responses^2 / 2
    } else {
      max_total_distance <- max_distance * (total_responses + 1) * (total_responses - 1) / 2
    }
    pci2 <- total_actual_distance / max_total_distance
    return(pci2)
  }

  # Internal function for bipolar scale without neutral (e.g., -2 to 2 but no 0)
  calc_bipolar_without_neutral <- function(counts_vector) {
    counts_vector[is.na(counts_vector)] <- 0
    scale_values <- as.numeric(gsub('Count ', '', names(counts_vector)))
    names(counts_vector) <- as.character(scale_values)
    scale_values <- sort(scale_values)
    total_actual_distance <- 0
    total_responses <- sum(counts_vector)
    if (total_responses == 0) return(NA_real_)
    negative_values <- scale_values[scale_values < 0]
    positive_values <- scale_values[scale_values > 0]
    for (i in negative_values) {
      for (j in positive_values) {
        count_i <- counts_vector[as.character(i)]
        count_j <- counts_vector[as.character(j)]
        distance <- (abs(i) + abs(j))^exponent
        total_actual_distance <- total_actual_distance + 2 * count_i * count_j * distance
      }
    }
    max_distance <- (abs(min_scale_value) + abs(max_scale_value))^exponent
    if (total_responses %% 2 == 0) {
      max_total_distance <- max_distance * total_responses^2 / 2
    } else {
      max_total_distance <- max_distance * (total_responses + 1) * (total_responses - 1) / 2
    }
    pci2 <- total_actual_distance / max_total_distance
    return(pci2)
  }

  # Compute PCI based on input type
  if (is.data.frame(data)) {
    # Extract relevant count columns per variable and calculate PCI row-wise
    count_cols <- grep('^Count ', names(data), value = TRUE)
    pci_values <- purrr::pmap_dbl(data[count_cols], function(...) {
      counts_vector <- c(...)
      names(counts_vector) <- count_cols
      switch(
        scale_type,
        bipolar_with_neutral = calc_bipolar_with_neutral(counts_vector),
        bipolar_without_neutral = calc_bipolar_without_neutral(counts_vector),
        unipolar = calc_unipolar(counts_vector)
      )
    })
    dplyr::mutate(data, PCI = pci_values)
  } else if (is.numeric(data) && !is.null(names(data))) {
    # Single vector of counts named as 'Count X'
    switch(
      scale_type,
      bipolar_with_neutral = calc_bipolar_with_neutral(data),
      bipolar_without_neutral = calc_bipolar_without_neutral(data),
      unipolar = calc_unipolar(data)
    )
  } else {
    stop(paste0('Input must be a named numeric vector or a data.frame with columns like: ', paste(count_cols, collapse = ', ')))

  }
}

#' Bubble Plot Function
#'
#' @description Create a bubble plot to visualize PCI results.
#' This function generates a bubble plot to visualize PCI results.
#' Each bubble represents an item, with size proportional to total responses,
#' x-axis representing weighted mean, and color representing the PCI value.
#'
#' @param data A data frame containing at least the columns `name`, `Mean`, and `PCI`.
#' @param scale_type The type of response scale. One of 'bipolar_with_neutral', 'bipolar_without_neutral', or 'unipolar'.
#' @param ylim_range Numeric value to define y-axis range for bipolar scales.
#' @param unipolar_ylim A numeric vector of length 2 specifying y-axis limits for unipolar scale.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param title Title for the plot.
#' @param bubble_color Fill color for the bubbles.
#' @param bubble_stroke Border color for the bubbles.
#' @param x_line Position of the horizontal reference line.
#'
#' @return A ggplot2 object.
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
    custom_y_labels <- function(x) ifelse(x == 0, '', x)
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
    p <- p + ggtitle(title)
  }

  print(p)
}

