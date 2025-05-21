available::available("pcir", browse = FALSE, user = "fblpalmeira")

library(devtools)
create_package("D:/Francesca/pcir")

library(usethis)
use_package("Hmisc")
use_package("dplyr")
use_package("tidyr")
use_package("ggplot2")

# Set the directory for saving R scripts
r_directory <- file.path("D:/Francesca/pcir", "R")

# Ensure the R directory exists
if (!dir.exists(r_directory)) {
  dir.create(r_directory)
}

# Set the local directory
local_dir <- "D:/Francesca/pcir"

counting_code <- "
#' Create a Count Table with Percentages, Mean, and SD
#'
#' This function takes a data frame and computes the count and percentage of
#' each unique value across specified columns. It also calculates the weighted mean
#' and weighted standard deviation for each variable.
#'
#' The output is useful for summarizing responses from ordinal or Likert-type items,
#' showing how values are distributed and summarized across variables.
#'
#' @param df1 A data frame containing the variables to summarize.
#' @param cols A character vector with the names of the columns to include in the analysis.
#'
#' @return A data frame in wide format including:
#' \\itemize{
#'   \\item Counts and percentages for each unique value.
#'   \\item Weighted mean and standard deviation.
#' }
#'
#' @export
counting <- function(df1, cols = names(df1)) {
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
"

writeLines(counting_code, file.path(r_directory, "counting.R"))

pci_code <- "
#' Calculate the Potential for Conflict Index (PCI2)
#'
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

  # Internal function for bipolar scale without neutral (e.g., -2, -1, 1, 2)
  calc_bipolar_without_neutral <- function(counts_vector) {
    if ('Count 0' %in% names(counts_vector)) {
      stop('Scale must not include value zero (neutral) for bipolar_without_neutral.')
    }
    counts_vector[is.na(counts_vector)] <- 0
    scale_values <- as.numeric(gsub('Count ', '', names(counts_vector)))
    expected_values <- min_scale_value:max_scale_value
    expected_values <- expected_values[expected_values != 0]
    for (val in expected_values) {
      label <- paste0('Count ', val)
      if (!label %in% names(counts_vector)) {
        counts_vector[label] <- 0
      }
    }
    counts_vector <- counts_vector[paste0('Count ', expected_values)]
    negatives <- expected_values[expected_values < 0]
    positives <- expected_values[expected_values > 0]
    total_actual_distance <- 0
    for (i in negatives) {
      for (j in positives) {
        count_i <- counts_vector[[paste0('Count ', i)]]
        count_j <- counts_vector[[paste0('Count ', j)]]
        distance <- (abs(i) + abs(j) - 1)^exponent
        total_actual_distance <- total_actual_distance + 2 * count_i * count_j * distance
      }
    }
    total_responses <- sum(counts_vector)
    if (total_responses == 0) return(NA_real_)
    max_distance <- (abs(min_scale_value) + abs(max_scale_value) - 1)^exponent
    if (total_responses %% 2 == 0) {
      max_total_distance <- max_distance * total_responses^2 / 2
    } else {
      max_total_distance <- max_distance * (total_responses + 1) * (total_responses - 1) / 2
    }
    pci2 <- total_actual_distance / max_total_distance
    return(pci2)
  }

  # If input is a data frame
  if (is.data.frame(data)) {
    count_cols <- paste0('Count ', min_scale_value:max_scale_value)
    if (!all(count_cols %in% names(data))) {
      stop(paste0('The data.frame must contain columns from ', paste(count_cols, collapse = ', ')))
    }
    data <- dplyr::mutate(
      data,
      PCI = purrr::pmap_dbl(
        .l = dplyr::select(data, dplyr::all_of(count_cols)),
        .f = function(...) {
          counts <- c(...)
          names(counts) <- count_cols
          switch(scale_type,
                 'unipolar' = calc_unipolar(counts),
                 'bipolar_with_neutral' = calc_bipolar_with_neutral(counts),
                 'bipolar_without_neutral' = calc_bipolar_without_neutral(counts))
        }
      )
    )
    return(data)
  }

  # If input is a named numeric vector
  else if (is.numeric(data) && !is.null(names(data))) {
    switch(scale_type,
           'unipolar' = calc_unipolar(data),
           'bipolar_with_neutral' = calc_bipolar_with_neutral(data),
           'bipolar_without_neutral' = calc_bipolar_without_neutral(data))
  }

  else {
    stop(paste0('Input must be a named numeric vector or a data.frame with the following columns: ', paste(count_cols, collapse = ', ')))

  }
}
"

writeLines(pci_code, file.path(r_directory, "pci.R"))

# Define and save the updated bubble function
bubble_code <- "
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
    custom_y_labels <- function(x) ifelse(x == 0, \"\", x)
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
"

writeLines(bubble_code, file.path(r_directory, "bubble.R"))


# Content for the pcir.package.R file
pcir_package_code <- "
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
#' \\itemize{
#'   \\item Count and percentage for each value per variable
#'   \\item Weighted mean
#'   \\item Weighted standard deviation (SD)
#'   \\item Total number of observations per variable
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
"

# Write the content to a file called pcir.package.R
writeLines(pcir_package_code, file.path(r_directory, "pcir.package.R"))

# Define o conteúdo do DESCRIPTION com coautoria e todos os metadados
description_text <- "
Package: pcir
Title: Potential for Conflict Index in R
Version: 0.0.0.9000
Authors@R: c(
    person('Francesca Belem Lopes', 'Palmeira', email = 'francesca@alumni.usp.br', role = c('aut', 'cre'),
            comment = c(ORCID = '0000-0002-7597-1157')),
    person('Bruna', 'Wundervald', email = 'brunadaviesw@gmail.com', role = 'aut',
            comment = c(ORCID = '0000-0001-8163-220X')))
Description: Provides functions to calculate, compare, and visualize the Potential for Conflict Index (PCI),
    a descriptive statistic that summarizes the level of agreement or disagreement among stakeholders.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Imports:
    Hmisc,
    dplyr,
    ggplot2,
    magrittr,
    tidyr,
    stats,
    utils,
    devtools,
    roxygen2,
    rprojroot
Suggests:
    testthat,
    tibble
Config/Needs/website: true
URL: https://github.com/fblpalmeira/pcir,
     https://fblpalmeira.github.io/pcir
BugReports: https://github.com/fblpalmeira/pcir/issues
Date: 2025-05-21
"

# Write the description to a DESCRIPTION file
writeLines(description_text, file.path(local_dir, "DESCRIPTION"))

# Define the content of the MIT License
license_text <- "
MIT License

Copyright (c) 2025 Francesca Palmeira

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"

# Write the license to a LICENSE file
writeLines(license_text, file.path(local_dir, "LICENSE"))

# Create a citation entry for the pcir package
citation_text <- citation("pcir")

# Convert the citation list to a string with each element on a new line
citation_text_str <- paste(citation_text, collapse = "\n")

# Save the citation to a file
cat(citation_text_str, file = "citation.txt")

# You can also print it to the console to view
print(citation_text)

# Install required packages if not already installed
#install.packages("gh")        # For interacting with the GitHub API
#install.packages("writexl")   # For writing to Excel files

# Load necessary libraries
library(gh)
library(writexl)

# Define your repository details
repo <- "fblpalmeira/pcir"  # Replace with your GitHub repository, e.g., 'user/repo'

# Fetch the issues from the repository
issues <- gh("GET /repos/:owner/:repo/issues", owner = "fblpalmeira", repo = "pcir")

# Process the issues into a data frame
issues_df <- data.frame(
  number = sapply(issues, function(x) x$number),
  title = sapply(issues, function(x) x$title),
  state = sapply(issues, function(x) x$state),
  created_at = sapply(issues, function(x) x$created_at),
  updated_at = sapply(issues, function(x) x$updated_at),
  body = sapply(issues, function(x) x$body)
)

# Define the file path for the spreadsheet
file_path <- file.path(getwd(), "github_issues.xlsx")

# Write the issues data to an Excel file
write_xlsx(issues_df, file_path)

# Output message indicating the spreadsheet has been created
cat("The GitHub Issues spreadsheet has been created at", file_path, "\n")

# Define the content of README.Rmd
readme_rmd_content <- "
---
output: github_document
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  fig.path = 'man/figures/logo_pcir.png',
  out.width = '100%',
  eval = TRUE)

library(pcir)
```

# pcir: Potential for Conflict Index in R <a href='https://fblpalmeira.github.io/pcir/'><img src='man/figures/pcir_logo.png' alt='pcir website' align='right' height='139'/></a>

## Overview

`pcir` is an R package developed to assist researchers and practitioners in calculating,
comparing, and visualizing the Potential for Conflict Index (PCI) among stakeholders.
PCI is a descriptive statistical method designed to enhance the understanding of
outcomes in human dimensions research [(Manfredo et al. 2003;](https://www.tandfonline.com/doi/abs/10.1080/10871200304310)
[Vaske et al. 2010)](https://www.tandfonline.com/doi/abs/10.1080/01490401003712648).

The concepts of consensus, disagreement, and conflict are relevant across a wide
range of disciplines, including economics, political science, psychology,
sociology, and natural resource management. Although PCI can be calculated using
software such as Excel, SPSS, and SAS, no dedicated R package existed for this
analysis — until now.

This package was developed as part of my training in the
[rOpenSci Champions Program](https://ropensci.org/champions/),
supported by the Chan Zuckerberg Initiative.

Additional information:

- [Introducing rOpenSci Champions - Cohort 2023-2024](https://ropensci.org/blog/2024/02/15/champions-program-champions-2024/)

## Theoretical approach

<img src='man/figures/likert_scales1.png'  align='center' height='400'/>

**Figure 1.** Likert scales used in the Potential for Conflict Index (PCI).

## Workflow

Steps implemented in the 'pcir' package:

 1. Read the input data from interviews – see example dataset [(Spreadsheet)]();

 2. Count the frequency of responses for each question – see [(Table 1)]();

 3. Calculate the PCI for each question – see [(Table 2)]();

 4. Generate a bubble chart to visualize the results – see [(Figure)]().

<img src='man/figures/diagrammer_pcir.png' align='center'>

**Figure 2.** Workflow of the `pcir` package.

## Features

- `counting()`: Summarizes data by calculating counts, percentages, means, and standard deviations.

- `pci()`: Computes the Potential for Conflict Index from summary data.

- `bubble()`: Visualizes PCI results using a bubble plot.

## Installation

Install the development version of `pcir` directly from GitHub:

```r
# Uncomment the line below if devtools is not installed
# install.packages(\"devtools\")

devtools::install_github(\"fblpalmeira/pcir\")
```

## Usage

After installation, load the package:

```r
library(pcir)
```

Example dataset:

```r
df1 <- data.frame(
  A = c(-1, -1, -1, 0, -1),
  B = c(-1, 1, 0, -1, 1),
  C = c(0, 0, 1, 0, -1),
  D = c(0, -1, 1, 1, 1),
  E = c(1, 1, 0, -1, -1)
)
```

Count responses:

```r
data <- counting(df1, cols = names(df1)[2:6])
data
```

Calculate PCI:

```r
data <- pci(data,
                 scale_type = 'bipolar_with_neutral',
                 min_scale_value = -1,
                 max_scale_value = 1)
```

Visualize with a bubble plot:

```r
# The bubble function creates a bubble plot to visualize the PCI results.
# You can customize the colors and title as per your preferences.
plot <- bubble(data = df3,
               scale_type = 'bipolar_with_neutral',
               ylim_range = 3,
               bubble_color = 'lightblue',
               bubble_stroke = 'darkblue',
               title = 'Custom Bubble Colors')
plot

```

<img src='man/figures/output_pci.png'>

**Figure 3.** Bubble chart illustrating the Potential for Conflict Indices.

## References

Manfredo, M., Vaske, J., Teel, T. (2003). [The potential for conflict index: A
graphic approach to practical significance of human dimensions research](https://www.tandfonline.com/doi/abs/10.1080/10871200304310).
Human Dimensions of Wildlife, 8(3), 219-228.

Vaske, J. J., Beaman, J., Barreto, H., Shelby, L. B. (2010). [An extension and
further validation of the potential for conflict index](https://www.tandfonline.com/doi/abs/10.1080/01490401003712648).
Leisure Sciences, 32(3), 240-254.

## Citation

If you use the `pcir` package in your work, please cite it as follows:

```r
citation(package = 'pcir')
```

Example output:

```r
To cite the 'pcir' package in publications, use:

  Palmeira F, Wundervald B (2025). _pcir: Potential for
  Conflict Index in R_. R package version 0.0.0.9000,
  <https://github.com/fblpalmeira/pcir>.

The BibTeX entry for LaTeX users is

  @Manual{,
    title = {pcir: Potential for Conflict Index in R},
    author = {Francesca Belem Lopes Palmeira and Bruna Wundervald},
    year = {2025},
    note = {R package version 0.0.0.9000},
    url = {https://github.com/fblpalmeira/pcir},
  }
```

## License

This package is licensed under the [MIT License](https://github.com/fblpalmeira/pcir?tab=MIT-1-ov-file) file for more details.

## Bug Reports

If you encounter any bugs or issues, please report them on the [GitHub Issues](https://github.com/fblpalmeira/pcir/issues) page.

Note: To report a bug, you need a GitHub account, which you can join for free.

## Discussions

The [Discussions section](https://github.com/fblpalmeira/pcir/discussions) is the space for the `pcir` community  to ask questions,
share ideas, or get help without opening formal issues.

## Contact

For any questions or inquiries, please contact Francesca Palmeira at
francesca@alumni.usp.br.
"

#Write the README.Rmd file to the local repository
writeLines(readme_rmd_content, file.path(local_dir, "README.Rmd"))

#Add README.md to the Git stage
repo_url <- git2r::repository(local_dir)
git2r::add(repo_url, "README.Rmd")

#Commit the README.Rmd file
git2r::commit(repo_url, "Add README.Rmd")

#Push the commit to the remote repository
#git2r::push(repo_url)
rmarkdown::render("README.Rmd")

# Initialize the website structure (only needed once)
#pkgdown::init_site()

# Optional: Add the pkgdown configuration file (this step can be skipped if
# `pkgdown::init_site()` was used)
config_content <- "
# Template settings
template:
  bootstrap: 5
  bootswatch: flatly

navbar:
  structure:
    - left:
        - reference
        - articles
    - right:
        - icon: fa-github
          href: https://github.com/fblpalmeira/pcir

url: https://fblpalmeira.github.io/pcir/
"

# Write the configuration file
writeLines(config_content, file.path(local_dir, "_pkgdown.yml"))

# Build the site again to apply the config (if you added it)
pkgdown::build_site()
pkgbuild::check_build_tools(debug = TRUE)
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "D:R/rtools45/usr/bin", sep=";"))
#install.packages("devtools")
devtools::find_rtools()  # Check if Rtools is correctly detected
devtools::build()
# Initialize git repository if not already initialized (skip if already done)
repo <- git2r::repository()

# Stage all the changes, especially in the 'docs/' folder
git2r::add(repo, "*")

# Commit the changes (including the docs folder)
git2r::commit(repo, message = "Build and deploy pkgdown site to GitHub Pages")

# Push the changes to GitHub (you may need to authenticate if this is the first push)
# Define the remote origin URL (your GitHub repository)
#remote_add(repo, "origin", "https://github.com/fblpalmeira/pcir.git")

# Push to the GitHub repository (you may be prompted for credentials)
#push(repo, name = "origin", refspec = "refs/heads/main")

# Document, build, and install the package
devtools::document()
devtools::build()
devtools::install()
