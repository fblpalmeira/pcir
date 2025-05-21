
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

