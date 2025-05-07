
#' Calculate the Potential for Conflict Index (PCI)
#'
#' This function computes the Potential for Conflict Index (PCI) for each item
#' in a summarized data frame produced by the 'counting' function. The PCI is
#' a measure of consensus versus conflict in responses, based on the distribution
#' of responses across the two poles of a scale (e.g., -1 and 1).
#'
#' @param df2 A data frame produced by the 'counting' function, containing
#' columns like 'Count -1', 'Count 0', 'Count 1', and 'Total'.
#' @param negative_val The numeric value representing the negative pole of the scale.
#' Default is -1.
#' @param positive_val The numeric value representing the positive pole of the scale.
#' Default is 1.
#' @param scale_type Type of scale used: 'bipolar' (default) or 'unipolar'.
#' Currently included for future expansion; the function currently assumes a bipolar structure.
#'
#' @return A data frame including the original columns plus the computed 'PCI' value
#' for each row/item. Numeric columns are rounded to two decimal places.
#'
#' @details
#' The PCI is calculated using the formula:
#' PCI = [1 - (na / xt - nu / xt)] * xt / z
#' where 'na' and 'nu' are the counts of agreement and disagreement respectively,
#' 'xt' is the total of positive and negative responses, and 'z' is the total number
#' of responses.
#'
#' The resulting PCI ranges from 0 (no conflict) to 1 (maximum conflict).
#'
#' @examples
#' df1 <- data.frame(
#'   A = c(-1, 1, 1, 0, -1),
#'   B = c(-1, 1, 0, -1, 1),
#'   C = c(1, 1, -1, 0, -1),
#'   D = c(0, 1, 1, -1, -1),
#'   E = c(1, 1, 0, -1, -1)
#' )
#' df2 <- counting(df1, cols = c('A', 'B', 'C', 'D', 'E'))
#' result <- pci(df2)
#' print(result)
#'
#' @importFrom dplyr mutate across
#' @export
pci <- function(df2, negative_val = -1, positive_val = 1, scale_type = c('bipolar', 'unipolar')) {
  scale_type <- match.arg(scale_type)

  neg_col <- paste0('Count ', negative_val)
  pos_col <- paste0('Count ', positive_val)

  if (!(neg_col %in% names(df2)) || !(pos_col %in% names(df2))) {
    stop('One or both specified Count columns not found in the data.')
  }

  df2 %>%
    mutate(
      nu = .data[[neg_col]],
      na = .data[[pos_col]],
      xt = nu + na,
      z = Total,
      PCI = (1 - (na / xt - nu / xt)) * xt / z
    ) %>%
    mutate(across(where(is.numeric), function(x) round(x, 2)))
}

