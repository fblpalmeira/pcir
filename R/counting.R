
#' Create a Count Table with Percentages, Mean, and SD
#'
#' This function takes a data frame and computes the count and percentage of
#' each value across specified columns. It also calculates the weighted mean and
#' standard deviation for each variable. The output is a summary data frame
#' useful for analyzing distributions of ordinal or Likert-type scales.
#'
#' @param df1 A data frame containing the variables to summarize.
#' @param cols A character vector with the names of the columns to include in the analysis.
#' @return A data frame in wide format containing:
#' \itemize{
#'   \item Counts of each value per variable
#'   \item Percentages of each value per variable
#'   \item Weighted mean
#'   \item Weighted standard deviation
#' }
#' @examples
#' df1 <- data.frame(A = c(-1, -1, -1, 0, -1), B = c(-1, 1, 0, -1, 1)),
#'                   C = c(1, 1, 1, 0, -1), D = c(0, -1, 1, 1, 1), E = c(1, 1, 0, -1, -1))
#' counting(df1, cols = c('A', 'B', 'C', 'D', 'E'))
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

