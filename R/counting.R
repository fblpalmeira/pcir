
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
#' \itemize{
#'   \item Counts and percentages for each unique value.
#'   \item Weighted mean and standard deviation.
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

