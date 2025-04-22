
#' Create a Count Table with Percentages, Mean, and SD
#'
#' This function takes a data frame, transforms it by computing counts, percentages,
#' mean, and standard deviation for specified columns. It helps in summarizing
#' the data to understand the distribution and variation.
#'
#' @param df1 A data frame containing the data to be processed. The data frame
#' should have at least 5 columns to select from.
#' @return A data frame with computed statistics, including counts, percentages,
#' mean, and standard deviation.
#' @examples
#' df1 <- data.frame(A = c(-1, 2, 2, 3, -1), B = c(-1, 2, 3, -1, 2),
#' C = c(1, 2, -2, 3, -1), D = c(3, 2, 1, -1, -2), E = c(2, 3, 1, -1, -3))
#' result <- counting(df1)
#' print(result)
#' @export
#' @importFrom stats weighted.mean
counting <- function(df1) {
  df1 [,-1] %>%
    #select(2:6) %>%
    pivot_longer(everything()) %>%
    group_by(name, value) %>%
    dplyr::summarise(Count = n()) %>%
    group_by(name) %>%
    mutate(`%` = 100 * (Count / sum(Count)),
           Mean = weighted.mean(value, Count),
           SD = sqrt(Hmisc::wtd.var(value, Count)),
           Total = sum(Count)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'value',
                names_sep = ' ',
                values_from = c('Count', `%`),
                names_vary = 'slowest')
}

