test_that("counting() computes correct structure and stats", {
  df <- data.frame(ID = 1:5,
                   A = c(-1, 2, 2, 3, -1),
                   B = c(-1, 2, 3, -1, 2),
                   C = c(1, 2, -2, 3, -1),
                   D = c(3, 2, 1, -1, -2),
                   E = c(2, 3, 1, -1, -3))

  result <- counting(df)

  # Check if essential columns are present
  expect_true(all(c("name", "Mean", "SD", "Total") %in% colnames(result)))

  # Check if there are rows for all variables
  expect_true(all(c("A", "B", "C", "D", "E") %in% result$name))
})

