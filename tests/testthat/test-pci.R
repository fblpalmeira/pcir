test_that("pci() computes PCI correctly and returns expected structure", {
  input_data <- tibble::tibble(
    group = c("A", "B", "C"),
    `Count -1` = c(10, 20, 15),
    `Count 1` = c(5, 10, 7),
    Total = c(100, 150, 120)
  )

  expected_result <- tibble::tibble(
    group = c("A", "B", "C"),
    `Count -1` = c(10, 20, 15),
    `Count 1` = c(5, 10, 7),
    Total = c(100, 150, 120),
    nu = c(10, 20, 15),
    na = c(5, 10, 7),
    xt = c(15, 30, 22),
    z = c(100, 150, 120),
    PCI = c(0.20, 0.27, 0.25)
  )

  result <- pci(input_data)

  # Sort columns to ensure same order
  result <- result[, names(expected_result)]

  expect_equal(result, expected_result, ignore_attr = TRUE)
})

