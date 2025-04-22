library(ggplot2)

test_that("bubble() returns a ggplot object", {
  # Example simulated data for the test
  data <- data.frame(
    group = c("A", "B", "C"),
    pci = c(0.2, 0.5, 0.9),
    n = c(10, 20, 15)
  )

  # Run the function
  p <- bubble(data)

  # Check if the return value is a ggplot object
  expect_s3_class(p, "ggplot")
})
