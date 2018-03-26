context("Create BMI categories")

test_that("Created BMI categories are correct", {
  expect_equal(nlevels(BMIgroup(25)), 4)
  expect_equal(as.character(BMIgroup(18.4)), "Underweight (below 18.5)")
})
