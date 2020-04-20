context("Lending KPI's test")

test_that("Application Approval Amount Rate", {
  submitted <- c(100219, 38219, 120219, 85219, 105219, 75219)
  approved <- c(120219, 85219, 105219)
  sum(approved) / sum(submitted)
  expect_equal(applicationApprovalAmountRate(submitted, approved), 0.5925018, tolerance=1E-7)
})