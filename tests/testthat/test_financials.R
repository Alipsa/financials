context("Basic financial tests")

test_that("Payment plan calculation works", {
  loanAmt <- 50429
  tenureMonths <- 6 * 12
  amortizationFreeMonths <- 0
  interest <- 0.0677
  invoiceFee <- 30

  ppdf <- paymentPlan(loanAmt, interest, tenureMonths, amortizationFreeMonths, invoiceFee)
  expect_equal(nrow(ppdf), tenureMonths + 1)

  expect_equal(ppdf$outgoingBalance[1], loanAmt)
  expect_equal(ppdf$cacheFlow[1], loanAmt * -1)

  verifyPayment <- function(month, costOfCredit, interestAmt, amortization, invoiceFee, outgoingBalance, cacheFlow, p) {
    delta <- 0.01
    expect_equal(p$month, month, info="month")
    expect_equal(p$costOfCredit, costOfCredit, tolerance=delta, info="costOfCredit")
    expect_equal(p$interestAmt, interestAmt, tolerance=delta, info="interestAmt")
    expect_equal(p$amortization, amortization, tolerance=delta, info="amortization")
    expect_equal(p$invoiceFee, invoiceFee, info="invoiceFee")
    expect_equal(p$outgoingBalance, outgoingBalance, tolerance=delta, info="outgoingBalance")
    expect_equal(p$cacheFlow, cacheFlow, tolerance=delta, info="cacheFlow")
  }

  verifyPayment(1, 854.21, 284.50, 569.70, invoiceFee, 49859.30, 884.21, ppdf[2,])
  verifyPayment(5, 854.21, 271.54, 582.67, invoiceFee, 47548.17, 884.21, ppdf[6,])
  verifyPayment(15, 854.21, 237.82, 616.39, invoiceFee, 41537.60, 884.21, ppdf[16,])
  verifyPayment(35, 854.21, 164.41, 689.79, invoiceFee, 28452.83, 884.21, ppdf[36,])
  verifyPayment(70, 854.21, 14.30, 839.91, invoiceFee, 1694.06, 884.21, ppdf[71,])
  verifyPayment(72, 854.21, 4.79, 849.41, invoiceFee,0, 884.21, ppdf[73,])
})
