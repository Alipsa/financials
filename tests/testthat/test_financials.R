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
  expect_equal(ppdf$cashFlow[1], loanAmt * -1)

  verifyPayment <- function(month, costOfCredit, interestAmt, amortization, invoiceFee, outgoingBalance, cashFlow, p) {
    delta <- 0.01
    expect_equal(p$month, month, info="month")
    expect_equal(p$costOfCredit, costOfCredit, tolerance=delta, info="costOfCredit")
    expect_equal(p$interestAmt, interestAmt, tolerance=delta, info="interestAmt")
    expect_equal(p$amortization, amortization, tolerance=delta, info="amortization")
    expect_equal(p$invoiceFee, invoiceFee, info="invoiceFee")
    expect_equal(p$outgoingBalance, outgoingBalance, tolerance=delta, info="outgoingBalance")
    expect_equal(p$cashFlow, cashFlow, tolerance=delta, info="cashFlow")
  }

  verifyPayment(1, 854.21, 284.50, 569.70, invoiceFee, 49859.30, 884.21, ppdf[2,])
  verifyPayment(5, 854.21, 271.54, 582.67, invoiceFee, 47548.17, 884.21, ppdf[6,])
  verifyPayment(15, 854.21, 237.82, 616.39, invoiceFee, 41537.60, 884.21, ppdf[16,])
  verifyPayment(35, 854.21, 164.41, 689.79, invoiceFee, 28452.83, 884.21, ppdf[36,])
  verifyPayment(70, 854.21, 14.30, 839.91, invoiceFee, 1694.06, 884.21, ppdf[71,])
  verifyPayment(72, 854.21, 4.79, 849.41, invoiceFee,0, 884.21, ppdf[73,])

  internalReturn <- irr(ppdf$cashFlow);
  expect_equal(0.006667407, internalReturn, tolerance=0.0000001);

  ammualPercentage <- apr(internalReturn);
  expect_equal(0.0830, ammualPercentage, tolerance=0.0001);
})

test_that("Effective interest, total payment, and montly payment works", {
  loanAmt <- 263429
  tenureMonths <- 15 * 12
  amortizationFreeMonths <- 12
  interest <- 0.055
  invoiceFee <- 30

  expEffectiveInterest <- 0.0585669
  expMonthlyAnnuity <- 2251.86
  expTotalPmt <- 398200.88

  cashFlow <- cashFlow(loanAmt, interest, tenureMonths, amortizationFreeMonths, invoiceFee)
  internalReturn <- irr(cashFlow)
  effectiveInterestRate <- apr(internalReturn)
  expect_equal(effectiveInterestRate, expEffectiveInterest, tolerance=1E-7)

  monthlyPayment <- monthlyAnnuityAmount(loanAmt, interest, tenureMonths, amortizationFreeMonths)
  expect_equal(monthlyPayment, expMonthlyAnnuity, tolerance=0.01)

  totalPaymentAmt <- totalPaymentAmount(loanAmt, interest, tenureMonths, amortizationFreeMonths, invoiceFee)
  expect_equal(totalPaymentAmt, expTotalPmt,  tolerance=0.01)

  interestCostAmfreePeriod <- loanAmt * interest / 12
  altTotal <- (monthlyPayment + invoiceFee) * tenureMonths - (monthlyPayment - interestCostAmfreePeriod) * amortizationFreeMonths
  expect_equal(totalPaymentAmt, altTotal, tolerance=0.01)
})
