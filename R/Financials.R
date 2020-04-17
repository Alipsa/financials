
# Equivalent to Excel/Calc's PMT(interest_rate, number_payments, PV, FV, Type)
# function, which calculates the payments for a loan or the future value of an investment
# @param r    - periodic interest rate represented as a decimal.
# @param nper - number of total payments / periods.
# @param pv   - present value -- borrowed or invested principal.
# @param fv   - future value of loan or annuity.
# @param type - when payment is made: beginning of period is 1; end, 0.
# @return <code>double</code> representing periodic payment amount.
pmt <- function(r, nper, pv, fv = 0, type = 0) {
  -r * (pv * ((1 + r)^ nper) + fv) / ((1 + r * type) * (((1 + r)^ nper) - 1))
}


monthlyAnnuityAmount <- function(loanAmount, interestRate, tenureMonths, amortizationFreemonths) {
  montlyInterest <- interestRate / 12
  totalNumberOfPaymentPeriods <- tenureMonths - amortizationFreemonths
  pmt(montlyInterest, totalNumberOfPaymentPeriods, loanAmount * -1)
}

# return a vector of cachFlow entries for each period
cashFlow <- function(loanAmount, tenureMonths, amFreeMonths, interest, invoiceFee) {
  interestCostAmFreePeriod <- loanAmount * interest / 12
  monthlyAnnuity <- monthlyAnnuityAmount(loanAmount, interest, tenureMonths / 12, amFreeMonths)
  p <- loanAmount * -1.0
  for (month in 1:tenureMonths) {
    if (amFreeMonths >= month) {
      costOfCredit <- interestCostAmFreePeriod
    } else {
      costOfCredit <- monthlyAnnuity
    }
    cacheFlow <- costOfCredit + invoiceFee
    p <- c(p, cacheFlow)
  }
  p
}

paymentPlan <- function(loanAmount, tenureMonths, amFreeMonths, interest, invoiceFee) {
  paymentPlans <- data.frame(
    month = numeric(),
    costOfCredit = numeric(),
    interestAmt = numeric(),
    amortization = numeric(),
    invoiceFee = numeric(),
    outgoingBalance = numeric(),
    cacheFlow = numeric()
  )
  interestCostAmFreePeriod <- loanAmount * interest / 12
  monthlyAnnuity <- monthlyAnnuityAmount(loanAmount, interest, tenureMonths, amFreeMonths)
  p <- list()
  p$month <- 0
  p$costOfCredit <- 0.0
  p$interestAmt <- 0.0
  p$amortization <- 0.0
  p$invoiceFee <- 0.0
  p$outgoingBalance <- loanAmount
  p$cacheFlow <- loanAmount * -1.0

  paymentPlans <- rbind(paymentPlans, p)
  prevOB <- p[["outgoingBalance"]]
  for (month in 2:(tenureMonths + 1)) {
    p <- list()
    p$month <- month - 1
    if (amFreeMonths >= month) {
      p$costOfCredit <- interestCostAmFreePeriod
    } else {
      p$costOfCredit <- monthlyAnnuity
    }
    p$interestAmt <- prevOB * interest / 12
    p$amortization <- p$costOfCredit - p$interestAmt
    p$invoiceFee <- invoiceFee
    p$outgoingBalance <- prevOB - p$amortization
    p$cacheFlow <- p$costOfCredit + p$invoiceFee
    if (month == 1) {
      str(p)
    }
    paymentPlans <- rbind(paymentPlans, p)
    prevOB <- p[["outgoingBalance"]]
  }
  paymentPlans
}
# @param monthlyIrr the MONTHLY internal rate of return (monthly irr)
# @return the annual percentage rate (effektiv ränta)
apr <- function(monthlyIrr) {
  ((1 + monthlyIrr)^12) - 1
}

# Net present value (NPV) is the difference between the present value 
# of cash inflows and the present value of cash outflows over a period of time
npv <- function(i, cf, t=seq(along=cf)) sum(cf/(1+i)^t)

# Computes the internal rate of return
# @param cf a vector of the cash flow
irr <- function(cf, precision = 1e-6) {
  uniroot(npv, c(0,1), cf=cf, tol = precision)$root
}

