
# Equivalent to Excel/Calc's PMT(interest_rate, number_payments, PV, FV, Type)
# function, which calculates the payments for a loan or the future value of an investment
# @param interestRate    - periodic interest rate represented as a decimal.
# @param nper - number of total payments / periods.
# @param pv   - present value -- borrowed or invested principal.
# @param fv   - future value of loan or annuity, default to 0 (which is what you want for loans)
# @param type - when payment is made: beginning of period is 1; end is 0. Default is 0
# @return double representing the periodic payment amount.
pmt <- function(interestRate, nper, pv, fv = 0, type = 0) {
  -interestRate * (pv * ((1 + interestRate)^ nper) + fv) / ((1 + interestRate * type) * (((1 + interestRate)^ nper) - 1))
}

# the monthly annuity amount i.e. the amortization and interest amount each payment period (month)
# @param loanAmount the total loan amount including capitalized fees (e.g. startup fee)
# @param interestRate the annual nominal interest
# @param tenureMonths the tenure of the loan in number of months
# @param amortizationFreeMonths the number of initial amortization free months, default to 0
# @param type - when payment is made: beginning of period is 1; end is 0. Default is 0
monthlyAnnuityAmount <- function(loanAmount, interestRate, tenureMonths, amortizationFreemonths = 0, type = 0) {
  montlyInterest <- interestRate / 12
  totalNumberOfPaymentPeriods <- tenureMonths - amortizationFreemonths
  pmt(montlyInterest, totalNumberOfPaymentPeriods, loanAmount * -1, type)
}

# return a vector of cachFlow entries for each period
# @param loanAmount the total loan amount including capitalized fees (e.g. startup fee)
# @param interestRate the annual nominal interest
# @param tenureMonths the tenure of the loan in number of months
# @param amortizationFreeMonths the number of initial amortization free months, default to 0
# @param invoiceFee a fee for each statement invoiced, default to 0
cashFlow <- function(loanAmount, interestRate, tenureMonths, amortizationFreeMonths, invoiceFee) {
  interestCostAmFreePeriod <- loanAmount * interestRate / 12
  monthlyAnnuity <- monthlyAnnuityAmount(loanAmount, interestRate, tenureMonths, amortizationFreeMonths)
  p <- loanAmount * -1.0
  for (month in 1:tenureMonths) {
    if (amortizationFreeMonths >= month) {
      costOfCredit <- interestCostAmFreePeriod
    } else {
      costOfCredit <- monthlyAnnuity
    }
    cashFlow <- costOfCredit + invoiceFee
    p <- c(p, cashFlow)
  }
  p
}

# @return a dataframe with the initial payment plan based on the input, based on monthly payment periods
# @param loanAmount the total loan amount including capitalized fees (e.g. startup fee)
# @param interestRate the annual nominal interest
# @param tenureMonths the total tenure of the loan in number of months
# @param amortizationFreeMonths the number of initial amortization free months, default to 0
# @param invoiceFee a fee for each statement invoiced, default to 0
paymentPlan <- function(loanAmount, interestRate, tenureMonths, amortizationFreeMonths = 0, invoiceFee = 0) {
  paymentPlans <- data.frame(
    month = numeric(),
    costOfCredit = numeric(),
    interestAmt = numeric(),
    amortization = numeric(),
    invoiceFee = numeric(),
    outgoingBalance = numeric(),
    cashFlow = numeric()
  )
  interestCostAmFreePeriod <- loanAmount * interestRate / 12
  monthlyAnnuity <- monthlyAnnuityAmount(loanAmount, interestRate, tenureMonths, amortizationFreeMonths )
  p <- list()
  p$month <- 0
  p$costOfCredit <- 0.0
  p$interestAmt <- 0.0
  p$amortization <- 0.0
  p$invoiceFee <- 0.0
  p$outgoingBalance <- loanAmount
  p$cashFlow <- loanAmount * -1.0

  paymentPlans <- rbind(paymentPlans, p)
  prevOB <- p$outgoingBalance
  for (month in 2:(tenureMonths + 1)) {
    p <- list()
    p$month <- month - 1
    if (amortizationFreeMonths >= month) {
      p$costOfCredit <- interestCostAmFreePeriod
    } else {
      p$costOfCredit <- monthlyAnnuity
    }
    p$interestAmt <- prevOB * interestRate / 12
    p$amortization <- p$costOfCredit - p$interestAmt
    p$invoiceFee <- invoiceFee
    p$outgoingBalance <- prevOB - p$amortization
    p$cashFlow <- p$costOfCredit + p$invoiceFee
    paymentPlans <- rbind(paymentPlans, p)
    prevOB <- p$outgoingBalance
  }
  paymentPlans
}

totalPaymentAmount <- function(loanAmount, interestRate, tenureMonths, amortizationFreeMonths = 0, invoiceFee = 0) {
  pp <- paymentPlan(loanAmount, interestRate, tenureMonths, amortizationFreeMonths, invoiceFee)
  sum(pp$costOfCredit) + sum(pp$invoiceFee)
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

