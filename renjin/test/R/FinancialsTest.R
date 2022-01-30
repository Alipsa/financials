library('hamcrest')
library('se.alipsa:financials')

# To run in GNU.R:
# install.packages("devtools")
#devtools::install_github("bedatadriven/hamcrest")

delta <- 1e-9

test.irrAndApr <- function() {
  cf <- cashFlow(
    loanAmount=50429, 
    interestRate=0.149000, 
    tenureMonths=60, 
    amortizationFreeMonths=12, 
    invoiceFee=30
  )
  i <- irr(cf, 1e-9)
  a <- apr(i)
  assertThat(i, closeTo(0.013248756, delta))
  assertThat(a, closeTo(0.171097219, delta))
}

test.highPrecisionIrrAndApr <- function() {
  ####
  # delta precision needs to be one or two decimals less than the irr precision
  delta <- 1e-5
  cf <- cashFlow(400429, 0.147100, 120, 12, 30)
  i <- irr(cf, 1e-6)
  expectIrr <- 0.012356
  a <- apr(i)
  expectApr <-  0.158782
  assertThat(i, closeTo(expectIrr, delta))
  assertThat(a, closeTo(expectApr, delta))
}