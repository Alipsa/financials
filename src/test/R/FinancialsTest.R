library('hamcrest')
library('se.alipsa:financials')

delta <- 1e-9

test.irrAndApr <- function() {
  
  cf <- cashFlow(50429, 60, 12, 0.149000, 30)
  i <- irr(cf, 1e-9)
  a <- apr(i)
  assertThat(i, closeTo(0.013248756, delta))
  assertThat(a, closeTo(0.171097219, delta))
  
  ####
  # delta precision needs to be one or two decimals less than the irr precision
  delta <- 1e-5
  cf <- cashFlow(400429, 120, 12, 0.147100, 30)
  i <- irr(cf, 1e-6)
  expectIrr <- 0.012356
  a <- apr(i)
  expectApr <-  0.158782
  assertThat(i, closeTo(expectIrr, delta))
  assertThat(a, closeTo(expectApr, delta))
}
