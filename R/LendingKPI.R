

# Pull Through Rate
# Formula: (# of Funded Loans) / (# of Applications Submitted in Same Period)
#  measures pipeline efficiency by dividing total funded loans by the number of applications during a defined period.
#
# Purpose: Pull-through rate provides a high-level perspective on the overall health your mortgage operation.
# This metric provides important insights into workflow efficiency,
# the quality of applications submitted, level of customer service,
# interest rate competitiveness and the suitability of a potential customer’s profile.

# Average Cycle Time
# Formula: (Sum of Days from Application to Funding for All Loans) / (# of Loans Funded in Same Period)
#
# Purpose: Average cycle time is a basic, yet important, performance metric to track for its ability to benchmark overall efficiency.
# As you begin to pull levers within your processes and integrate improvements, you would expect to see average cycle time decrease on new loans.
#
# A poor cycle time has been shown to correlate directly to pull-through rates and loan profitability metrics.
# Referral partners and borrowers have expectations that can quickly sour relationships when loans do not close on time, or efficiently.
# Also known as
# Decision to Close Time
# The decision to close time cycle provides information about the number of days required to close and fund a loan
# after the offer (underwriting decision) has been made.
# This KPI provides insights about how efficiently a lending team is coordinating origination efforts with loan officers.

# Abandoned Loan Rate
# Abandoned loan rate measures the percentage of loan applications that are abandoned by a borrower
# after they have been approved by the lender.

# Average Origination Value
# The average origination value measures the total revenue earned for each loan over a given time.
# This KPI combines origination and underwriting fees, as well as any other fees that are added to revenue.


# Application Approval Amount Rate
# This metric is calculated by dividing the amount of approved applications by the amount of submitted applications.
applicationApprovalAmountRate <- function(submittedApplications, approvedApplications) {
  sum(approvedApplications) / sum(submittedApplications)
}

# Application Approval Number Rate
# This metric is calculated by dividing the number of approved applications by the number of submitted applications.

# Net Charge-Off Rate
#  the difference between gross charge-offs and any subsequent recoveries of delinquent debt.
#  This KPI effectively represents that amount of debt a lender believes it will never collect compared to average receivables.

# Customer Acquisition Cost
# the ratio of a borrower’s lifetime value to a borrower’s acquisition cost.
# These costs include but aren’t limited to research, marketing and advertising.

# Average Number of Conditions Per Loan