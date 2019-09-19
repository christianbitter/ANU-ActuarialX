rm(list = ls());

library(dplyr);
library(ggplot2);

# Practice Question 1.1
# An individual invests $10,000 at an interest rate of 2% per annum. 
# Calculate the accumulated value after 10 years (to 2 decimal places).

B <- 1e4;
i <- .02; # per year
years <- 10;

accumulated <- B * (1 + i)^years;

cat("accumulated value after 10 years (to 2 decimal places).: ", round(accumulated, 2), "\r\n");

# Assessment Question 1.1 
# A life insurer receives $500,000 in premiums and invests them at an interest rate of 3.5% per annum. 
# Calculate the accumulated value after 2 years (to 2 decimal places).

P <- 5e5;
i <- .035;
y <- 2;

A <- P * (1 + i)^y;
cat("Accumulated value after", y, "years: ", round(A, 2), "\r\n");

# Practice Question 1.2
# Calculate the present value, at an interest rate of 6% per annum, 
# of a cash flow of $20,000 due in 25 years (to 2 decimal places).

i <- .06;
A <- 2e4;
y <- 25;

v <- (1+i)^(-y);
P <- A * v;

cat("Present value of 20,000$ at 6%/a over 25 years is: ", round(P, 2), "\r\n");


# Assessment Question 1.2
# An individual is required to make a payment of $1,500 in 3 years from today. 
# Calculate the amount that must be set aside today in order to have sufficient 
# money to make the repayment (to 2 decimal places). Use an interest rate of 5.5% per annum.

y <- 3;
i <- 0.055;
A <- 1.5e3;

v <- (1+i)^(-y);
P <- A * v;
cat("individual is required to set aside: ", round(P, 2), "$\r\n");



# Practice Question 1.3
# Recalculate the Actual Reserves of the insurer in the example used in this Lesson at Year 5 
# (to the nearest whole number) using an interest rate of 6% per annum. 
# Recall that the cash flows affecting the insurer are: Year Premium Received Claims Paid

i <- 0.06
y <- 5;

df <- data.frame(year = seq(0, y, 1),
                 premium = c(535824, 0, 0, 0, 0, 0),
                 claims  = c(0, 35498, 72512, 59334, 104177, 89265 ));

df$i <- i;
df$reserve <- df$premium * (1 + i);
for (l in 2:nrow(df)) {
  j <- l - 1;
  r <- df$reserve[j];
  p <- df$premium[l];
  c <- df$claims[l];
  rc <- (r + p - c) * (1.0 + i);
  # cat("reserve: ", r, " Premium: ", p, " Claim: ", c, " Compounded Reserve: ", rc, "\r\n")
  df$reserve[l] <- rc;
}



A <- 535824*(i+1)^y - 
     35498*(i+1)^(y-1) - 
     72512*(i+1)^(y-2) - 
     59334*(i+1)^(y-3) - 
     104177*(i+1)^(y-4) - 
     89265*(i+1)^(y-5)
cat("Actual reserve after year", y, "is: ", round(A), "\r\n");
# Assessment Question 1.3
# An individual invests an amount of $5,000 in a bank account today, 
# knowing that they will need to withdraw 
# $1,000 in 1 year, 
# $500 in 2 years and 
# $2,000 in 3 years. 
# If the interest rate on the amount invested is 2.5% per annum, 
# calculate the amount of money in the bank account immediately after the $2,000
# withdrawal in 3 years from today (to 2 decimal places).
i <- .025;
y <- 3;
A <- 
  5000*(1+i)^y - 
  1000*(1+i)^(y-1) - 
  500*(1+i)^(y-2) - 
  2000*(1+i)^(y-3);
cat("Amount of money in the bank account: ", round(A, 2), "\r\n");


# Practice Question 1.4
# Calculate the present value at Year 0 of the CLAIMS paid by the insurer 
# in the example used in this Lesson (to the nearest whole number),
# using the same interest rate of 4% per annum. Recall that the cash flows affecting the insurer are:

y <- 5;
i <- .04;
premium <- c(535824, 0, 0, 0, 0, 0);
claims  <- c(0, 35498, 72512, 59334, 104177, 89265);


P <- 
  claims[y + 1]*(1+i)^(-y) + 
  claims[y + 1 - 1]*(1+i)^(-(y-1)) + 
  claims[y + 1 - 2]*(1+i)^(-(y-2)) + 
  claims[y + 1 - 3]*(1+i)^(-(y-3)) +
  claims[y + 1 - 4]*(1+i)^(-(y-4));

cat("Calculate the present value at Year 0 of the CLAIMS: ", round(P), "\r\n");


# Assessment Question 1.4 
# Calculate the total present value today (to 2 decimal places) of the following cash flows:
# $300 due in 2 years from today
# $1,000 due in 5 years from today
# $200 due in 15 years from today
# Use an interest rate of 12% per annum.

i <- .12;
y <- 15;

P <- 
  200 * (1+i)^(-15) + 
  1000 * (1+i)^(-5) + 
  300 * (1 + i)^(-2)
  
cat("Total present value today: ", round(P, 2), "\r\n");
