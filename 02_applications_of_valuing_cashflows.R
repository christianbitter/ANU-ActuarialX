rm(list = ls());
library(dplyr);
library(ggplot2);


# the equation of value is 
# present value of incomings = present value of outgoings
# 
# lets assume we receive some premiums at time 0, 1
# but have to make some claims at time 2 and 3
cumulate <- function(v, i, t) v*(1 + i)^t
discount <- function(v, i, t) v*(1 + i)^-t

A_ti <- function(A_t1, claim_t, premium_t, irate) {
  return(A_t1*(1+irate) + premium_t - claim_t);
}

A_t <- function(v, irate, t) {
  return(v * (1+irate) * ((1 - irate-t) / irate));
}

X      <- 1000; # this risk premium
Y      <- 1000;

income <- c(X, X, 0, 0);
outgoings <- c(0, 0, Y, Y);

irate <- .035;
v <- (1 + irate);

# present value of income is the income at time 0 + 
# income at time 1 discounted back 
pvi <- sum(
  sapply(X = 1:length(income),
         FUN = function(i)discount(income[i], irate, i - 1))
);

pvo <- sum(
  sapply(X = 1:length(outgoings),
         FUN = function(i)discount(outgoings[i], irate, i - 1))
);
  
# the 

cat("Interest Rate:", 100 * irate, "%\r\n")
cat("Assuming income of X =", X, " present value of income =", pvi, "\r\n")
cat("Assuming claim of Y =", Y, " present value of income =", pvo, "\r\n")
cat("pv_i = pv_o\r\n")


## Video 2.2 - Linking Equations of Value to Valuing Cash Flows
ir <- .035;
A_0 <- 933.51;
A_1 <- A_ti(A_t1 = A_0, claim_t = 0, premium_t = 933.51, irate = ir);
A_2 <- A_ti(A_t1 = A_1, claim_t = 1000, premium_t = 0, irate = ir);
A_3 <- A_ti(A_t1 = A_2, claim_t = 1000, premium_t = 0, irate = ir);

cat("A0 = ", A_0, "=> A_1 =", A_1, "\r\n");
cat("A1 = ", A_1, "=> A_2 =", A_2, "\r\n");
cat("A2 = ", A_2, "=> A_3 =", A_3, "\r\n");


irate <- 0.02;
v     <- 1 + 1.02^-1 + 1.02^-2 + 1.02^-3 + 1.02^-4 + 1.02^-5 + 1.02^-6 + 1.02^-7 + 1.02^-8 + 1.02^-9;
v15   <- 1.02^-15
