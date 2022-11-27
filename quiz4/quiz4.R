#' ---
#' title: "QUIZ 4"
#' author: "Amanjit Gill"
#' date: "November 21, 2022"
#' ---
#' 

library(psych)

# QUESTION 3

S <- matrix(
    c(
        177, 40, -14, 4, 
        40, 98, -37, -4, 
        -14, -37, 314, 5, 
        4, -4, 5, 1), 
    4, 4)

n <- 30
p <- ncol(S)
R <- cov2cor(S)

(test_stat <- -n*log(det(R)))

(df <- p*(p-1)/2)

(p_value <- pchisq(test_stat, df, lower.tail = FALSE))


# QUESTION 4 - PART A

X <- read.csv("Assignments/quiz4/q4_data.csv")

# try 1 factor
# result: 1 factor is NOT sufficient
f1 <- factanal(X, 1)
f1$STATISTIC
f1$dof
f1$PVAL

# try 2 factors
# result: 2 factors are sufficient
f2 <- factanal(X, 2)
f2$STATISTIC
f2$dof
f2$PVAL

# try 3 factors
# result: 3 factors are sufficient
f3 <- factanal(X, 3)
f3$STATISTIC
f3$dof
f3$PVAL

# overall result: 2 factors are sufficient

# QUESTION 4 - PART Bi

f2.vmax <- fa(X, 2, fm="ml", rotate="varimax")
f2.vmax$loadings

# result:
# factor 1 = X3, X7, X8
# factor 2 = X4, X5, X6

# QUESTION 4 - PART Bii

Lambda <- f2.vmax$loadings
Sigma_e <- f2.vmax$uniquenesses

R <- Lambda %*% t(Lambda) + Sigma_e
dump(list=c("R"), file="")



