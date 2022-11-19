#' ---
#' title: "QUIZ 3"
#' author: "Amanjit Gill"
#' date: "November 14, 2022"
#' ---

library(CCA)
library(ggm)
library(CCP)

# QUESTION 3 

meat <- read.csv("price-cons.csv")[,2:5]

P <- meat[,1:2]
C <- meat[,3:4]

pc.cc <- cc(P, C)
pc.cc[c("cor", "xcoef", "ycoef")]

p.perm(P, C, type="Wilks")

# QUESTION 4 

n <- nrow(meat)
p <- ncol(P)
q <- ncol(C)

p.asym(pc.cc$cor, n, p, q, tstat="Wilks")
p.asym(pc.cc$cor, n, p, q, tstat="Hotelling")
p.asym(pc.cc$cor, n, p, q, tstat="Pillai")
p.asym(pc.cc$cor, n, p, q, tstat="Roy")

# QUESTION 6 

meat.cor <- cor(meat)

# cor34.12 using pcor

cor34.12 <- pcor(c(3,4,1,2), meat.cor)
cor34.12

# cor34.12 using formula, with values from pcor

cor34.2 <- pcor(c(3,4,2), meat.cor)
cor31.2 <- pcor(c(3,1,2), meat.cor)
cor41.2 <- pcor(c(4,1,2), meat.cor)

cor34.12 <- cor34.12 <- (cor34.2 - cor31.2*cor41.2) / 
    sqrt((1 - cor31.2**2)*(1 - cor41.2**2))

cor34.12

# hypothesis test

pcor.test(cor34.12, 2, 20)

# linear regression

meat.mlm <- lm(
    cbind(beef_consumption, pork_consumption) ~ steer_price+hog_price,
    data=meat)

summary(meat.mlm)

# QUESTION 7 

# see whether standardised data change the hypothesis test results
# re-use meat data for this

P.std <- scale(P)
C.std <- scale(C)

pc.cc.std <- cc(P.std, C.std)
pc.cc.std[c("cor", "xcoef", "ycoef")]

p.perm(P.std, C.std, type="Wilks")

p.asym(pc.cc.std$cor, n, p, q, tstat="Wilks")
p.asym(pc.cc.std$cor, n, p, q, tstat="Hotelling")
p.asym(pc.cc.std$cor, n, p, q, tstat="Pillai")
p.asym(pc.cc.std$cor, n, p, q, tstat="Roy")

# QUESTION 9 

S <- matrix(
    c(
        177, 40, -14, 4, 
        40, 98, -37, -4, 
        -14, -37, 314, 5, 
        4, -4, 5, 1
    ),
    4, 4)

# canonical correlations between (X1,X2) and (X3,x4)
# using Cholesky decomposition for S22_neg_sqrt

S11 <- S[1:2, 1:2]
S12 <- S[1:2, 3:4]
S21 <- S[3:4, 1:2]
S22 <- S[3:4, 3:4]

S22_neg_sqrt <- chol(solve(S22))

prod <- S22_neg_sqrt %*% S21 %*% solve(S11) %*% S12 %*% t(S22_neg_sqrt)

sqrt(eigen(prod)$values)

# hypothesis test for zero correlation b/w (X1,X2) and (X3,x4)

n <- 30
p <- 2
q <- 2

m <- n - 3/2 - (p+q)/2
s <- sqrt((p**2 * q**2 - 4)/(p**2 + q**2 - 5))

df1 <- p*q
df2 <- m*s - p*q/2 + 1

(big_lambda <- det(S) / (det(S11) * det(S22)))

# check big_lambda using Q1 and Q2

Q1 <- S22 - S21 %*% solve(S11) %*% S12
Q2 <- S21 %*% solve(S11) %*% S12
det(Q1 %*% solve(Q1+Q2))

# final figures for hypothesis test

(F_stat <- (1-sqrt(big_lambda))/sqrt(big_lambda) * df2/df1)

(p_value <- pf(F_stat,df1, df2, lower.tail = FALSE))


