#' ---
#' title: "QUIZ 2 - QUESTION 2"
#' author: "Amanjit Gill"
#' date: "November 8, 2022"
#' ---

library(ggm)

Sigma <- matrix(
    c(
        177, 40, -14, 4, 
        40, 98, -37, -4, 
        -14, -37, 314, 5, 
        4, -4, 5, 1
    ),
    4, 4
)

# PART A

(r12.34 <- pcor(c(1,2,3,4), Sigma))

# PART A manually

Sig11 <- Sigma[1:2, 1:2]
Sig12 <- Sigma[1:2, 3:4]
Sig21 <- Sigma[3:4, 1:2]
Sig22 <- Sigma[3:4, 3:4]

Sig12.34 <- Sig11 - Sig12 %*% solve(Sig22) %*% Sig21

(r12.34 <- Sig12.34[1,2] / sqrt(Sig12.34[1,1] * Sig12.34[2,2]))

# PART B

C <- Sigma[2:4, 2:4]

sig0 <- Sigma[1,2:4]

sigYY <- Sigma[1,1]

(r1.234 <- sqrt(t(sig0) %*% solve(C) %*% sig0 / sigYY))

# PART Ci

# first get eigenvalues and eigenvectors

(eig <- eigen(Sigma))

# 1st and 2nd PC coefficient vectors

(pc1 <- eig$vectors[,1])
(pc2 <- eig$vectors[,2])

# PART Cii

# first obtain all the variances
sum_vars <- sum(eig$values)
var1 <- eig$values[1]
var2 <- eig$values[2]
var3 <- eig$values[3]
var4 <- eig$values[4]

# now compute contributions to total variance
var_cont1 <- var1/sum_vars * 100
var_cont2 <- var2/sum_vars * 100
var_cont3 <- var3/sum_vars * 100
var_cont4 <- var4/sum_vars * 100

(var_cont12 <- var_cont1 + var_cont2)
(var_cont123 <- var_cont12 + var_cont3)
(var_cont1234 <- var_cont123 + var_cont4)

# result: need 3 PCs to explain at least 90% of variability

# PART Ciii

# Kaiser's rule is equivalent to checking each variance against the average

(avg_var <- sum_vars/4)

# result: only 2 PCs have var > average var

# PART Civ

# correlation matrix is same as standardised covariance matrix
(Sigma_std <- cov2cor(Sigma))

# now redo PCA
(eig_std <- eigen(Sigma_std))

# compute contributions to total variance
sum_vars_std <- sum(eig_std$values)
var1_std <- eig_std$values[1]
var2_std <- eig_std$values[2]
var3_std <- eig_std$values[3]
var4_std <- eig_std$values[4]
var_cont1_std <- var1_std/sum_vars_std * 100
var_cont2_std <- var2_std/sum_vars_std * 100
var_cont3_std <- var3_std/sum_vars_std * 100
var_cont4_std <- var4_std/sum_vars_std * 100

(var_cont_std <- c(var_cont1_std, var_cont2_std, var_cont3_std, var_cont4_std))
