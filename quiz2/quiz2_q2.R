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

# PART C

# perform eigendecomposition
eig <- eigen(Sigma)

# extract variances and PC vectors
p <- nrow(Sigma)
sum_vars <- sum(eig$values)

vars <- c()
PCs <- list()
var_conts <- c()

for (i in 1:p) {
    vars <- c(vars, eig$values[i])
    PCs[[i]] <- eig$vectors[,i]
    var_conts <- c(var_conts, vars[i]/sum_vars*100)
}

PCs

# choose k using 90% variance explained
var_conts.c <- cumsum(var_conts)
(min(which(var_conts.c >= 90)))

# choose k using Kaiser's rule
# since Sigma isn't standardised (yet), use an alternative definition
(max(which(vars > mean(vars))))

# REDO PCA WITH STANDARDISED SIGMA

# standardised Sigma is same as correlation matrix
Sigma.std <- cov2cor(Sigma)

# now redo PCA
eig.std <- eigen(Sigma.std)

# compute variance contribution for each variable
sum_vars.std <- sum(eig.std$values)

vars.std <- c()
var_conts.std <- c()

for (i in 1:p) {
    vars.std <- c(vars.std, eig.std$values[i])
    var_conts.std <- c(var_conts.std, vars.std[i]/sum_vars.std)
}

var_conts.std

