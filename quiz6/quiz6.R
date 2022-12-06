#' ---
#' title: "QUIZ 6"
#' author: "Amanjit Gill"
#' date: "December 6, 2022"
#' ---

library(mclust)
library(dplyr)
library(GGally)
library(copula)

# QUESTION 6 - PART A

X <- read.csv("Assignments/quiz6/X.csv")

ans_a_fit <- Mclust(X)

ans_a_K <- 6

plot(ans_a_fit, what=c("BIC", "classification"))

# QUESTION 6 - PART B

ans_b_fit <- Mclust(X, G=2)

ans_b_o <- TRUE

plot(ans_b_fit, what=c("BIC", "classification"))

# QUESTION 7 - PART A

X <- matrix(c(1.35, -0.53, 0.73, 7.72, 1.86, 0.19, 0.54, 2.07, 0.92, 1.76, 
              1.18, -0.65, 1.9, 0.17, 1.58, 0.23, 0.5, 0.96, 1.97, 1.79, 
              2.06, 1.17, 0.93, -3.41, 0.96, -1.02, 0.84, 1, 0.63, -1.42, 
              1.96, -2.15, 1.23, 5.11, 0.15, 2.18, 1.2, -1.8, 0.56, 0.04), 
            20, 2, byrow=TRUE)

XU <- pobs(X)

e_fit <- fitCopula(normalCopula(dim=2), XU, method="irho")

ans_a_r <- -0.1965

# QUESTION 7 - PART B

normGNc <- mvdc(normalCopula(dim=2), margins=c("gamma","norm"),
                paramMargins = list(list(shape=1, rate=1),
                                    list(mean=0, sd=1)))

gn_fit <- fitMvdc(as.matrix(X), normGNc, start = c(1,1, 0,1, 0))

summary(gn_fit)

ans_b_r <- -0.2033
    
ans_b_mean <- 0.6731
ans_b_sd <- 2.4363
    
ans_b_shape <- 3.358
ans_b_rate <- 2.914
    
