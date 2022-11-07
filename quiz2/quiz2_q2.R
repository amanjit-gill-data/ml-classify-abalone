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

# ans_a

(r12.34 <- pcor(c(1,2,3,4), Sigma))

# ans_b

C <- matrix(
    c(
        98, -37, -4, 
        -37, 314, 5, 
        -4, 5, 1
    ),
    3, 3
)

sig0 <- Sigma[1,2:4]

sigYY <- Sigma[1,1]

(r1.234 <- sqrt(t(sig0) %*% solve(C) %*% sig0 / sigYY))

# ans_c_i_1

