#' ---
#' title: "QUIZ 4"
#' author: "Amanjit Gill"
#' date: "November 21, 2022"
#' ---
#' 

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
(R <- cov2cor(S))

(test_stat <- -n*log(det(R)))

(df <- p*(p-1)/2)

(p_value <- pchisq(test_stat, df, lower.tail = FALSE))


# QUESTION 4

X <- read.csv("../Assignments/quiz4/q3_data.csv")

