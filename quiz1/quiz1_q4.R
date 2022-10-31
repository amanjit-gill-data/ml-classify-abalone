#' ---
#' title: "QUIZ 1 - QUESTION 4"
#' author: "Amanjit Gill"
#' date: "October 31, 2022"
#' ---

library(GGally)
library(dplyr)
library(MVN)

dev.new(width = 2.5, height = 2.5, unit = "in")

b <- as_tibble(read.csv("bridges.csv"))
head(b)

# PART A - pairwise plots and normality test

b.subset <- b %>% select(erected, length)
b.subset

ggpairs(b.subset)

mvn(b.subset, mvnTest = "mardia", univariateTest = "SW")

# PART B - Hypothesis Test

alpha <- 0.05

# separate into separate sets by river (M or A)
# river 1 = M, river 2 = A
x1 <- b[b$river == 'M', ] %>% select(erected, length)
x2 <- b[b$river == 'A', ] %>% select(erected, length)

p <- ncol(x1)

(n1 <- nrow(x1))
(n2 <- nrow(x2))

xbar1 <- colMeans(x1)
S1 <- cov(x1)

xbar2 <- colMeans(x2)
S2 <- cov(x2)

(Sp <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1+n2-2))

# approach 1: compare T^2 with critical T^2

(Tsq <- t(xbar1-xbar2) %*% solve(Sp*(1/n1 + 1/n2)) %*% (xbar1-xbar2))
(Tsq_crit <- qf(1-alpha, p, n1+n2-p-1) * p*(n1+n2-2)/(n1+n2-p-1))

# approach 2: compare F with critical F

(F <- Tsq * (n1+n2-p-1)/(p*(n1+n2-2)))
(F_crit <- qf(1-alpha, p, n1+n2-p-1))

# approach 3: compare p-value with critical p (0.05)

(prob_of_F <- pf(F, p, n1+n2-p-1, lower.tail = FALSE))

   
   
   
   
