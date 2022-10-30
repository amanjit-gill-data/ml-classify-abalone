# QUIZ 1 - QUESTION 4

library(GGally)
library(dplyr)
library(MVN)

b <- as_tibble(read.csv("bridges.csv"))
head(b)

# PART A - pairwise plots and normality test

b.subset <- b %>% select(erected, length)
b.subset

ggpairs(b.subset)

mvn(b.subset, mvnTest = "mardia", univariateTest = "SW")

# PART B - Hypothesis Test

# extract year built, length and river
b.subset <- b %>% select(river, erected, length)
head(b.subset)

# separate into separate sets by river (M or A)
b.M <- b.subset[b.subset$river == 'M', ] %>% select(erected, length)
b.A <- b.subset[b.subset$river == 'A', ] %>% select(erected, length)

p <- ncol(b.A)

(n1 <- nrow(b.M))
(n2 <- nrow(b.A))

xbar1 <- colMeans(b.M)
S1 <- cov(b.M)

xbar2 <- colMeans(b.A)
S2 <- cov(b.A)

Sp <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1+n2-2)









