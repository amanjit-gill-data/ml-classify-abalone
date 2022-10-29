# QUIZ 1
# A. GILL

# QUESTION 3

n <- 12
p = 2
xbar <- cbind(c(-1.2, -3.9))
S <- rbind(c(6,1), c(1,6))
Sinv <- solve(S)

# PART A - confidence ellipsoid at level 90%

alpha <- 0.1
S.eig <- eigen(S)
scale <- p*(n-1)*qf(1-alpha, p, n-p)/(n*(n-p))

# major axis endpoints
(major_pt1 <- xbar - S.eig$vectors[,1] * sqrt(S.eig$values[1]*scale))
(major_pt2 <- xbar + S.eig$vectors[,1] * sqrt(S.eig$values[1]*scale))

# minor axis endpoints
(minor_pt1 <- xbar - S.eig$vectors[,2] * sqrt(S.eig$values[2]*scale))
(minor_pt2 <- xbar + S.eig$vectors[,2] * sqrt(S.eig$values[2]*scale))

# vector of x-coordinates for major axis
major_xvals <- c(major_pt1[1], major_pt2[1])

# vector of y-coordinates for major axis
major_yvals <- c(major_pt1[2], major_pt2[2])

# vector of x-coordinates for minor axis
minor_xvals <- c(minor_pt1[1], minor_pt2[1])

# vector of y-coordinates for minor axis
minor_yvals <- c(minor_pt1[2], minor_pt2[2])

# plot major and minor axes
plot(c(major_xvals,minor_xvals), c(major_yvals,minor_yvals), pch=4, col="blue", lwd=2)
lines(major_xvals, major_yvals)
lines(minor_xvals, minor_yvals)
points(xbar[1], xbar[2], pch=16, col="blue")

# PART B - confidence ellipsoid for known variance

# to build the ellipse, everything stays the same except for the scale

scale <- qchisq(1-alpha, p)

# major axis endpoints
(major_pt1 <- xbar - S.eig$vectors[,1] * sqrt(S.eig$values[1]*scale))
(major_pt2 <- xbar + S.eig$vectors[,1] * sqrt(S.eig$values[1]*scale))

# minor axis endpoints
(minor_pt1 <- xbar - S.eig$vectors[,2] * sqrt(S.eig$values[2]*scale))
(minor_pt2 <- xbar + S.eig$vectors[,2] * sqrt(S.eig$values[2]*scale))

# vector of x-coordinates for major axis
major_xvals <- c(major_pt1[1], major_pt2[1])

# vector of y-coordinates for major axis
major_yvals <- c(major_pt1[2], major_pt2[2])

# vector of x-coordinates for minor axis
minor_xvals <- c(minor_pt1[1], minor_pt2[1])

# vector of y-coordinates for minor axis
minor_yvals <- c(minor_pt1[2], minor_pt2[2])

# plot major and minor axes
plot(c(major_xvals,minor_xvals), c(major_yvals,minor_yvals), pch=4, col="blue", lwd=2, xlim=c(-5,2), ylim=c(-7,-1))
lines(major_xvals, major_yvals)
lines(minor_xvals, minor_yvals)
points(xbar[1], xbar[2], pch=16, col="blue")

# PART C - projection to get confidence interval

scale <- p*(n-1)*qf(1-alpha, p, n-p)/(n*(n-p))

# for variable x1
l_x1 <- c(1,0)
x1_ci_low <- c(crossprod(l_x1, xbar)) - sqrt(scale * t(l_x1) %*% S %*% l_x1)
x1_ci_high <- c(crossprod(l_x1, xbar)) + sqrt(scale * t(l_x1) %*% S %*% l_x1)

# for variable x2
l_x2 <- c(0,1)
x2_ci_low <- c(crossprod(l_x2, xbar)) - sqrt(scale * t(l_x2) %*% S %*% l_x2)
x2_ci_high <- c(crossprod(l_x2, xbar)) + sqrt(scale * t(l_x2) %*% S %*% l_x2)

# plot x1 CI as vertical lines
# plot x2 CI as horizontal lines
abline(v=c(x1_ci_low, x1_ci_high))
abline(h=c(x2_ci_low, x2_ci_high))


