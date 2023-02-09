n <- 1000
mu <- 2
s2 <- 3

# gen a vector of sample data from normal distribution
Xi <-rnorm(n, mu, sd = sqrt(s2))

# plot the hist
hist(Xi, freq=FALSE, 
     xlim = c(-6+mu, 6+mu), ylim = c(0,0.25),
     cex.axis = 0.5, cex.lab= 2) # non-parametrix which mean

mu.hat <- mean(Xi)
s2.hat <- var(Xi)
# to add a true line of normal distribution to compare to the estimated line
x <- seq(-3*sqrt(s2)+mu,3*sqrt(s2)+mu,length.out=200)
# use dnorm to draw line based on the estimated mu 
# we need to compare the empirical and the true mu
lines(x, dnorm(x, mean = mu.hat, sd = sqrt(s2.hat)),col="red") # parametric estimator

# true(underlying) density
lines(x, dnorm(x, mean = mu, sd = sqrt(s2)),col="green", lwd=3)

h <- 0.5   # change from .25 to 0.5 
# r have a default value  refer to Vebables and Ripley(2002).
# see p46 for more details
# the larger the smoother
# try density function
f.hat <- density(Xi, n=200, 
                 from = -3*sqrt(s2)+mu, to= 3*sqrt(s2)+mu, 
                 bw = h)
x.fhat <-  f.hat$x
y.fhat <- f.hat$y # non-parametric estimator
# -----------
lines(x.fhat, y.fhat, col="blue", lty=2,lwd=2)

f.hat <- density(Xi, n=200, 
                 from = -3*sqrt(s2)+mu, to= 3*sqrt(s2)+mu, 
                 bw = h, kernel="epanechnikov")

lines(x.fhat, y.fhat, col="blue", lty=3,lwd=2)

# --------------
# how to get the eigenvalue and eigenvector


S <- cov(measure[,1:3])
S 


