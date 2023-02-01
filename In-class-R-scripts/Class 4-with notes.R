
###################################################
###  setup
###################################################
# the lib of this book
library("MVA")
# 
library("lattice")

# This is week 3 class mainly talk about the Chapter 1's code replication.
# topic 1: nominal, ordinal, interval, ratio
# a quick way to add comment
cat("here you can type any comment")
# note the last question in the HW need to be revised!
# anoter way to get the distance
# norm(matrix(), "F")
# need to interpret the outcome in the midterm !!!!!
# how to interpret the multivariate normal distribution
# a plane to cut the graphs if it is circle? if it is an Ellipse,
# positive correlation? negative correlation?
# how to coduct the inner prodcut
z<-c(1,2,3,4)
y<-c(5,6,7,8)

z <-matrix(z,4,1)
y<- matrix(y,4,1)
t(z)%*%y
# hardamard product vs inner product
# qq plot compare the theoratical plot vs the sample data
# if they are the same  or close we can say normal distributed
# how to interpret the chi-quantile plot ????? 

#-----------------
# 02.01.2023
#------------------
# note the apply() function 
# to get the mean of each vector
z <- matrix(rnorm(20),5,4)
m <- rep(0, 4)
for (i in 1:4){
  m[i] <- mean(z[, i])
}
m

# try to use the apply function to get the mean vector in oneline
# MARGIN is to indicate the row or column. it represent the dimension
apply(z, MARGIN=2, FUN = mean)
apply(z, MARGIN=2, FUN = sd)
# you can also omited the argument
apply(z, 2, mean)

#-----------------
# 02.01.2023
# chapter 2 how to visualize the multivariate data
#------------------
data("USairpollution", package = "HSAUR2")

plot(popul~ manu, data = USairpollution)
# to add the corresponding label on the x and y axises
rug(USairpollution$manu, side =1)
rug(USairpollution$popul, side =2)

# one way to arrange the output layout
# chapter 2.1.1

z <- c(2,5,8,-1)
# a negative number means to exclude that number 
z[-3]

#----------------------------------------
# parametric and non-parametric estimation
#---------------------------------------

n <- 100
# seq is to create a sequence of number from A to B with the 
# interval of C, and the length.out argument is to define the length
# to generate a sequence from -3 to 3 with the length of n
z <- seq(-3,3, length.out =n)
x <- rnorm(n, mean = 0, sd=1)
# set the freq = F to give density on the Y-axis
# if freq = T, it will return the frequency
hist(x,freq = F)
# draw a line using  the z as x-axis and a 100 normal distributed number as Y
# this is what we called the true density
lines(z, dnorm(z, mean = 0, sd =1, log = F), col=2, lwd=4)
# parametric estimator is you use the historical data to estimate
lines(z, dnorm(z, mean = mean(x), sd = sd(x), log = F), col=3, lwd=4)
# kernel density function is used at you don't know how the data is distributed.
# you use a small kernel with normal/bimodal/triangular distribution to cover each
# observation and then culmulate all the distribution together to have a overall
# view of the dataset.
lines(density(x)$x, density(x)$y, col=4, lwd=4)  


###################################################
### MVA:tab:hypo
###################################################
# to create the table
hypo <-
  structure(list(individual = 1:10, sex = structure(c(2L, 2L, 2L,
    2L, 2L, 1L, 1L, 1L, 1L, 1L), .Label = c("Female", "Male"), class = "factor"),
    age = c(21L, 43L, 22L, 86L, 60L, 16L, NA, 43L, 22L, 80L),
    IQ = c(120L, NA, 135L, 150L, 92L, 130L, 150L, NA, 84L, 70L
    ), depression = structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
    1L, 1L), .Label = c("No", "Yes"), class = "factor"), health = structure(c(3L,
    3L, 1L, 4L, 2L, 2L, 3L, 1L, 1L, 2L), .Label = c("Average",
    "Good", "Very good", "Very poor"), class = "factor"), weight = c(150L,
    160L, 135L, 140L, 110L, 110L, 120L, 120L, 105L, 100L)), .Names = c("individual",
    "sex", "age", "IQ", "depression", "health", "weight"), class = "data.frame", row.names = c(NA, -10L))



###################################################
### MVA:hypo:subset
###################################################
# to subset the original dataset
hypo[1:2, c("health", "weight")]


###################################################
### MVA:tab:measure
###################################################
measure <-
  structure(list(V1 = 1:20, V2 = c(34L, 37L, 38L, 36L, 38L, 43L,
                 40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L,
                 35L), V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,
                 24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), V4 = c(32L,
                 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L,
                 38L, 37L, 38L, 37L, 40L, 35L)), .Names = c("V1", "V2", "V3",
                 "V4"), class = "data.frame", row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")


###################################################
### MVA:tab:pottery
###################################################
data("pottery", package = "HSAUR2")


###################################################
### MVA:tab:exam
###################################################
exam <-
  structure(list(subject = 1:5, math = c(60L, 80L, 53L, 85L, 45L
  ), english = c(70L, 65L, 60L, 79L, 80L), history = c(75L, 66L,
  50L, 71L, 80L), geography = c(58L, 75L, 48L, 77L, 84L), chemistry = c(53L,
  70L, 45L, 68L, 44L), physics = c(42L, 76L, 43L, 79L, 46L)), .Names = c("subject",
  "maths", "english", "history", "geography", "chemistry", "physics"
  ), class = "data.frame", row.names = c(NA, -5L))


###################################################
### MVA:USairpollution:tab
###################################################
data("USairpollution", package = "HSAUR2")


###################################################
### Section 1.5
###################################################
cov(measure[, c("chest", "waist", "hips")])

# note another method to subset the data !!!!!
# note another method to subset the data !!!!!
cov(subset(measure, gender == "female")[, 
           c("chest", "waist", "hips")])

cov(subset(measure, gender == "male")[, 
           c("chest", "waist", "hips")])

cor(measure[, c("chest", "waist", "hips")])


dist(scale(measure[, c("chest", "waist", "hips")],center = FALSE))


x <- dist(scale(measure[, c("chest", "waist", "hips")],center = FALSE))
as.dist(round(as.matrix(x), 2)[1:12, 1:12])
cat("...")

# need to interpret the outcome in the midterm !!!!!
# need to interpret the outcome in the midterm !!!!!




###################################################
### Section 1.6
###################################################

library("mvtnorm")
# density of the normal distribution
x <- y <- seq(from = -3, to = 3, length = 50)
dat <- as.matrix(expand.grid(x, y))
d <- dmvnorm(dat, mean = c(0, 0),sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
d <- matrix(d, ncol = length(x))

x11()
persp(x = x, y = y, z = d, xlab = "x1", ylab = "x2",zlab = "f(x)")


###################################################
x <- seq(from = -3, to = 3, length = 1000)
Fx <- pnorm(x)
Fy <- pnorm(x, mean = -1)

# useful function x11() is to open a new graph-drawing window
# all the code under this code are all draw all this window
x11()
plot(x, Fx, type = "l", axes = FALSE, xlab = "",
     ylab = "Cumulative distribution function") 
lines(x, Fy, type = "l")
x0 <- which.min((x - 1.2)^2)
x05 <- which.min((x + 0.5)^2)
x08 <- which.min((x + 0.9)^2)
xx <- which.min(abs(Fy - Fx[x0]))
arrows(x0 = c(min(x), x[x0], x[xx], x[x08], x[x08], x[x08]),
       y0 = c(Fx[x0], Fx[x0], Fy[xx], 0, Fx[x08], Fy[x08]), 
       x1 = c(x[x0], x[x0], x[xx], x[x08], -3, -3), 
       y1 = c(Fx[x0], 0, 0, Fy[x08], Fx[x08], Fy[x08]))
mtext(at = c(x[x08], x[xx], x[x0]), side = 1, line = 1, text =
      c(expression(q), expression(q[2](p)), expression(q[1](p))))
mtext(at = c(0, Fx[x08], Fy[x08], Fx[x0], 1), line = 1, side = 2, text =
      c(0, expression(p[1](q)), expression(p[2](q)), expression(p), 1)) 
box()


###################################################
x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)

d <- apply(x, MARGIN = 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))

x11()
par(mfrow=c(1,3))
qqnorm(measure[,"chest"], main = "chest"); qqline(measure[,"chest"])
qqnorm(measure[,"waist"], main = "waist"); qqline(measure[,"waist"])
qqnorm(measure[,"hips"], main = "hips"); qqline(measure[,"hips"])

x11()
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
xlab = expression(paste(chi[3]^2, " Quantile")),ylab = "Ordered distances")
abline(a = 0, b = 1)


###################################################
x11()
layout(matrix(1:8, nc = 2))
sapply(colnames(USairpollution), function(x) {
    qqnorm(USairpollution[[x]], main = x)
    qqline(USairpollution[[x]])
})

x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))

x11()
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 7), 
     sd <- sort(d),
     xlab = expression(paste(chi[7]^2, " Quantile")), 
     ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)


###################################################
### Exercise 1.4
###################################################
s <- c(3.8778,
       2.8110,  2.1210,
       3.1480,  2.2669,  2.6550,
       3.5062,  2.5690,  2.8341,   3.2352)
S <- diag(4)
S[!lower.tri(S)] <- s
S <- S + t(S)
diag(S) <- diag(S) / 2


###################################################
### Exercise 1.5
###################################################
X <- matrix(
 c(3, 4, 4, 6, 1,
   5, 1, 1, 7, 3,
   6, 2, 0, 2, 6,
   1, 1, 1, 0, 3,
   4, 7, 3, 6, 2,
   2, 2, 5, 1, 0,
   0, 4, 1, 1, 1,
   0, 6, 4, 3, 5,
   7, 6, 5, 1, 4,
   2, 1, 4, 3, 1), ncol = 5)


