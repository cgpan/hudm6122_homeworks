# about running the PCA

# ----------------------------------------------
# Q1:how to find the eigenvectors of a given martix
# ----------------------------------------------

# prepare the data
matrix_01 <- matrix(c(-4, -4,
                      -1, 1,
                      1, -1, 
                      4, 4),4,2, byrow = T)
matrix_01

# since this matrix is already centered
# to have the observed covariance matrix
m_01_cov <- cov(matrix_01)

# to have the eigenvector by using eigen()
eigen_ <- eigen(m_01_cov)
y <- matrix_01 %*% eigen_$vectors
y
?eigen

# ----------------------------------------------
# Q2: to have the eigenvector and eigenvalue by hand
# ----------------------------------------------

dat = read.table("/Users/panpeter/Desktop/PhD_Learning/HUDM6122 Multivariate Analysis I/week 6/marks.dat",head=TRUE)
n=dim(dat)[1]
p=dim(dat)[2]
names(dat)

X=as.matrix(dat,byrow=FALSE)
m=apply(X,MARGIN=c(2),FUN=mean)
# center the data
dmX=X-m
# compute the variance matrix
S=t(dmX)%*%dmX/n

L=eigen(S)$values
W=eigen(S)$vectors
Y=dmX%*%W

x11()
plot(dat$Phys,dat$Stat)

# import the data
depr <- c(
  0.212,
  0.124,  0.098,
  -0.164,  0.308,  0.044,
  -0.101, -0.207, -0.106, -0.208,
  -0.158, -0.183, -0.180, -0.192, 0.492)
LAdepr <- diag(6) / 2
LAdepr[upper.tri(LAdepr)] <- depr
LAdepr <- LAdepr + t(LAdepr)
rownames(LAdepr) <- colnames(LAdepr) <- c("CESD", "Health", "Gender", "Age", "Edu", "Income")
# LAdepr <- as.data.frame(LAdepr)
LAdepr
r11 <- LAdepr[1:2, 1:2]
r11
r22 <- LAdepr[-(1:2), -(1:2)]
r22
r12 <- LAdepr[1:2, -(1:2)]
r21 <- LAdepr[-(1:2), 1:2]
r12
r21
E1 <- solve(r11)%*%r12%*%solve(r22)%*%r21
E2 <- solve(r22)%*%r21%*%solve(r11)%*%r12


# -------------------------------------------------------------
x <- rnorm(5)
y <- rnorm(5)

# calculate the distance
sum((x-y)^2)

# norm function will be helpful.
?norm()

# make sure the input is matrix! R always give a number array df
x <- matrix(x,5,1)
y <- matrix(y,5,1)
# calculate the euclidean distance
norm(x-y, type = "F")^2 
# we need to square to have the elu-distance, since norm "F" gives you d_ij
# not d_ij^2

# -------------------- class 7 ----------------------
n <- 20
q <- 3
X <- matrix(rnorm(n*q),n,q)
S <- t(X)%*%X
B <- X %*% t(X)
# to have the rank of the matrix of S and B using rankMatrix
library(Matrix)
rankMatrix(X)

round(eigen(S)$value, 5)
dim(S)

# we have 20 eignvalues and eigenvectors, but we only have 3!!rank!!
round(eigen(B)$value, 5)
dim(B)

lambda <- round(eigen(B)$values, 5)
V <- eigen(B)$vectors
dim(V)
lambda <- eigen(B)$values
B2 <-lambda[1]*V[,1]*t(V[,1])+lambda[2]*V[,2]*t(V[,2])+lambda[3]*V[,3]*t(V[,3])
