

######################################## 
##                                    ##
##              Ex. 2.1               ##
##                                    ##
########################################
# Use the bivariate boxplot on the scatterplot of each pair of variables in the 
# air pollution data to identify any outliers. Calculate the correlation between 
# each pair of variables using all the data and the data with any identified 
# outliers removed. Comment on the results.

# import the data
library(HSAUR2)
library(MVA)
attach(USairpollution)
head(USairpollution)
dim(USairpollution)
# draw the bivariate boxplot of each pair of variables with a for-loop
for (i in 1:7) {
  for (j in i:7) {
    if (i != j) {
      var_pair <- USairpollution[, c(i, j)]
      bvbox(var_pair, 
            xlab = names(USairpollution)[i],
            ylab = names(USairpollution)[j])
      text(USairpollution[,i],USairpollution[,j],
           row.names(USairpollution), # to add the point name
           pos=2,col = "blue")
    } 
  }
}

cat(" From the graphs, we can easily find that the outliers among the observations 
are Chicago, Detroit, Cleveland,Philadelphia, Miami, Phoenix,
 Albuquerque, Providence. I run the correlation matrix on all observations first.")

# create the correaltion matrix on all observations
round(cor(USairpollution),2)
# remove all identified outliers
drop_city <- match(c("Chicago", "Detroit","Cleveland",
                     "Philadelphia", "Miami","Phoenix",
                     "Albuquerque", "Providence"), rownames(USairpollution))

round(cor(USairpollution[-drop_city,]),2)

cat("After dropping all the identified outliers, some of the correlation coefficients 
# has changed to the opposite direction, like from positive to negative, others 
# shrink or increase. It is reasonable since some outliers are with high leverage.")

######################################## 
##                                    ##
##              Ex. 2.2               ##
##                                    ##
########################################
# Compare the chi-plots with the corresponding scatterplots for each pair of 
# variables in the air pollution data. Do you think that there is any advantage 
# in the former?

for (i in 1:7) {
  for (j in i:7) {
    if (i != j) {
      plot(USairpollution[,i],USairpollution[,j],
           xlab = names(USairpollution)[i], ylab=names(USairpollution)[j])
      chiplot(USairpollution[,i],USairpollution[,j],
              main = paste(names(USairpollution)[i],
                           "vs", 
                           names(USairpollution)[j]))
    } 
  }
}

cat("From the results, one can easily find that the scatter plots are sometimes 3
    difficult to identify the independence between two variables. But, 
    comparatively the `chiplot` presents more straightforward way to tell this 
    attribute. For example, it is hard to find the relation from the scattorplot 
    for `manu` and `predays`, but the chiplot clearly demonstrates that these two 
    varriables are independent.  ")

######################################## 
##                                    ##
##              Ex. 2.3               ##
##                                    ##
########################################
# Construct a scatterplot matrix of the body measurements data that has the 
# appropriate boxplot on the diagonal panels and bivariate boxplots on the other 
# panels. Compare the plot with Figure 2.17, and say which diagram you find more 
# informative about the data.


# create the  body measure data. Codes offered by Prof.Motta.
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

# to only extract the continuous data
measure <- measure[,c(1:3)]
par(mfrow=c(3, 3))
for (i in 1:3) {
  for (j in 1:ncol(measure)) {
    if(i != j) {
      bvbox(measure[, c(i, j)])
    }
    else {
      boxplot(measure[[i]])
    }
  }
}


######################################## 
##                                    ##
##              Ex. 2.4               ##
##                                    ##
########################################
# Construct a further scatterplot matrix of the body measurements data that 
# labels each point in a panel with the gender of the individual, and plot on 
# each scatterplot the separate estimated bivariate densities for men and women.
ncols <- 3 # only the first 3 columns are numeric

par(mfrow=c(ncols, ncols))
for (i in 1:ncols) {
  for (j in 1:ncols) {
    plot(measure[, i], measure[, j], 
         xlab = names(measure)[i], ylab=names(measure)[j])
    if(i != j) {
      bvbox(measure[which(measure$gender == "male"), c(i, j)], add=TRUE)
      bvbox(measure[which(measure$gender == "female"), c(i, j)], add=TRUE)
    }
    points(measure[which(measure$gender == "male"), c(i, j)], col="blue")
    points(measure[which(measure$gender == "female"), c(i, j)], col="red")
  }
}