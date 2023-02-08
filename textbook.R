
rm(list=ls())

###################################################
### MVA:tab:hypo
###################################################
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
### MVA:tab:USairpollution
###################################################
data("USairpollution", package = "HSAUR2")


###################################################
### MVA:tab:heptathlon
###################################################
data("heptathlon", package = "HSAUR2")


###################################################
### MVA:tab: headsize
###################################################
data("frets", package = "boot")
headsize <- frets


###################################################
### MVA:tab:earthquakes
###################################################
data("quakes", package="datasets")
earthquake=quakes


###################################################
### MVA:tab:LAdepr
###################################################
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
x <- LAdepr
LAdepr <- as.data.frame(LAdepr)



