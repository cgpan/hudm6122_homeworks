library(MVA)
library(HSAUR2)

names(pottery)
summary(pottery$Al2O3)
vec_01 <- pottery[pottery$kiln == "1", 1]