setwd("~/Desktop/PhD_Learning/HUDM6122 Multivariate Analysis I/hudm6122_homeworks/homework_08")
# import the data
df <- read.table("food.txt", header = T)
rownames(df) <- df$Workertype
df <- df[,-c(1,2)]
colnames(df)
dim(df)
# RUN CA
library(FactoMineR)
ca_ <- CA(df)