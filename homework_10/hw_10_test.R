library(MVA)
demo("Ch-LME")


data("BtheB", package = "HSAUR2")
head(BtheB)
dim(BtheB)
head(BtheB_long)
dim(BtheB_long)

BtheB_lme3 <- lm(bdi ~ bdi.pre + time + treatment+ drug+ length +subject, 
                 data = BtheB_long, na.action = na.omit)
coef(BtheB_lme3)[4]