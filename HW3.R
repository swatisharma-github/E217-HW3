# Econ 217 Assignment 3
# Swati Sharma

library("foreign")
library("dplyr")

vetData <- read.csv("https://people.ucsc.edu/~aspearot/Econ_217/veteran.csv")

subData <- subset(vetData, TIME!=0, na.rm=TRUE)
summary(subData)
tapply(subData$TIME, subData$trt, summary)
tapply(subData$celltype, subData$trt, summary)
subData <- subset(subData, TIME<900)

library(survival)  

fit <- survfit(Surv(TIME,Y)~trt, data = subData)
plot(fit,lty = 2:3, lwd = 3, col = c("skyblue3", "firebrick4"), 
     main = "Kaplan Meier Estimator", xlab = "Time (Days)", ylab = "Probability of Survival" )
legend(23, 1, c("Control", "Treatment"), lty = 2:3, lwd = 3, col = c("skyblue3", "firebrick4"))