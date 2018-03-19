# Econ 217 Assignment 3
# Swati Sharma

# Q1 Part A
library("foreign")
library("dplyr")

vetData <- read.csv("https://people.ucsc.edu/~aspearot/Econ_217/veteran.csv")

subData <- subset(vetData, TIME!=0, na.rm=TRUE)
summary(subData)
tapply(subData$TIME, subData$trt, summary)
tapply(subData$celltype, subData$trt, summary)
subData <- subset(subData, TIME<900)

# Q1 Part B
library(survival)  

fit <- survfit(Surv(TIME,Y)~trt, data = subData)
plot(fit,lty = 2:3, lwd = 3, col = c("skyblue3", "firebrick4"), 
     main = "Kaplan Meier Estimator", xlab = "Time (Days)", ylab = "Probability of Survival" )
legend(23, 1, c("Control", "Treatment"), lty = 2:3, lwd = 3, col = c("skyblue3", "firebrick4"))

# Q1 Part C
form<-as.formula(Y~trt+celltype+age+priortherapy+offset(log(TIME)))
hazard_glm<-glm(form,family=poisson("log"),data=subData)
summary(hazard_glm)

#Q2 Part A
orgData <- read.dta("https://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
subData<- subset(orgData, state=="CA" & year==2013 & (!is.na(orgData[,21])) & (!is.na(orgData[,30])))

hourslw <- seq(min(subData$hourslw):max(subData$hourslw))
orderedData <- as.data.frame(hourslw)

loess1 <- loess(log(rw)~hourslw, subData, span = 0.8,degree = 1)
plot(hourslw,predict(loess1, orderedData),type="l", col="skyblue3",
     main = "Loess prediction of log(rw) using hourslw", 
     xlab = "Weekly Hours Worked", ylab = "Predicted Log of Real Wage")

loess2 <- loess(log(rw)~log(hourslw), subData, span = 1,degree = 1)
plot(log(hourslw),predict(loess2, orderedData),type="l", col="firebrick4",
     main = "Loess prediction of log(rw) using log(hourslw)" , 
     xlab = "Log of Weekly Hours Worked", ylab = "Predicted Log of Real Wage")

# Q2 Part B
library("gam")
gam2 <- gam(log(rw)~s(log(hourslw),2)+educ+age,data=subData)
summary(gam2)
plot(gam2,se=TRUE, rug=FALSE,terms="s", xlab = "Log Hours Worked (Weekly)", 
     ylab = "log(rw)", main = "Gam prediction of log(real wage) using hourslw")

# Q2 Part C
for(h in 1:20){
  for(i in 1:nrow(subData)){
    datadrop<-subData[i,]
    datakeep<-subData[-i,]
    fit<-loess(log(rw)~log(hourslw),datakeep,family="gaussian",span=(h/20), degree=1)
    dropfit<-predict(fit,datadrop,se=FALSE)
    sqrerr<-(log(datadrop$rw)-as.numeric(dropfit))^2
    ifelse(i*h==1,results<-data.frame(h,i,sqrerr), results<-rbind(results,data.frame(h,i,sqrerr)))
  }
}

spanVal <- results[which.min(results$sqrerr),1]

loess3 <- loess(log(rw)~(hourslw),subData,span = (spanVal/20),degree = 1)
plot((hourslw),predict(loess3, orderedData), type="l", main = "Loess prediction using 
     cross-validation", xlab = "Weekly Hours Worked", ylab = "Predicted Log of Real Wage" )

# Q3 Part A
subData<- subset(orgData, state=="CA" & year==2013)

# simple linear regression of log(rw) on educ and age
linModel <- lm(log(rw)~educ+age,data=subData)
summary(linModel)

# creating confidence interval
se <- summary(linModel)$coefficients["educCollege","Std. Error"]
B_college <- summary(linModel)$coefficients["educCollege","Estimate"]
error <- se*qt(.975,linModel$df)
CI <- c(B_college-error,B_college+error)
CI

# Q3 Part B
# Bootstrapping
for(b in 1:1000){
  tempData <- subData[sample(nrow(subData),nrow(subData),replace=TRUE),]
  tempLM <- lm(log(rw)~educ+age,data=tempData)
  tempBeta <- coef(summary(tempLM))[4,1]
  tempSE <- coef(summary(tempLM))[4,2]
  ifelse(b==1, bootCoef <- data.frame(tempBeta, tempSE), bootCoef <-rbind(bootCoef,data.frame(tempBeta, tempSE)))
}

quantile(bootCoef$tempBeta,prob=c(0.025,0.975),CI.level = 0.95,na.rm=TRUE)

# Q3 Part C
orgData <- read.dta("https://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
subData<- subset(orgData, state=="CA" & year==2013 & (!is.na(orgData[,21])) & (!is.na(orgData[,30])))

urLM <- lm(log(rw)~educ+age,data=subData)
subData <- cbind(subData, resid = resid(urLM), fitted = fitted(urLM))
subData$resid <- as.numeric(subData$resid)
subData$fitted <- as.numeric(subData$fitted)

for(b in 1:1000){
  subData$randResid<-sample(subData$resid, nrow(subData),replace=TRUE)
  subData$rw_boot <- subData$randResid + subData$fitted
  
  rLM <- lm(rw_boot~age,data=subData)
  SSR_r <- sum(resid(rLM)^2)    
  SSR_ur <- sum(subData$resid^2)
  F_stat <-  ((SSR_r - SSR_ur)/4)/(SSR_ur/(nrow(subData)-6-1))
  ifelse(b==1, residuals <- data.frame(SSR_r, SSR_ur, F_stat), residuals <-rbind(residuals,data.frame(SSR_r, SSR_ur, F_stat)))
}
plot(density(residuals$F_stat), main = "Density of F-stat", xlab="F-stat")
