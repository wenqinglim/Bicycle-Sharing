library(dplyr)
setwd("~/NUS/20162017 Semester 2/ST3131/Project/")

d1=read.table("Bike-Sharing-Dataset/day.csv", sep=',', header=TRUE)

cnt~ weekday+weathersit+atemp+hum+windspeed

d2=d1[,c("weekday", "weathersit", "temp", "atemp", "hum", "windspeed", "cnt")]
model=lm(cnt~., data=d2)
summary(model)
anova(model)
plot(model)

attach(d2)
plot(cnt~workingday)
plot(cnt~weathersit)
plot(cnt~temp)
plot(cnt~atemp)
plot(cnt~hum)
plot(cnt~windspeed)

plot(cnt~log(temp))

#Stepwise Regression (based on AIC)
model1 <- lm(cnt~weekday)
summary(model1)
nullmodel<-lm(cnt~1, data=d2)
fullmodel<-lm(cnt~., data=d2)
step(nullmodel, data=d2, scope=list(upper=fullmodel, lower=nullmodel), direction="both", k=2, test='F')

#Best model so far: cnt ~ atemp + weathersit + windspeed + hum + weekday
#Variance Stabilizing Transformation

d3=d2[,c("atemp", "weathersit", "windspeed", "hum","weekday", "cnt")]
model=lm(cnt~.,data=d3)
plot(model)
summary(model)
modeltrans=lm((cnt**.75)~.,data=d3)
plot(modeltrans)
#Test significance of transformed model
anova(modeltrans)
summary(modeltrans)

#Test of independance
#Runs Test
library(tseries)
res<-model$residuals
runs.test(factor(sign(res)))
#p<2.2e-16, Do not rej H0, no serial correlation exists
#Durbin-Watson
library(car)
durbinWatsonTest(model)
#DW=0.377, p=0, no serial correlation 