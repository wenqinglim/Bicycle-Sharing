###Plot Residuals
library(dplyr)
setwd("~/NUS/20162017 Semester 2/ST3131/Project/")

d1=read.table("Bike-Sharing-Dataset/day.csv", sep=',', header=TRUE)

d2=d1[,c("weekday", "weathersit", "temp", "atemp", "hum", "windspeed", "cnt")]

model1=lm(cnt~., data=d2)
Residuals=model1$residuals
Fitted=model1$fitted.values
plot(Residuals~Fitted, data=d2, main="Residuals VS Fitted Values")
abline(h=0,lty=2)
plot(Residuals~weekday, data=d2, main="Residuals VS weekday")
abline(h=0,lty=2)
plot(Residuals~weathersit, data=d2, main="Residuals VS weathersit")
abline(h=0,lty=2)
plot(Residuals~temp, data=d2, main="Residuals VS temp")
abline(h=0,lty=2)
plot(Residuals~atemp, data=d2, main="Residuals VS atemp")
abline(h=0,lty=2)
plot(Residuals~hum, data=d2, main="Residuals VS hum")
abline(h=0,lty=2)
plot(Residuals~windspeed, data=d2, main="Residuals VS windspeed")
abline(h=0,lty=2)
qqnorm(Residuals,xlab="normal",ylab="ordered residuals")
qqline(Residuals,lty=2)

d3=d2[,c("atemp", "weathersit", "windspeed", "hum","weekday", "cnt")]
model2=lm(cnt~., data=d3)
Residuals=model2$residuals
Fitted=model2$fitted.values
plot(Residuals~Fitted, data=d3, main="Residuals VS Fitted Values")
abline(h=0,lty=2)
plot(Residuals~weekday, data=d3, main="Residuals VS weekday")
abline(h=0,lty=2)
plot(Residuals~weathersit, data=d3, main="Residuals VS weathersit")
abline(h=0,lty=2)
plot(Residuals~atemp, data=d3, main="Residuals VS atemp")
abline(h=0,lty=2)
plot(Residuals~hum, data=d3, main="Residuals VS hum")
abline(h=0,lty=2)
plot(Residuals~windspeed, data=d3, main="Residuals VS windspeed")
abline(h=0,lty=2)
qqnorm(Residuals,xlab="normal",ylab="ordered residuals")
qqline(Residuals,lty=2)

model3=lm(cnt^0.75~weekday+weathersit+atemp+hum+windspeed, data=d3)
Residuals=model3$residuals
Fitted=model3$fitted.values
plot(Residuals~Fitted, data=d3, main="Residuals VS Fitted Values")
abline(h=0,lty=2)
plot(Residuals~weekday, data=d3, main="Residuals VS weekday")
abline(h=0,lty=2)
plot(Residuals~weathersit, data=d3, main="Residuals VS weathersit")
abline(h=0,lty=2)
plot(Residuals~atemp, data=d3, main="Residuals VS atemp")
abline(h=0,lty=2)
plot(Residuals~hum, data=d3, main="Residuals VS hum")
abline(h=0,lty=2)
plot(Residuals~windspeed, data=d3, main="Residuals VS windspeed")
abline(h=0,lty=2)
qqnorm(Residuals,xlab="normal",ylab="ordered residuals")
qqline(Residuals,lty=2)

attach(d3)
atemp2 = atemp^2
model4=lm(cnt^0.75~weekday+weathersit+atemp2+hum+windspeed) ##final model
Residuals=model4$residuals
Fitted=model4$fitted.values
plot(Residuals~Fitted, data=d3, main="Residuals VS Fitted Values")
abline(h=0,lty=2)
plot(Residuals~weekday, data=d3, main="Residuals VS weekday")
abline(h=0,lty=2)
plot(Residuals~weathersit, data=d3, main="Residuals VS weathersit")
abline(h=0,lty=2)
plot(Residuals~atemp2, data=d3, main="Residuals VS atemp2")
abline(h=0,lty=2)
plot(Residuals~hum, data=d3, main="Residuals VS hum")
abline(h=0,lty=2)
plot(Residuals~windspeed, data=d3, main="Residuals VS windspeed")
abline(h=0,lty=2)
qqnorm(Residuals,xlab="normal",ylab="ordered residuals")
qqline(Residuals,lty=2)

