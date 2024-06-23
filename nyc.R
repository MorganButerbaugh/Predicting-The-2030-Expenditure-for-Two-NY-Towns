library(tidyverse)
library(ggResidpanel)
library(Hmisc)

ny<-read_csv("ny.csv")
names(ny)

hist.data.frame(ny)

fit_full<-lm(expend~.,data=ny)
fit_reduced<-lm(expend~1,data=ny)

forward<-step(fit_reduced, scope = list(lower=fit_reduced,upper=fit_full),direction = "forward")
summary(forward)
resid_panel(forward,plots=c("resid",'qq'))

ny<-ny %>% select(-pop, -pgs, -density)
names(ny)

fit<-lm(log(expend)~log(wealth)+log(income),data=ny)
resid_panel(fit,plots=c("resid",'qq'))

new<-tibble(wealth=c(89000,115000),income=c(20000,25000))
prediction<-predict(fit,new=new,interval="prediction")
prediction
summary(fit)
sigma2<-sum(residuals(fit)^2)/911
sigma2
exp(prediction[1,2]+sigma2/2) #lower for Warwick
exp(prediction[1,3]+sigma2/2) #upper for Warwick
exp(prediction[2,2]+sigma2/2) #lower for Tuxedo
exp(prediction[2,3]+sigma2/2) #upper for Tuxedo
