library(rjson)
library(plyr)
library(boot)

my.JSON <- fromJSON(file="https://pomber.github.io/covid19/timeseries.json")

India_json <- lapply(my.JSON$India, function(x){ unlist(x)})
India_df <- rbind.fill(lapply(India_json,function(x) do.call("data.frame", as.list(x))))
India_df <- do.call("rbind", India_json)
India_df <- as.data.frame(India_df)

head(India_df)
names(India_df)
tail(India_df)

India_df$date <- as.Date(India_df$date,format = "%Y-%m-%d")
India_df$confirmed <- as.numeric(India_df$confirmed)
India_df$deaths <- as.numeric(India_df$deaths)
India_df$recovered <- as.numeric(India_df$recovered)

sapply(India_df, typeof)
attach(India_df)

#linear plot
plot(date,confirmed,type ='l',xlab = "date" 
     ,ylab = "confirmed cases", main = "confirmed cases per day")

#scatter plot
plot(date,confirmed,pch = 16,xlab = "date", col = "blue"
     ,ylab = "confirmed cases", main = "confirmed cases per day")

#pairs plot
pairs(India_df,col="blue")

#linear model
fit1 = lm(confirmed~date)
fit1
summary(fit1)
plot(date,confirmed,pch = 16,xlab = "date", col = "blue"
     ,ylab = "confirmed cases", main = "confirmed cases per day")
abline(fit1)

#Cross Validation

## LOOCV
glm.fit=glm(confirmed~date)
cv.glm(India_df,glm.fit)$delta 

loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

loocv(glm.fit)

cv.error=rep(0,5)
degree=1:6
for(d in degree){
  glm.fit=glm(confirmed~poly(date,d))
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(confirmed~poly(date,d))
  cv.error10[d]=cv.glm(India_df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

# so the best degree to use is 3 onwards

# polynomial regression
fit2 = lm(confirmed~poly(date,6))
fit2
summary(fit2)
plot(date,confirmed,pch = 16,xlab = "date", col = "blue"
     ,ylab = "confirmed cases", main = "confirmed cases per day")
points(date,fitted(fit2),col="red",pch=16)

#prediction
new <- data.frame(date = as.Date(c("2020-05-05","2020-05-06","2020-05-07","2020-05-08","2020-05-09","2020-05-10","2020-05-11")))
pred_confirmed <-predict.lm(fit2,new,interval = "confidence",level = 0.95)
newindia_df <- cbind(new, pred_confirmed)
newindia_df
