library(readxl)
realtordata <- read_excel("C:\\Users\\himaj\\Downloads\\DMML\\realtorjyo.xlsx")
realtordata
str(realtordata)

realtordatafac <-
  c("price","bed","bath","acre_lot","zip_code","house_size")

realtordata[realtordatafac] <- lapply(realtordata[realtordatafac],function(x) as.numeric(as.character(x)))

str(realtordata)
#missing data
library(dplyr)
library(tidyr)
rd2 <- select(realtordata,bed,bath,acre_lot,zip_code,house_size,price)

rd2 <- rd2 %>% drop_na()

sapply(rd2,function(x) sum(is.na(x)))

realtordata <- realtordata %>% drop_na()

sapply(realtordata,function(x) sum(is.na(x)))
#training
set.seed(125)
grid_data <- sample(1:nrow(rd2),size=nrow(rd2)*0.7,replace=FALSE)

train.grid<-rd2[grid_data,]
test.grid<-rd2[-grid_data,]

z <- test.grid$price
z
y<-train.grid$price
y

x<-data.matrix(train.grid[,c('bed','bath','acre_lot','zip_code','house_size','price')])
x
w<-data.matrix(test.grid[,c('bed','bath','acre_lot','zip_code','house_size','price')])
w
install.packages('glmnet')
library(glmnet)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
cv_model
best_lambda <- cv_model$lambda.min
best_lambda
best_lambda <- cv_model$lambda.min
plot(cv_model)

best_model <- glmnet(w, z, alpha = 1, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = w)
y_predicted

sst <- sum((z - mean(z))^2)
sse <- sum((y_predicted - z)^2)

rsq <- 1 - sse/sst
rsq
install.packages('Metrics')
library(Metrics)
# RF Tmin pred with cross validation#
print(paste0('MAE: ' , mae(y_predicted,test.grid$price) ))

print(paste0('MSE: ' ,caret::postResample(y_predicted,test.grid$price)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(y_predicted,test.grid$price)['Rsquared'] ))