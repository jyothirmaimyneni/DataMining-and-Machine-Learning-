#read the file
library(readxl)
realtordata <- read_excel("C:\\Users\\himaj\\Downloads\\DMML\\realtorjyo.xlsx")
realtordata

str(realtordata)

realtordatafac <-
  c("price","bed","bath","acre_lot","zip_code","house_size")

realtordata[realtordatafac] <- lapply(realtordata[realtordatafac],function(x) as.numeric(as.character(x)))

str(realtordata)

library(dplyr)
library(tidyr)
rd2 <- select(realtordata,bed,bath,acre_lot,zip_code,house_size,price)

rd2 <- rd2 %>% drop_na()

sapply(rd2,function(x) sum(is.na(x)))

realtordata <- realtordata %>% drop_na()

sapply(realtordata,function(x) sum(is.na(x)))

set.seed(125)
split <- sample(1:nrow(rd2),size=nrow(rd2)*0.8,replace=FALSE)
split

train.grid<-rd2[split,]
test.grid<-rd2[-split,]
train.grid

library(randomForest)
bestmtry <- tuneRF(train.grid,train.grid$price,stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 

model <- randomForest(price~.,data= train.grid)
model

importance(model)
varImpPlot(model) 


pred_test <- predict(model, newdata = test.grid, type= "class")

pred_test

library(Metrics)
print(paste0('MAE: ' , mae(pred_test,test.grid$price) ))

print(paste0('MSE: ' ,caret::postResample(pred_test,test.grid$price)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(pred_test,test.grid$price)['Rsquared'] ))