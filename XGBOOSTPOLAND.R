
#reading dataset
polanddata <- read.csv("E:\\Masters in Ireland\\DMML\\DMML Project\\JyoData\\Car_Prices_Poland.csv")
polanddata

sapply(polanddata,function(x) sum(is.na(x)))

str(polanddata)

polanddatafac <-
  c("mark","model","generation_name","fuel")
polanddata[polanddatafac] <- lapply(polanddata[polanddatafac], factor)

str(polanddata)

library(dplyr)

pd2 <- select(polanddata,mark,model,generation_name,year,mileage,vol_engine,fuel,price)

pd2
library(xgboost)
library(caret)
parts = createDataPartition(pd2$price, p = .8, list = F)
train = pd2[parts, ]
test = pd2[-parts, ]

train_x = data.matrix(train)
train_x
train_y = train[,8]

test_x = data.matrix(test)
test_y = test[, 8]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

summary(model_xgboost)

predicted = predict(model_xgboost, xgb_test)

test['predicted'] <- predicted
View(test)

residuals = test_y - predicted
residuals
RMSE = sqrt(mean(residuals^2))
RMSE
cat('The root mean square error of the test data is ', round(RMSE1,3),'\n')


y_test_mean = mean(test_y)

# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )

# Calculate residual sum of squares
rss =  sum(residuals^2)

# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = test_y))

# Plot predictions vs test data

ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
