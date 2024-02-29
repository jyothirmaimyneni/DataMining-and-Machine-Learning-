#read the dataset
library(readxl)
bankdata <- read_excel("C:\\Users\\himaj\\Downloads\\DMML\\Bank_Marketing.xlsx")
bankdata

str(bankdata)

bankdatafac <-
  c("job","marital","education","default","housing","loan","contact", "month","day_of_week","poutcome","y")

bankdata[bankdatafac] <- lapply(bankdata[bankdatafac],factor)
str(bankdata)

#finding null/missing values in the dataset
sapply(bankdata,function(x) sum(is.na(x)))
library(Amelia)
missmap(bankdata,main = "Missing values vs Observed")
#correlation matrix
library(corrplot)
corContVar <-cor(bankdata[sapply(bankdata,is.numeric)],use = "complete.obs")
col <- colorRampPalette(c("darkgreen", "pink","pink"
                          , "green"))
corrplot(corContVar,method = "number",col=col(200),
         order="hclust",
         type = "full",
         #addCoef.col = "black",
         tl.col="black", tl.srt=45, tl.cex=0.7, tl.offset = 0.5,
         number.cex=0.5,
         #diag = FALSE,
         number.digits = 2)
corrplot(corContVar, method="color", col=col(200), 
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.offset = 0.5, #Text label color and rotation
)
mtext("Correlation Plot", family = "serif",
      col = "#0B3948", side = 2, at= 5, line= 2.50, cex=2)
#histogram
par(mfrow=c(2,2))
hist(bank$age)
hist(bank$duration)
hist(bank$campaign)
hist(bank$previous)



#training
sampling = sample(2,nrow(bankdata),replace = TRUE,prob=c(0.7,0.3))
train <- bankdata[sampling==1,]
test <- bankdata[sampling==2,]
#SVM
library(e1071)
banksvm <- svm(y ~ ., data =train, kernel = "linear", cost = 10, scale = FALSE)
banksvm

pred <- predict(banksvm,test)
test$pred <- pred
test

CFM <- table(test$y,test$pred)


Classification_Accuracy <- sum(diag(CFM)/sum(CFM))
Classification_Accuracy

library(e1071)
library(caret)
confusionMatrix(test$y,test$pred,positive='yes')