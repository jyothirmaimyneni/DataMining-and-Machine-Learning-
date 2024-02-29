#read the file
library(readxl)
bank <- read_excel("C:\\Users\\himaj\\Downloads\\DMML\\Bank_Marketing.xlsx")
str(bank)

bank$y <- ifelse(bank$y=='yes',1,0)
bank

# Correlation Matrix
library(corrplot)
corContVar <-cor(bank[sapply(bank,is.numeric)],use = "complete.obs")
col <- colorRampPalette(c("pink", "pink","pink"
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


ranfac <- c("age","job","marital","education","default","housing","loan","contact","month","day_of_week","duration","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y"
)

data_num <- as.data.frame(apply(bank[ranfac], 2, as.numeric))  # Convert all variable types to numeric
sapply(data_num, class)
str(bank[ranfac])

library(dplyr)
bank1<- select(bank,age,duration,campaign,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)
bank1

set.seed(125)
dat.d<-sample(1:nrow(bank),size=nrow(bank)*0.7,replace=FALSE)

train.bank<-bank[dat.d,]
test.bank<-bank[-dat.d,]

bank_t<- select(train.bank,age,duration,campaign,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)
bank_tt<- select(test.bank,age,duration,campaign,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)
install.packages('fastDummies')
library(class)
bank_knn <- knn(train = bank_t,test = bank_tt,cl = train.bank$y,k = 1)
bank_knn_1<- knn(train = bank_t,test = bank_tt,cl = train.bank$y,k = 3)
bank_knn_2<- knn(train = bank_t,test = bank_tt,cl = train.bank$y,k = 5)
bank_knn_3<- knn(train = bank_t,test = bank_tt,cl = train.bank$y,k = 9)
bank_knn_4<- knn(train = bank_t,test = bank_tt,cl = train.bank$y,k = 15)
misClassError <- mean(bank_knn != test.bank$y)
print(paste('Accuracy =', 1-misClassError))

misClassError <- mean(bank_knn_1 != test.bank$y)
print(paste('Accuracy =', 1-misClassError))

misClassError <- mean(bank_knn_2 != test.bank$y)
print(paste('Accuracy =', 1-misClassError))

misClassError <- mean(bank_knn_4 != test.bank$y)
print(paste('Accuracy =', 1-misClassError))

#knn
table(bank_knn,test.bank$y)
caret::confusionMatrix(table(bank_knn,test.bank$y))
# F1 Score
library(MLmetrics)
F1_Score(bank_knn,test.bank$y,positive = '1')

#knn1
table(bank_knn_1,test.bank$y)
caret::confusionMatrix(table(bank_knn_1,test.bank$y))
F1_Score(bank_knn_1,test.bank$y,positive = '1')

#knn_2
table(bank_knn_2,test.bank$y)
caret::confusionMatrix(table(bank_knn_2,test.bank$y))
F1_Score(bank_knn_2,test.bank$y,positive = '1')

#knn_3
table(bank_knn_3,test.bank$y)
caret::confusionMatrix(table(bank_knn_3,test.bank$y))
F1_Score(bank_knn_3,test.bank$y,positive = '1')

#knn_4
table(bank_knn_4,test.bank$y)
caret::confusionMatrix(table(bank_knn_4,test.bank$y))
F1_Score(bank_knn_4,test.bank$y,positive = '1')
