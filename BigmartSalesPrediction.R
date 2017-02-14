path<-"C:/Users/Rahul Kumar/Desktop/SalesPrediction"
setwd(path)
train<-read.csv("Train.csv",na.strings=c(" ","","NA"))
test<-read.csv("Test.csv",na.strings=c(" ","","NA"))
colSums(is.na(train))
colSums(is.na(test))
summary(train)
as.matrix(prop.table(table(train$Outlet_Type)))
as.matrix(prop.table(table(train$Outlet_Location_Type)))
as.matrix(prop.table(table(train$Item_Fat_Content)))
as.matrix(prop.table(table(train$Item_Type)))
as.matrix(prop.table(table(train$Outlet_Size)))
library("mlr")
imputed_train<-impute(train,classes=list(factor=imputeMode()))
train<-imputed_train$data
imputed_test<-impute(test,classes=list(factor=imputeMode()))
test<-imputed_test$data
library(zoo)
df<-train$Item_Weight
train$Item_Weight<-na.approx(df)
colSums(is.na(train))
df1<-test$Item_Weight
test$Item_Weight<-na.approx(df1)
colSums(is.na(test))
train$Outlet_Year=2017-train$Outlet_Establishment_Year
test$Outlet_Year=2017-test$Outlet_Establishment_Year
fit=lm(Item_Outlet_Sales~ ., data=train)
prediction <- predict(fit, newdata = test)
submit=data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Indentifier
                     ,Item_Outlet_Sales = prediction)
write.csv(submit, file = "salesprediction.csv", row.names = FALSE)