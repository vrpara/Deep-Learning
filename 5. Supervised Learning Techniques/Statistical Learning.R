data <- read.csv("Dataset.csv")
#View(data)

### No Missing Data, so no statistics to deal with it
### Split dataset for prediction and dataset(train/test) for model building 
data_for_prediction <- data[1:10, ]
#View(data_for_prediction)
data_for_model <- data[11:nrow(data), ]
#View(data_for_prediction)
#View(data_for_model)
Property2_yes_1<-replace(data_for_model$Property2, data_for_model$Property2 == "yes", as.numeric(1))  # replacing yes/no for property2 with 1 and 0
Property2_yes_1_no_0<-(replace(Property2_yes_1, Property2_yes_1 == "no", as.numeric(0)))   # replacing yes/no for property2 with 1 and 0
#View(Property2_yes_1_no_0)
data_for_model <- data_for_model[,-2]
data_for_model=data.frame(data_for_model,Property2_yes_1_no_0)
names(data_for_model)[names(data_for_model) == "Property2_yes_1_no_0"] <- "Property2"
colnames(data_for_model)
View(data_for_model)
############## 1. BUILDING A MODEL ##############

############## 1.1 Feature Engineering ############## 
############## 1.1.a Based on correlation ##############
sapply(data_for_model, mode)
data_for_model_transformed<-transform(data_for_model, Property2 = as.numeric(Property2))

matrix <- cor(data_for_model_transformed)
View(matrix)
## notes ##
# Property1 Least correlated Features that can be removed- 1 4 3 2 
# High correlated Features Pairs(ascending order)- 5/7, 6/8, 9/12, 
# Property2 Least correlated Features that can be removed- 10, 4, 2 
## end of notes ##
Model_Data_std<-as.data.frame(scale(data_for_model_transformed))
Model_Data_std_matrix<-data.matrix(Model_Data_std)
# View(Model_Data_std_matrix)
library(RColorBrewer)
my_group <- as.numeric(as.factor(substr(rownames(Model_Data_std_matrix), 1 , 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(Model_Data_std_matrix, scale="column" , RowSideColors=colSide, col=colMain)
heatmap




############## 1.1.b Based on VIP ############## 
data_for_model_VIP <- data_for_model_transformed #[,-c(1,14)]
#View(data_for_model_VIP)
Model_Data_std<-as.data.frame(scale(data_for_model_VIP))
pca1<-prcomp(Model_Data_std)
pca1$sdev/sum(pca1$sdev)
loads<-pca1$rotation
#View(loads)
scores<-pca1$x
#View(scores)
#Select number of PCs
plot(pca1$sdev)
var_explained = pca1$sdev^2 / sum(pca1$sdev^2)
print(var_explained)
library(ggplot2)
qplot(c(1:14), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
#VIP Calculation
#update loadings w/ reduced no. of PCs
loads_vip<-loads[,1:8]
property_vip_P1<-loads_vip[1,]
property_vip_P2<-loads_vip[14,]
features_vip<-loads_vip[2:13,]
weight_vip_P1<-property_vip_P1*features_vip
weight_vip_P2<-property_vip_P2*features_vip
#no. of weights should be equal to number of PCs included
vip_P1<-weight_vip_P1[,1]+weight_vip_P1[,2]+weight_vip_P1[,3]+weight_vip_P1[,4]+weight_vip_P1[,5]+weight_vip_P1[,6]+weight_vip_P1[,7]+weight_vip_P1[,8]
vip_P2<-weight_vip_P2[,1]+weight_vip_P2[,2]+weight_vip_P2[,3]+weight_vip_P2[,4]+weight_vip_P2[,5]+weight_vip_P2[,6]+weight_vip_P2[,7]+weight_vip_P2[,8]


barplot(vip_P1)
barplot(vip_P2)


############## 1.1.c Dealing with outliers ############## 

## Box plot before treating with outliers

data_Features <- data_for_model_transformed[,-c(1,14)]
boxplot(data_Features, col = rainbow(ncol(data_Features)))

library(tidyverse)

# Dealing with outliers
for (column in colnames(data_Features))
{
    section <- quantile(data_Features[,column], probs=c(.25, .75), na.rm = T)
    value <- quantile(data_Features[,column], probs=c(.05, .95), na.rm = T)
    zone <- 1.5 * IQR(data_Features[,column], na.rm = T)

    for (k in 1:nrow(data_Features))
    {
      if (data_Features[k, column] < (section[1] - zone))
      {
        data_Features[k, column] = value[1]
      }
      else if (data_Features[k, column] > (section[2] + zone))
      {
        data_Features[k, column] = value[2]
      }
    }
}

boxplot(data_Features, col = rainbow(ncol(data_Features)))

data_for_model_P1 <- data_for_model_transformed[,1]
data_for_model_P1 <- data.frame(data_Features,data_for_model_P1)
names(data_for_model_P1)[names(data_for_model_P1) == "data_for_model_P1"] <- "Property1"
#View(data_for_model_P1)
data_for_model_P2 <- data_for_model_transformed[,14]
data_for_model_P2 <- data.frame(data_Features,data_for_model_P2)
names(data_for_model_P2)[names(data_for_model_P2) == "data_for_model_P2"] <- "Property2"
#View(data_for_model_P2)

############## 1.2 Model Building Property1 ############## 
#Divide into training and testing data
set.seed(508)
data_partition_P1 = sample(1:nrow(data_for_model_P1), nrow(data_for_model_P1)*.85)
train_data_P1 <- data_for_model_P1[data_partition_P1,]
test_data_P1 <- data_for_model_P1[-data_partition_P1,]

############## 1.2.a SVR ############## 
library(e1071)
svr_fit <- svm(Property1 ~Feature5+Feature8+Feature9+Feature10+Feature11+Feature12, data=train_data_P1)
svr_predict_train <- predict(svr_fit,train_data_P1)
svr_predict_test <- predict(svr_fit,test_data_P1)
View(svr_predict_test)

## SVR Tuning##
tuned_svr <- tune(svm, Property1 ~Feature5+Feature8+Feature9+Feature10+Feature11+Feature12,  data = train_data_P1,ranges = list(epsilon = seq(0.3,0.5,0.01), cost = 2^(2:9)))
plot(tuned_svr)
svr_tunedModel <- tuned_svr$best.model

library(Metrics)
library(caret)


svr_predict_train <- predict(svr_tunedModel,train_data_P1)
svr_predict_test <- predict(svr_tunedModel,test_data_P1)
svr_RMSE_train = rmse(train_data_P1$Property1, as.numeric(svr_predict_train))
svr_RMSE_train

svr_RMSE_test = rmse(test_data_P1$Property1, as.numeric(svr_predict_test))
svr_RMSE_test

sst_train<-sum((train_data_P1$Property1-mean(train_data_P1$Property1))^2)
sse_train<-sum((svr_predict_train-train_data_P1$Property1)^2)
rsq_train<-1-sse_train/sst_train
rsq_train

sst_test<-sum((test_data_P1$Property1-mean(test_data_P1$Property1))^2)
sse_test<-sum((svr_predict_test-test_data_P1$Property1)^2)
rsq_test<-1-sse_test/sst_test
rsq_test

plot(train_data_P1$Property1,svr_predict_train,xlab="SVR Actual Train After Tuning",ylab="SVR Predicted Train After Tuning")
plot(test_data_P1$Property1,svr_predict_test,xlab="SVR Actual Test After Tuning",ylab="SVR Predicted Test After Tuning")

############## 1.2.b Lasso ############

install.packages("glmnet")
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
x_train <- data.matrix(train_data_P1[, c('Feature5', 'Feature8', 'Feature9', 'Feature10','Feature11','Feature12')])
y_train <- train_data_P1$Property1

x_test <- data.matrix(test_data_P1[, c('Feature5', 'Feature8', 'Feature9', 'Feature10','Feature11','Feature12')])
y_test <- test_data_P1$Property1

cv_model <- cv.glmnet(x_train, y_train, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
#produce plot of test MSE by lambda value
plot(cv_model) 
lasso_model <- glmnet(x_train, y_train, alpha = 0.5, lambda = best_lambda)
lasso_predict_train <- predict(lasso_model, s = best_lambda, newx = x_train)
lasso_predict_test <- predict(lasso_model, s = best_lambda, newx = x_test)

library(Metrics)
library(caret)

lasso_RMSE_train = rmse(train_data_P1$Property1, as.numeric(lasso_predict_train))
lasso_RMSE_train

lasso_RMSE_test = rmse(test_data_P1$Property1, as.numeric(lasso_predict_test))
lasso_RMSE_test

sst_train<-sum((train_data_P1$Property1-mean(train_data_P1$Property1))^2)
sse_train<-sum((lasso_RMSE_train-train_data_P1$Property1)^2)
rsq_train<-1-sse_train/sst_train
rsq_train

sst_test<-sum((test_data_P1$Property1-mean(test_data_P1$Property1))^2)
sse_test<-sum((lasso_RMSE_test-test_data_P1$Property1)^2)
rsq_test<-1-sse_test/sst_test
rsq_test

plot(train_data_P1$Property1,lasso_predict_train,xlab="LASSO Actual Train",ylab="LASSO Predicted Train")
plot(test_data_P1$Property1,lasso_predict_test,xlab="LASSO Actual Test",ylab="LASSO Predicted Test")

############## 1.2.c Ridge ############

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x_train, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x_train)
ridge_predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)

Ridge_RMSE_train = rmse(train_data_P1$Property1, as.numeric(ridge_predictions_train))
Ridge_RMSE_train

Ridge_RMSE_test = rmse(test_data_P1$Property1, as.numeric(ridge_predictions_test))
Ridge_RMSE_test

sst_train<-sum((train_data_P1$Property1-mean(train_data_P1$Property))^2)
sse_train<-sum((Ridge_RMSE_train-train_data_P1$Property1)^2)
rsq_train<-1-sse_train/sst_train
rsq_train

sst_test<-sum((test_data_P1$Property1-mean(test_data_P1$Property1))^2)
sse_test<-sum((Ridge_RMSE_test-test_data_P1$Property1)^2)
rsq_test<-1-sse_test/sst_test
rsq_test

plot(train_data_P1$Property1,ridge_predictions_train,xlab="Ridge Actual Train",ylab="Ridge Predicted Train")
plot(test_data_P1$Property1,ridge_predictions_test,xlab="Ridge Actual Test",ylab="Ridge Predicted Test")




############## 1.2.d GPR ############ 
install.packages("kernlab")
library(kernlab)
descriptors_train_gpr<-train_data_P1[,! names(train_data_P1) %in% c("Property1")]
descriptors_test_gpr<-test_data_P1[,! names(test_data_P1) %in% c("Property1")]
mdl_gpr<-gausspr(descriptors_train_gpr,train_data_P1$Property1)
pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_data_P1$Property1))
rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_data_P1$Property1))
rmse_gpr_train
rmse_gpr_test
sst<-sum((train_data_P1$Property1-mean(train_data_P1$Property1))^2)
sse<-sum((pred_train_gpr-train_data_P1$Property1)^2)
rsq<-1-sse/sst
rsq
sst_test<-sum((test_data_P1$Property1-mean(test_data_P1$Property1))^2)
sse_test<-sum((pred_test_gpr-test_data_P1$Property1)^2)
rsq_test<-1-sse_test/sst_test
rsq_test

plot(train_data_P1$Property1,pred_train_gpr,xlab="GPR Actual Train",ylab="GPR Predicted Train")
plot(test_data_P1$Property1,pred_test_gpr,xlab="GPR Actual Test",ylab="GPR Predicted Test")



############## 1.3 Model Building Property2 ##############
set.seed(508)
data_partition_P2 = sample(1:nrow(data_for_model_P2), nrow(data_for_model_P2)*.85)
train_data_P2 <- data_for_model_P2[data_partition_P2,]
test_data_P2 <- data_for_model_P2[-data_partition_P2,]
View(data_for_model_P2)
############## 1.3.1 SVM ############
library(e1071)
library(Metrics)
library(caret)
install.packages("cvms")
library(cvms)
library(tibble)


#Tuning
tune.out <- tune(svm, Property2~., data = train_data_P2, kernel = "radial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 0.7,0.9,1,5, 10, 100)))
(bestmod <- tune.out$best.model)

svmfit=svm(train_data_P2$Property2~.,data=train_data_P2,kernel ="radial", epsilon=0.1,gamma =0.08333333,cost =10)

svm.probs_train <- predict(svmfit,train_data_P2,type = "response")
svm.pred_train <- ifelse(svm.probs_train > 0.5, "1", "0")
basic_table <- table(Predict=svm.pred_train, truth = train_data_P2$Property2)
confusionMatrix(table(Predict=svm.pred_train, truth = train_data_P2$Property2))
fourfoldplot(basic_table, color = c("blue", "cyan"),
             conf.level = 0, margin = 1, main = "Train Data Confusion Matrix")

svm.probs_test <- predict(svmfit,newdata=test_data_P2,type = "response")
svm.pred_test <- ifelse(svm.probs_test > 0.5, "1", "0")
conf_mat_com <- tibble("target" = test_data_P2$Property2,"prediction" = svm.pred_test)
conf_mat <- confusion_matrix(targets = conf_mat_com$target,
                             predictions = conf_mat_com$prediction)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])


############## 1.3.2 Bagging ############
install.packages("rpart")
library(rpart)
fit<-rpart(classification~.,train_dt)
fit
rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE)
tree.pred_train=predict(fit,train_dt,type="class")
with(train_dt,table(tree.pred_train,classification))

confusionMatrix(table(Predict=tree.pred_train, truth = train_dt$classification))

tree.pred_test=predict(fit,test_dt,type="class")
with(test_dt,table(tree.pred_test,classification))

confusionMatrix(table(Predict=tree.pred_test, truth = test_dt$classification))

#test_data_P2$Property2

cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(Property2)~.,data=train_data_P2,method="treebag",trControl=cvcontrol,importance=TRUE,nbagg=50,minsplit=4,minbucket=2,maxdepth=3,cp=0)
train.bagg
plot(varImp(train.bagg))
tree.pred_train=predict(train.bagg,train_data_P2)
with(train_data_P2,table(tree.pred_train,Property2))
tree.pred_test=predict(train.bagg,test_data_P2)
with(test_data_P2,table(tree.pred_test,Property2))

############## 1.3.3 Random Forest with boosting ############

train.gbm <- train(as.factor(Property2) ~ ., data=train_data_P2,method="gbm",verbose=F,trControl=cvcontrol)
train.gbm
tree.pred_train=predict(train.gbm,train_data_P2)
with(train_data_P2,table(tree.pred_train,Property2))
tree.pred_test=predict(train.gbm,test_data_P2)
with(test_data_P2,table(tree.pred_test,Property2))
############## 1.3.4 Logistic Regression ############

#Develop Logistic Regression Model on the Training Data
glm.fit <- glm(train_data_P2$Property2 ~ ., data = train_data_P2, family = binomial)
glm.fit
glm.probs_train <- predict(glm.fit,train_data_P2,type = "response")
glm.pred_train <- ifelse(glm.probs_train > 0.5, "1", "0")
#Assess prediction result as table as yes / no accuracy
(misclass <- table(glm.pred_train, truth = train_data_P2$Property2))
#Apply model and repeat on training data
glm.probs_test <- predict(glm.fit,test_data_P2,type = "response")
glm.pred_test <- ifelse(glm.probs_test > 0.5, "1", "0")
(misclass <- table(glm.pred_test, truth = test_data_P2$Property2))




############## 2. PREDICTING UNKNOWNS ##############
############## 2.1 Property 1 Prediction ##############
data_predict_P1 <- data_for_prediction[,-(1:2)]
View(data_predict_P1)
############## 2.1.a SVR ############
svr_predict_test <- as.data.frame(predict(svr_tunedModel,data_predict_P1))
names(svr_predict_test)[names(svr_predict_test) == "predict(svr_tunedModel, data_predict_P1)"] <- "SVR- Predicted Property1"
View(svr_predict_test)

############## 2.1.b GPR ############
descriptors_test_gpr<-data_predict_P1[,! names(data_predict_P1) %in% c("Property1")]
pred_test_gpr<-as.data.frame(predict(mdl_gpr,descriptors_test_gpr))
names(pred_test_gpr)[names(pred_test_gpr) == "V1"] <- "GPR- Predicted Property1"
View(pred_test_gpr)
############## 2.2 Property 2 Prediction ##############
data_predict_P2 <- data_for_prediction[,-(1:2)]
View(data_predict_P2)
############## 2.2.a SVM ############
svm.probs_train <- as.data.frame(predict(svmfit,data_predict_P2,type = "response"))
svm.pred_train <- ifelse(svm.probs_train > 0.5, "Yes", "No")
colnames(svm.pred_train)[1] <- "SVM- Predicted Property2"
View(svm.pred_train)
############## 2.2.a Boosting ############
tree.pred_unknwon=as.data.frame(predict(train.gbm,data_predict_P2))
tree.pred_uknwn <- ifelse(tree.pred_unknwon == 1, "Yes", "No")
colnames(tree.pred_uknwn)[1] <- "Boosting- Predicted Property2"
View(tree.pred_uknwn)
