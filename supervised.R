library(dplyr)
library(ggplot2)
library(reshape)
library(pROC)
library(caret)
library(MASS)
library(randomForest)


###### CORRELATION MARTIX

#at first, I select only numerical variables
corr_data <- select_if(data, is.numeric)
#I remove id and year_birth, binary variables and engineered ones
corr_data <- dplyr::select(corr_data, -1, -2, -19, -20, -21, -22, -23, -27, -28)

#Correlation matrix and heatmap visualization
correlation_matrix <- cor(corr_data)
heat_corr <- melt(correlation_matrix)
corr_graph <- ggplot(heat_corr, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
corr_graph



########### DIFFERENT SUPERVISED METHODS COMPARISON

#### DATASET CREATION: excluding useless variables, scaling, factoring the categorical ones

#remove id, year birth and country
data$Cluster <- kmeans_tsne$cluster
sup_data <- dplyr::select(data, -1,-2, -28)

#scaling numerical variables
toscale <- c(3:18, 26)
sup_data[, toscale] <- scale(sup_data[,toscale])

#now I have sup_data with  categoricals and scaled numericals, I remove engineered
sup_data <- dplyr::select(sup_data, -27, -28)
sup_data$Cluster <- as.factor(sup_data$Cluster)
sup_data$education <- as.factor(sup_data$education)
sup_data$marital_status <- as.factor(sup_data$marital_status)
sup_data$acceptedcmp1 <- as.factor(sup_data$acceptedcmp1)
sup_data$acceptedcmp2 <- as.factor(sup_data$acceptedcmp2)
sup_data$acceptedcmp3 <- as.factor(sup_data$acceptedcmp3)
sup_data$acceptedcmp4 <- as.factor(sup_data$acceptedcmp4)
sup_data$acceptedcmp5 <- as.factor(sup_data$acceptedcmp5)
sup_data$response <- as.factor(sup_data$response)
sup_data$complain <- as.factor(sup_data$complain)
#now I split sup_data in 80% training set and 20% test set
set.seed(101)
partition <- createDataPartition(sup_data$response, p=0.8, list = FALSE) 
train_sup <- sup_data[partition,]
test_sup <- sup_data[-partition,]


### 1. LOGISTIC REGRESSION


log_reg <- glm(response~., data=train_sup, family="binomial")
summary(log_reg)

#AUC to determine classifier quality
test_prob_LR = predict(log_reg, newdata = test_sup, type = "response")
test_roc_LR = roc(test_sup$response ~ test_prob_LR, plot = TRUE, print.auc = TRUE)
auc_LR <- 0.901

# predict classes from modeland compute confusion matrix
predicted_classes_LR <- ifelse(predict(log_reg, type = "response") > 0.5, 1, 0)
conf_matrix_LR <- confusionMatrix(as.factor(predicted_classes_LR), as.factor(train_sup$response))

#measures of evaluation
recall_LR <- conf_matrix_LR$byClass["Sensitivity"]
specificity_LR <- conf_matrix_LR$byClass["Specificity"]
precision_LR <- conf_matrix_LR$byClass["Pos Pred Value"]
f1_score_LR <- 2 * (precision_LR * recall_LR) / (precision_LR + recall_LR)
accuracy_LR <- conf_matrix_LR$overall["Accuracy"]



### 2. LOGISTIC REGRESSION - FORWARD SELECTION

#forward stepwise selection on full logistic model
log_reg_FWD <- stepAIC(log_reg, direction = "both", trace = T)
summary(log_reg_FWD)

#AUC to determine classifier quality
test_prob_FWD = predict(log_reg_FWD, newdata = test_sup, type = "response")
test_roc_FWD = roc(test_sup$response ~ test_prob_FWD, plot = TRUE, print.auc = TRUE)
auc_LR_FWD <- 0.895

# predict classes from modeland compute confusion matrix
predicted_classes_LR_FWD <- round(test_prob_FWD)
conf_matrix_LR_FWD <- confusionMatrix(data = factor(predicted_classes_LR_FWD, levels = c("0", "1")),
                                      reference = as.factor(test_sup$response),
                                      positive = "1")

#measures of evaluation
recall_LR_FWD <- conf_matrix_LR_FWD$byClass["Sensitivity"]
specificity_LR_FWD <- conf_matrix_LR_FWD$byClass["Specificity"]
precision_LR_FWD <- conf_matrix_LR_FWD$byClass["Pos Pred Value"]
f1_score_LR_FWD <- 2 * (precision_LR_FWD * recall_LR_FWD) / (precision_LR_FWD + recall_LR_FWD)
accuracy_LR_FWD <- conf_matrix_LR_FWD$overall["Accuracy"]


### 3. RANDOM FOREST

# Fit Random Forest model
set.seed(123)
#train_sup$response <- factor(train_sup$response, levels = 0:1, labels = c("0", "1"))
#test_sup$response <- factor(test_sup$response, levels = 0:1, labels = c("0", "1"))

ran_for <- randomForest(response ~ ., data = train_sup, ntree = 500)
summary(ran_for)
#AUC to determine classifier quality
library(randomForest)
test_prob_RF <- predict(ran_for, newdata = test_sup, type = "prob")
test_roc_RF = roc(test_sup$response ~ test_prob_RF[,"1"], plot = TRUE, print.auc = TRUE)
auc_RF <- 0.903

#confusion matrix
predictions_prob_RF <- predict(ran_for, newdata = test_sup, type = "prob")[, "1"]
predictions_class_RF <- ifelse(predictions_prob_RF > 0.5, 1, 0)
conf_matrix_RF <- confusionMatrix(as.factor(predictions_class_RF), as.factor(test_sup$response))

#measures of evaluation
recall_RF <- conf_matrix_RF$byClass["Sensitivity"]
specificity_RF <- conf_matrix_RF$byClass["Specificity"]
precision_RF <- conf_matrix_RF$byClass["Pos Pred Value"]
f1_score_RF <- 2 * (precision_RF * recall_RF) / (precision_RF + recall_RF)
accuracy_RF <- conf_matrix_RF$overall["Accuracy"]


#importance of the variables
importance(ran_for)

plot_varImp <- varImpPlot(ran_for, sort=TRUE, main = "Variable importance")



