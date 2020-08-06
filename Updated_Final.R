# Machine Learning packages
library(caret)
library(nnet)
# library(mlbench)
# library(Boruta)
# library(leapp)
library(GGally)
library(tidyverse)
# RF
library(rattle)
#ANN
library(NeuralNetTools)
# Classification
library(AppliedPredictiveModeling)
## Loading DMwr to balance the unbalanced class
library(DMwR)
# rpart plot
library(rattle)
library(rpart)
library(neuralnet)
library(ggrepel)



#############################################
'2020 Predictions using Machine Learing'
#############################################

'We are reloading the big_5.csv in case there are users that simply want to run the ML portion of this project. 
Big_five data should be in your Global Environment if you have ran everything before this'

# Load Data
five <- read.csv("big_5.csv", sep = ",")
head(five, 15)

# replace NAs with 0
five[is.na(five)] <- 0

# load test data set. This is the data set with the canidates from 2020
test_data2020 <- read.csv("test.csv", sep = ",")


#############################################
'Binary Classification'
#############################################

# The target variable is "Rank"
c_model <- five[,c(2, 4, 12, 13, 14, 15, 16, 20,21,  22, 23, 25, 26 , 36, 3)]

# Turn Rank into factor class
c_model$Rank <- as.factor(c_model$Rank)

# Add a column "Results" with yes or no answer
c_model <- c_model %>%
  mutate(Results = ifelse(Rank == 1, "MVP", "Not_MVP")) #%>%

# Make Results column a factor type
c_model$Results <- as.factor(c_model$Results)

# Create binary c_model
c_model2 <- c_model %>%
  # Include all columns besides Rank
  select(-Rank)

set.seed(4321)
# create a single 75/25% split of the Results data:
trainIndex <- createDataPartition(c_model2$Results, p = .75, list = FALSE, times = 1)

# Training and Testing data
Train_C2 <- c_model2[ trainIndex,]
Test_C2  <- c_model2[-trainIndex,]


set.seed(4321)
# Logisitic - binary
c_logistic_model2 <- glm(Results ~ ., data = Train_C2[,3:15], family = binomial)
                           
set.seed(4321)
# decision tree model
c_tree_model2 <- rpart(Results ~ ., data = Train_C2[,3:15])

set.seed(4321)
# ANN model
c_ann_model2 <- neuralnet(Results ~ ., data = Train_C2[,3:15], hidden = c(2,1))
          
# Apply model to make prediction on Testing set
logistic_pred_2 <-ifelse(round(predict(c_logistic_model2, Test_C2[,-15], type = "response")) == 0,"MVP","Not_MVP")
tree_pred_2 <-predict(c_tree_model2, Test_C2[,-15], type = "class") 
ann_pred_2 <-ifelse(round(predict(c_ann_model2, Test_C2[,-15], type ="t") [,2]) == 0,"MVP","Not_MVP")

# Model performance (Displays confusion matrix and statistics)
logistic_results_2 <- confusionMatrix(factor(logistic_pred_2), Test_C2$Results,  positive = "MVP")
tree_results_2 <- confusionMatrix(tree_pred_2, Test_C2$Results, positive = "MVP")
ann_results_2 <- confusionMatrix(factor(ann_pred_2), Test_C2$Results,  positive = "MVP")

# Results on binary classification
logistic_results_2
tree_results_2
ann_results_2



'Now doing the UPGRADED BINARY CLASSIFICATION MODELS'

# SMOTE
over_2 <- SMOTE(Results ~., Train_C2, perc.over = 400, perc.under = 125,  k = 5)
over_2$Results <- factor(over_2$Results, levels = c("MVP", "Not_MVP"))

# Check target variable's distribution
table(over_2$Results)

# fit control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(4321)
# logistic
upgraded_c_logistic_model2 <- train(Results ~ ., data = over_2[,3:15], 
                                    method = "glmboost", 
                                    trControl = fitControl)

set.seed(4321)
## Decision Tree
upgraded_c_tree_model2 <- train(Results ~ ., data = over_2[,3:15],
                                method = "rpart",
                                tuneLength = 10,
                                trControl = fitControl)

# decay is the weight decay, and there are three tuning values. size is the number of hidden units.
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
set.seed(4321)
## ANN model
upgraded_c_ann_model2 <- train(Results ~ ., data = over_2[,3:15],
                               method = "nnet",
                               trControl = fitControl,
                               preProcess = c('center', 'scale'),
                               tuneGrid = nnetGrid)

## Make prediction
upgraded_logistic_pred_2 <-predict(upgraded_c_logistic_model2, Test_C2[,-15])
upgraded_tree_pred_2 <-predict(upgraded_c_tree_model2, Test_C2[,-15])
upgraded_ann_pred_2 <-predict(upgraded_c_ann_model2, Test_C2[,-15]) 


# Results
upgraded_logistic_results_2 <- confusionMatrix(upgraded_logistic_pred_2, Test_C2$Results)
upgraded_tree_results_2 <- confusionMatrix(upgraded_tree_pred_2, Test_C2$Results,  positive = "MVP")
upgraded_ann_results_2 <- confusionMatrix(upgraded_ann_pred_2, Test_C2$Results,  positive = "MVP")


# Results of updated binary classification model
upgraded_logistic_results_2
upgraded_tree_results_2
upgraded_ann_results_2


logistic_results_2
upgraded_logistic_results_2


tree_results_2
upgraded_tree_results_2

ann_results_2
upgraded_ann_results_2





#############################################
'Multiclass Classification'
#############################################

set.seed(4321)
trainIndex <- createDataPartition(c_model$Rank, p = .75, list = FALSE, times = 1)
# Include everything besides the "Results" Column
c5 <- c_model[,1:15]

# Training and Testing data
Train_C5 <- c5[ trainIndex,]
Test_C5  <- c5[-trainIndex,]

# Build Training model
set.seed(4321)
c_log_model5 <- multinom(Rank ~ ., data = Train_C5[,3:15])

set.seed(4321)
c_tree_model5 <- rpart(Rank ~ ., data = Train_C5[,3:15])

set.seed(4321)
c_ann_model5 <- nnet(Rank ~  ., data = Train_C5[,3:15], size = 1)

# Apply model to make prediction on Testing set
log_pred_5 <-predict(c_log_model5, Test_C5[,-15], type = "class")
tree_pred_5 <-predict(c_tree_model5, Test_C5[,-15], type = "class")
ann_pred_5 <- predict(c_ann_model5, Test_C5[,-15], type = "class")

# modify ann to factor
ann_pred_5 <- factor(ann_pred_5, levels = c_ann_model5$lev)
str(ann_pred_5)

# Model performance (Displays confusion matrix and statistics)
log_results_5 <- confusionMatrix(log_pred_5, Test_C5$Rank)
tree_results_5 <- confusionMatrix(tree_pred_5, Test_C5$Rank)
ann_results_5 <- confusionMatrix(ann_pred_5, Test_C5$Rank)

log_results_5
tree_results_5
ann_results_5


# Upgraded multiclass Classification
set.seed(4321)
# create a single 75/25% split of the c_model data:
trainIndex <- createDataPartition(c5$Rank, p = .75, list = FALSE, times = 1)

Train_C5 <- c5[ trainIndex,]
Test_C5  <- c5[-trainIndex,]


# SMOTE
set.seed(4321)
over_5 <- SMOTE(Rank ~., Train_C5, perc.over = 200,perc.under = 420, k = 5)

# Check target variable's distribution
table(over_5$Rank)

set.seed(4321)
# Build Training model
upgraded_c_log_model5 <- train(Rank ~  ., data = over_5[,3:15],
                               method = 'multinom',
                               trControl = fitControl)

set.seed(4321)
upgraded_c_tree_model5 <- train(Rank ~ ., data = over_5[,3:15],
                                method = "ctree",
                                trControl = fitControl)

# decay is the weight decay, and there are three tuning values. size is the number of hidden units.
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
set.seed(4321)
# ANN model
upgraded_c_ann_model5 <- train(Rank ~  ., data = over_5[,3:15],
                               method = "nnet",
                               preProcess = c('center', 'scale'),
                               tuneGrid = nnetGrid,
                               trControl = fitControl)



# Apply model to make prediction on Testing set
upgraded_log_pred_5 <-predict(upgraded_c_log_model5, Test_C5[,-15])
upgraded_tree_pred_5 <-predict(upgraded_c_tree_model5, Test_C5[,-15])
upgraded_ann_pred_5 <-predict(upgraded_c_ann_model5, Test_C5[,-15]) 

# Model performance (Displays confusion matrix and statistics)
upgraded_log_results_5 <- confusionMatrix(upgraded_log_pred_5, Test_C5$Rank)
upgraded_tree_results_5 <- confusionMatrix(upgraded_tree_pred_5, Test_C5$Rank)
upgraded_ann_results_5 <- confusionMatrix(upgraded_ann_pred_5, Test_C5$Rank)

# See updated results
log_results_5
upgraded_log_results_5

tree_results_5
upgraded_tree_results_5

ann_results_5
upgraded_ann_results_5


#############################################
           'Regression Tasks'
############################################


# Target variable is "Pts Won"
r_data <- five[,c(2, 4, 12, 13, 14, 15, 16, 20,21,  22, 23, 25, 26 , 36, 7)]
colnames(r_data)[15] <- c("Points_Won")

# create a single 75%/25% split of the NBA data:
trainIndex <- createDataPartition(r_data$Points_Won, p = .75, list = FALSE, times = 1)

Train_r <- r_data[ trainIndex,]
Test_r  <- r_data[-trainIndex,]

set.seed(4321)
# create MLR model
r_mlr_model <- lm(Points_Won ~ WS + W, data = Train_r)

set.seed(4321)
# Decision Tree model
r_tree_model <- rpart(Points_Won ~., data = Train_r[,3:15])

set.seed(4321)
# ANN model
# You need to add ' linout = TRUE' to make it a regression model or else you'll only get 1 for an output
r_ann_model <- nnet(Points_Won ~ ., data = Train_r[,3:15], size = 65, linout= TRUE)


# Creating a dataframe with all our results
reg_results <- NULL
reg_results$Year <- Test_r$Year
reg_results$Player <- Test_r$Player
reg_results$pred_mlr <- predict(r_mlr_model, Test_r[-15])
reg_results$pred_tree <- predict(r_tree_model, Test_r[, -15])
reg_results$pred_ann <- predict(r_ann_model, Test_r[, -15])
reg_results <-as.data.frame(reg_results)
reg_results$Actual <- Test_r$Points_Won

reg_results

# Round data frame
reg_results$pred_mlr <- round(reg_results$pred_mlr, 0)
reg_results$pred_tree <- round(reg_results$pred_tree, 0)
reg_results$pred_ann <- round(reg_results$pred_ann, 0)


'          Upgraded Regression Models    ' 
set.seed(4321)
# create a single 75%/25% split of the NBA data:
trainIndex <- createDataPartition(r_data$Points_Won, p = .75, list = FALSE, times = 1)

Train_r <- r_data[ trainIndex,]
Test_r  <- r_data[-trainIndex,]

# fit control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Make a custom tuning grid
tuneGrid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1,
                                                  length = 10))

set.seed(4321)
upgraded_r_mlr_model <- train(Points_Won ~., data = Train_r[,3:15],
                              method = "glmnet",
                              trControl = fitControl,
                              tuneGrid = tuneGrid)

set.seed(4321)
# Decision Tree model
upgraded_r_tree_model <- train(Points_Won ~., data = Train_r[,3:15],
                               method = "rpart",
                               trControl = fitControl)


# Make a custom tuning grid
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.0001, to = 0.5, by = 0.1))

set.seed(4321)
# ANN model
# You need to add ' linout = 1' to make it a regression model or else you'll only get 1 for an output
upgraded_r_ann_model <- train(Points_Won ~ ., data = Train_r[,3:15],
                              method = "nnet",
                              trControl = fitControl,
                              linout = 1,
                              tuneGrid = nnetGrid,
                              preProcess = c('center', 'scale'))


# Creating a dataframe for all our upgraded results
upgraded_reg_results <- NULL
upgraded_reg_results$Year <- Test_r$Year
upgraded_reg_results$Player <- Test_r$Player
upgraded_reg_results$pred_mlr <- predict(upgraded_r_mlr_model, Test_r[-15])
upgraded_reg_results$pred_ann <- predict(r_ann_model, Test_r[, -15])
upgraded_reg_results$pred_tree <- predict(r_tree_model, Test_r[, -15])
upgraded_reg_results <-as.data.frame(upgraded_reg_results)
upgraded_reg_results$Actual <- Test_r$Points_Won


# MLR Results
MLR_Results <- data.frame(
  Basic = postResample(reg_results$pred_mlr, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_mlr, upgraded_reg_results$Actual)
)


# Trees Results
Tree_Results <- data.frame(
  Basic = postResample(reg_results$pred_tree, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_tree, upgraded_reg_results$Actual)
)


# ANN Results
ANN_Results <- data.frame(
  Basic = postResample(reg_results$pred_ann, reg_results$Actual),
  Upgraded = postResample(upgraded_reg_results$pred_ann, upgraded_reg_results$Actual)
)




###################################################################
' Predicting on 2020 Data'
###################################################################

# Create testing data set with the 2020 candiates stats
Final_Test <- test_data2020[,c("Year","Player","PTS","TRB","AST","STL", "BLK","WS","WS48" ,"Seed","W","PER", "TS.","Sum")]


final_binary <- data.frame(probs = predict(upgraded_c_ann_model2, Final_Test, type="prob"))
final_multi <- data.frame(probs = predict(upgraded_c_log_model5, Final_Test, type="prob"))
final_regression <- predict(upgraded_r_mlr_model, Final_Test)



#############################################
'Data Viz'
#############################################
reg_long <- upgraded_reg_results %>%
  pivot_longer(-c(Year, Player, Actual), names_to = "Model", values_to = "Prediction")

reg_long <- reg_long[,c (1, 2,4, 5, 3)]

# Graph to show our estimates of our upgraded regression models
p <- ggplot(reg_long, aes(x = Actual, y = Prediction, color = Model, fill = Model)) + 
  geom_point( aes(x = Actual, y = Actual))  +
  geom_line(size=1, alpha = 0.7) +
  theme_classic() + 
  xlim(0, 1250) +
  ylim(0, 1250) 

p


# Create long verison of c_model2 
c_long <- c_model %>% 
  pivot_longer(-c(Year, Player, Results, Rank), names_to = "Statistic", values_to = "Value")

# You can switch 'y=value' to x=value to switch graph look. Just personal preference 
ggplot(c_long, aes(y=Value, fill = Results, color = Results)) + 
  geom_boxplot(col=4, alpha = .55) +
  facet_wrap(~Statistic, scales = "free") +
  theme_classic()


# You can switch 'y=value' to x=value to switch graph look. Just personal preference 
ggplot(c_long, aes(y=Value, fill = Rank, color = Rank)) + 
  geom_boxplot(col=4, alpha = .55) +
  facet_wrap(~Statistic, scales = "free") +
  theme_classic()


# bar plot of the normal binary data
bar <- ggplot(c_model2, aes(x = Results, fill = Results)) + 
  geom_bar() +
  theme_classic() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
bar

# b2
bar2 <- ggplot(over_2, aes(x = Results, fill = Results)) + 
  geom_bar() +
  theme_classic() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

bar2

# b5
bar5 <- ggplot(over_5, aes(x = Rank, fill = Rank)) + 
  geom_bar() +
  theme_classic()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

bar5

# Trees !!
library(rpart.plot)
# Binary
rpart.plot(c_tree_model2, main = "Basic Binary")
rpart.plot(upgraded_c_tree_model2$finalModel, main = "Upgraded Binary")

# Multiclass
fancyRpartPlot(c_tree_model5, cex = .75, main = "Basic Multiclass")
plot(upgraded_c_tree_model5$finalModel, cex = .75,  main = "Upgraded Multiclass")

# Regression
rpart.plot(r_tree_model, main = "Basic Regression")
rpart.plot(upgraded_r_tree_model$finalModel, main = "Upgraded Regression")




binary_predicted.data <- data.frame(
  Player = paste(Test_C2$Player, Test_C2$Year),
  probability.of.mvp = logistic_pred_2,
  mvp = Test_C2$Results
)


binary_predicted.data <- binary_predicted.data[
  order(binary_predicted.data$probability.of.mvp, decreasing= FALSE),]
binary_predicted.data$rank <- 1:nrow(binary_predicted.data)



## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
logistic_training_plot2 <- ggplot(data=binary_predicted.data, aes(x=rank, y=probability.of.mvp)) +
  geom_point(aes(color=mvp), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of winning MVP") 


logistic_training_plot2 + 
  geom_label_repel(mapping = aes(label = Player), nudge_y = .4) +
  theme_classic() +
  labs(title = "Basic Logistic Regression Predictions")


' UPGRADED BINARY LOGISTIC PLOT SORRY FOR THE LONG VARIABLE NAMES'

up_binary_predicted.data <- data.frame(
  Player = paste(Test_C2$Player, Test_C2$Year),
  probability.of.mvp = upgraded_logistic_pred_2,
  mvp = Test_C2$Results
)


up_binary_predicted.data <- up_binary_predicted.data[
  order(up_binary_predicted.data$probability.of.mvp, decreasing= FALSE),]
up_binary_predicted.data$rank <- 1:nrow(up_binary_predicted.data)



## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
up_logistic_training_plot2 <- ggplot(data = up_binary_predicted.data, aes(x = rank, y = probability.of.mvp)) +
  geom_point(aes(color=mvp), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of winning MVP") +
  labs(title = "Upgraded Logistic Regression Predictions")


up_logistic_training_plot2 + 
  geom_label_repel(mapping = aes(label = Player), nudge_y = .4) +
  theme_classic()






