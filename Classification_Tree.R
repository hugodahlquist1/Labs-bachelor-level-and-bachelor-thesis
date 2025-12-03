library(ISLR)
library(tree)
library(caret)

data("Carseats")
names(Carseats)
summary(Carseats)

set.seed(0803)

str(Carseats)

#Divide the data into test and training

n <- length(Carseats$Sales)
nt <- round(0.75 * n)
train <- sample(1:n, nt)

data_train <- Carseats[train,]
data_test <- Carseats[-train,]

#Start computing the tree:

tree_train <- tree(Sales ~ .,data_train)
summary(tree_train)

#Plot

plot(tree_train)

text(tree_train , col = "blue", cex = 0.5, adj = c(0.5,0))

#Cross-validation:

cv_train <- cv.tree(tree_train , K =10)
cv_train
plot(cv_train$size, cv_train$dev, type = "b",
     xlab = "Number of Terminal Nodes", 
     ylab = "Deviance",
     main = "Cross-validation for Carseats Data")



optimal_index <- which.min(cv_train$dev)

optimal_nodes <- cv_train$size[optimal_index]
optimal_nodes

plot(cv_train$size, cv_train$dev, type = "b", 
     xlab = "Number of Terminal Nodes", ylab = "Deviance",
     main = "Cross-validation for Carseats Data")
points(cv_train$size[optimal_index], cv_train$dev[optimal_index], col = "red", cex = 1.5, pch = 20)

#Compute training predictions

pred_train <- predict (tree_train, newdata = data_train)
mse_train <- mean((pred_train - data_train$Sales)^2)
mse_train

#Compute test predictions

pred_test <- predict(tree_train, newdata = data_test)
mse_test <- mean((pred_test - data_test$Sales)^2)
mse_test 

#Pruning

pruning_train <- prune.tree(tree_train,best = 13)
summary(pruning_train)

plot(pruning_train)
text(pruning_train ,col = "blue", cex = 0.5, adj = c(0.5,0))

pred_train_pruning <- predict(pruning_train, newdata = data_train)

mse_train_pruning <- mean((pred_train_pruning - data_train$Sales)^2)
mse_train_pruning

#Compute test predictions

pred_test_pruning <- predict(pruning_train, newdata = data_test)

mse_test_pruning <- mean((pred_test_pruning - data_test$Sales)^2)
mse_test_pruning


