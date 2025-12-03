library(car)
library(neuralnet)
library(dplyr)
library(caret)
library(ggplot2)
library(lattice)
library(dplyr)
library(tidyverse)
library(keras)
library(knitr)
library(kableExtra)

dataset_amazon <- read_delim("/Users/jonasdahlquist/Desktop/R/Uppsats/Dataset_Amazon.csv")
dataset_amazon$Year <- as.numeric(format(dataset_amazon$Date, "%Y"))

dataset_amazon <- dataset_amazon %>% 
  mutate(DummyVariable = ifelse(lag(ClosingPrice) < ClosingPrice, 1, 0))
dataset_amazon <- dataset_amazon %>% 
  mutate(lag = lag(DummyVariable))

dataset_amazon <- dataset_amazon[-c(2519:2530), ]
dataset_amazon <- dataset_amazon[-c(1,2), ]

#Studera alla variabler och se ifall de är numeriska
str(dataset_amazon)

#Korrigera Växelkursen och gör den variabeln numeric
dataset_amazon$USDVäxelkurs<-as.numeric(dataset_amazon$USDVäxelkurs)
dataset_amazon$Debt<-as.numeric(dataset_amazon$Debt)
dataset_amazon$Cashflow<-as.numeric(dataset_amazon$Cashflow)
dataset_amazon$EPS<-as.numeric(dataset_amazon$EPS)
dataset_amazon$`P/E`<-as.numeric(dataset_amazon$`P/E`)

#Gör nytt dataset med enbart numeriska variabler
numeric_dataset_amazon <- dataset_amazon %>% 
  select_if(is.numeric)

# Ersätt NA-värden med medelvärdet för varje kolumn
numeric_dataset_amazon <- as.data.frame(numeric_dataset_amazon)

# Tar bort DEBT, för många NAs

numeric_dataset_amazon <- numeric_dataset_amazon %>%
  select(!c(AdjClose, ClosingPrice, Debt))

numeric_dataset_amazon <- numeric_dataset_amazon %>%
  select(!c(DummyVariable))

remaining_na_cols <- colnames(numeric_dataset_amazon)[colSums(is.na(numeric_dataset_amazon)) > 0]

dataset <- na.omit(numeric_dataset_amazon)

###Delar in datan i tränings -, validerings- och testdata

index_train <- dataset$Year == 2020
table(index_train)
test_df <- dataset[index_train,]
train_df <- dataset[-index_train,]

train_df <- train_df %>%
  select(!c(Year))
test_df <- test_df %>%
  select(!c(Year))


set.seed(123)

###Bygga modellen och applicera den på träningsdatan:

#Sepparera på respons- och prediktorvaribler för Träningsdatan och testdatan

y_test <- test_df$lag
predictor_variables_test <- test_df[, -20]
scaled_predictors_test <- scale(predictor_variables_test)

y_train <- train_df$lag
predictor_variables_train <- train_df[, -20]
scaled_predictors_train <- scale(predictor_variables_train)


#Bygg model med hjälp av keras

model <- keras_model_sequential()

#Lägger in lager i modellen


model %>%
  layer_dense(units = 400, activation = "relu", input_shape = ncol(scaled_predictors_train)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 300, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 200, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 100, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 1, activation = "sigmoid", kernel_regularizer = regularizer_l2(0.001))
#Samanställer modellen

model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error",
  metrics = c('accuracy')
)

summary(model)

#Gör om till matricer 

predictor_matrix_train <- as.matrix(scaled_predictors_train)

#Träna modellen, justera och testa olika epoker och batch sizes som ska tränas!

history <- model %>% fit(
  x = predictor_matrix_train,
  y = y_train,
  epochs = 300,
  batch_size = 1100,
  validation_split = 0.2
)


plot(history)

# Evaluera modellen på träningsdata
model %>% evaluate(predictor_matrix_train, y_train)

# Scale the predictor variables in the test data
scaled_predictors_test <- scale(predictor_variables_test)

# Convert the scaled predictors to a matrix
predictor_matrix_test <- as.matrix(scaled_predictors_test)

# Make predictions on the test data
predictions <- model %>% predict(predictor_matrix_test)

# Convert predictions to binary (0 or 1) based on a threshold
threshold <- 0.5

binary_predictions <- ifelse(predictions > threshold, 1, 0)


comparison <- data.frame(Actual = y_test, Predicted = binary_predictions)

y_test <- factor(y_test, levels = c(0, 1))

binary_predictions <- factor(binary_predictions, levels = c(0, 1))

# Create a confusion matrix
conf_matrix <- confusionMatrix(binary_predictions, y_test)

# Print the confusion matrix
print(conf_matrix)

#Utvärdera modellen:
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("F1 Score:", f1_score, "\n")



