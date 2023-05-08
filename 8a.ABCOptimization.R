library(abc)
library(ABCoptim)
library(e1071)
library(caret)
library(caret)

# Load the data
data <- as.data.frame(read.csv("Filtered_Results.csv"))
data <- data[,4:34]



# # Define the objective function to maximize
fit_func <- function(x) {
  # Split the data into training and testing sets
  set.seed(123)
  train_index <- createDataPartition(data$DiscretionaryAccrualsBinary, p = 0.7, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Train a logistic regression model on the training data
  model <- glm(DiscretionaryAccrualsBinary ~ ., data = train_data, family = binomial())
  
  # Predict the labels for the testing data
  preds <- predict(model, newdata = test_data, type = "response")
  
  # Convert predicted probabilities to binary labels
  preds <- ifelse(preds >= 0.5, 1, 0)
  
  # Compute the accuracy of the model
  accuracy <- mean(preds == test_data$DiscretionaryAccrualsBinary)
  
  # Return the accuracy as the final metric
  return(accuracy)
}



# Set the number of decision variables (features), Set the number of bees and iterations
n_dim <- 30
n_bees <- 15
n_iter <- 100

# Run the ABC algorithm to select the best 10 features
result <- abc_optim(par = rep(1, n_dim), fn = fit_func, FoodNumber = n_bees,
                    maxCycle = n_iter, lb = rep(0, n_dim), ub = rep(1, n_dim))



Foods <- as.data.frame(result[["Foods"]])


# assuming abc_optim returns a list with the item 'foods'
foods <- result$Foods

# calculate the importance scores for each feature
importance_scores <- apply(foods, 2, mean)

# create a data frame with features and their importance scores
df <- data.frame(features = colnames(data[1:30]), importance = importance_scores)

# sort the data frame by importance scores in descending order
df <- df[order(-df$importance), ]

# select the top 10 most important features
top10_features <- df$features[1:10]
top10_features


