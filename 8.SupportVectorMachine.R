library(dplyr)
library(tidyr)
library(ggplot2)
library("rpart")
library("partykit")
library("rpart.plot")
library("tidymodels")
library("ranger")
library("doParallel")
library("vip")
library("skimr")
library("corrplot")
library("ggridges")
library("themis")
library("knitr")
library("xgboost")
library("e1071")


Results <- as.data.frame(read.csv("Filtered_Results.csv"))
Results$DiscretionaryAccrualsBinary <- as.factor(Results$DiscretionaryAccrualsBinary)
Results$DiscretionaryAccrualsBinary <- factor(Results$DiscretionaryAccrualsBinary, levels = c("1", "0"))
str(Results$DiscretionaryAccrualsBinary)


set.seed(912340)
Results_split <- initial_split(data = Results, prop = 0.7, 
                               strata = DiscretionaryAccrualsBinary)


Results_train <- training(Results_split)
Results_test <- testing(Results_split)

set.seed(12345678)
cv_folds <- Results_train %>% vfold_cv(v = 10, strata = DiscretionaryAccrualsBinary)





svm_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary) 

svm_model_tune <- svm(cost = tune(), gamma = tune()) %>%
  set_mode("classification") %>%
  set_engine("e1071")



svm_tune_wf <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_model_tune)

class_metrics <- metric_set(accuracy, sensitivity, specificity, roc_auc)


svm_tune_res <- tune_grid(
  svm_tune_wf,
  resamples = cv_folds,
  grid = expand.grid(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1)),
  metrics = class_metrics
)