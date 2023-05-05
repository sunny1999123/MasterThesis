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

# Create a recipe
svm_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary)


# Create a model specification
#svm_spec <- svm_llinear(mode = "classification", cost = tune(), margin = tune())
# Create a workflow
svm_wflow <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_spec)

# Define the search grid for tuning
# svm_grid <- grid_latin_hypercube(
#   cost(),
#   margin(),
#   size = 20
# )
class_metrics <- metric_set(accuracy, sensitivity, 
                            specificity, roc_auc)



set.seed(12345678)
cv_folds <- Results_train %>% vfold_cv(v = 10, strata = DiscretionaryAccrualsBinary)


# Perform 10-fold cross-validation tuning
set.seed(123)
svm_tune_res <- tune_grid(
    svm_wflow, 
    resamples = cv_folds,
    grid = expand.grid(margin = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100"), cost = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100")),
    metrics = class_metrics,
    control = control_grid(verbose = TRUE)
  )

svm_tune_metrics <- svm_tune_res %>%
  collect_metrics()
svm_tune_metrics

svm_sens_spec <- svm_tune_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("sensitivity", "specificity")) %>%
  ggplot(aes(x = cost, y = mean, 
             colour = .metric)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free_y")  +
  scale_color_manual(values=c("black", "blue"))








best_acc <- select_best(svm_tune_res, "accuracy")
best_sens <- select_best(svm_tune_res, "sensitivity")
best_sens
best_spec <- select_best(svm_tune_res, "specificity")




svm_final_wf <- finalize_workflow(svm_wflow, best_sens)
svm_final_wf

svm_final_fit <- svm_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)


svm_final_fit %>%
  collect_metrics()

svm_final_fit %>% collect_predictions() %>% 
  conf_mat(truth = DiscretionaryAccrualsBinary, estimate = .pred_class) 



