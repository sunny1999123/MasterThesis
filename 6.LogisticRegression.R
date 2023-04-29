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

Results <- as.data.frame(read.csv("Filtered_Results.csv"))
Results$DiscretionaryAccrualsBinary <- as.factor(Results$DiscretionaryAccrualsBinary)
  
#-------
#Random Forest
#------

set.seed(12345678)
Results_split <- initial_split(data = Results, prop = 0.6, 
                               strata = DiscretionaryAccrualsBinary)


Results_train <- training(Results_split)
Results_test <- testing(Results_split)


set.seed(12345678)
cv_folds <- Results_train %>% vfold_cv(v = 10, strata = DiscretionaryAccrualsBinary)


rf_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") 


rf_model_tune <- rand_forest(mtry = tune(), trees = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_tune_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model_tune)

class_metrics <- metric_set(accuracy, kap, sensitivity, 
                            specificity, roc_auc)

registerDoParallel()

set.seed(12345678)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = expand.grid(mtry = 1:12, trees = seq(50, 500, 50)),
  metrics = class_metrics,
  control = control_grid(save_pred = TRUE)
)


rf_tune_res %>%
  collect_metrics()

best_acc <- select_best(rf_tune_res, "accuracy")
rf_final_wf <- finalize_workflow(rf_tune_wf, best_acc)
rf_final_wf

set.seed(9923)
rf_final_fit <- rf_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)

rf_final_fit %>%
  collect_metrics()

rf_final_fit %>% collect_predictions() %>% 
  conf_mat(truth = DiscretionaryAccrualsBinary, estimate = .pred_class) 


rf_model_vi <- rand_forest(mtry = 6, trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")
rf_vi_wf <- workflow() %>% 
  add_model(rf_model_vi) %>% 
  add_recipe(rf_recipe)

set.seed(9923)
rf_vi_fit <- rf_vi_wf %>% fit(data = Results_train)

rf_vi_fit %>% extract_fit_parsnip() %>% vi()
rf_vi_fit %>% extract_fit_parsnip() %>% vip(geom = "point", num_features = 10)
