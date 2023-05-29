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
library(progress)
library(foreach)


Results <- as.data.frame(read.csv("Data/Filtered_Results.csv"))
Results$DiscretionaryAccrualsBinary <- as.factor(Results$DiscretionaryAccrualsBinary)
Results$DiscretionaryAccrualsBinary <- factor(Results$DiscretionaryAccrualsBinary, levels = c("1", "0"))
str(Results$DiscretionaryAccrualsBinary)
#-------
#Random Forest
#------

set.seed(12345678)
Results_split <- initial_split(data = Results, prop = 0.7, 
                               strata = DiscretionaryAccrualsBinary)


Results_train <- training(Results_split)
Results_test <- testing(Results_split)


set.seed(12345678)
cv_folds <- Results_train %>% vfold_cv(v = 10, strata = DiscretionaryAccrualsBinary)


rf_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary)


rf_model_tune <- rand_forest(mtry = tune(), trees = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_tune_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model_tune)

class_metrics <- metric_set(accuracy, sensitivity, 
                            specificity, roc_auc)

registerDoParallel()

set.seed(12345678)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = expand.grid(mtry = 1:15, trees = seq(50, 2000, 50)),
  metrics = class_metrics,
  control = control_grid(save_pred = TRUE)
)


saveRDS(rf_tune_res, "Tuning/rf_tune_res.rds")
rf_tune_res <- readRDS("Tuning/rf_tune_res.rds")

RF_metrics <- rf_tune_res %>%
  collect_metrics()





RF_sens_specmtry <- rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sensitivity", "specificity", "roc_auc")) %>%
  ggplot(aes(x = mtry, y = mean, 
             colour = .metric)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free_y")  +
  scale_color_manual(values=c("black", "blue", "green", "purple")) +
  labs(x="Number of Splits", y="Metric")

ggsave("Figures/RFSensSpecMtry.pdf", plot = RF_sens_specmtry, width = 6, height = 4, dpi = 300)


RF_sens_spectrees <- rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sensitivity", "specificity", "roc_auc")) %>%
  ggplot(aes(x = trees, y = mean, 
             colour = .metric)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free_y")  +
  scale_color_manual(values=c("black", "blue", "green", "purple")) +
  labs(x="Number of Trees", y="Metric")

ggsave("Figures/RFSensSpecTrees.pdf", plot = RF_sens_spectrees, width = 6, height = 4, dpi = 300)




# RF_sens_spec <- rf_tune_res %>%
#   collect_metrics() %>%
#   filter(.metric %in% c("sensitivity", "specificity")) %>%
#   ggplot(aes(x = mtry, y = mean, ymin = mean - std_err, ymax = mean + std_err, 
#              colour = .metric)) +
#   geom_errorbar() + 
#   geom_line() +
#   geom_point() +
#   facet_grid(.metric ~ ., scales = "free_y")  +
#   scale_color_manual(values=c("black", "black"))
# 
# 
# ggsave("Figures/RFSensSpec.pdf", plot = RF_sens_spec, width = 6, height = 4, dpi = 300)
# 
# RF_Accuracy <- rf_tune_res %>%
#   collect_metrics() %>%
#   filter(.metric %in% c("roc_auc", "accuracy")) %>%
#   ggplot(aes(x = mtry, y = mean, ymin = mean - std_err, ymax = mean + std_err, 
#              colour = .metric)) +
#   geom_errorbar() + 
#   geom_line() +
#   geom_point() +
#   facet_grid(.metric ~ ., scales = "free_y") +
#   scale_color_manual(values=c("black", "black"))
# 
# ggsave("Figures/RFAccuracy.pdf", plot = RF_Accuracy, width = 6, height = 4, dpi = 300)


best_acc <- select_best(rf_tune_res, "accuracy")
best_sens <- select_best(rf_tune_res, "sensitivity")
best_sens
best_spec <- select_best(rf_tune_res, "specificity")


rf_final_wf <- finalize_workflow(rf_tune_wf, best_sens)
rf_final_wf

set.seed(9923)
rf_final_fit <- rf_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)

rf_final_fit %>%
  collect_metrics()

rf_final_fit %>% collect_predictions() %>% 
  conf_mat(truth =DiscretionaryAccrualsBinary, estimate = .pred_class) 

rf_final_fit %>% collect_predictions() %>% 
  roc_curve(loan_status, .pred_Default) %>% 
  autoplot()



rf_model_vi <- rand_forest(mtry = 6, trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")
rf_vi_wf <- workflow() %>% 
  add_model(rf_model_vi) %>% 
  add_recipe(rf_recipe)

set.seed(9923)
rf_vi_fit <- rf_vi_wf %>% fit(data = Results_train)

rf_vi_fit %>% extract_fit_parsnip() %>% vi()
RFVI <- rf_vi_fit %>% extract_fit_parsnip() %>% vip(geom = "point", num_features = 10)

ggsave("Figures/RFVI.pdf", plot = RFVI, width = 6, height = 4, dpi = 300)

