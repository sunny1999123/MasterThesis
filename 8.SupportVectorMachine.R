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
library(scales)
library(extrafont)
loadfonts()  
Results <- as.data.frame(read.csv("Data/Filtered_Results.csv"))

Results$DiscretionaryAccrualsBinary <- as.factor(Results$DiscretionaryAccrualsBinary)
Results$DiscretionaryAccrualsBinary <- factor(Results$DiscretionaryAccrualsBinary, levels = c("1", "0"))
str(Results$DiscretionaryAccrualsBinary)



#List of ABC
# List <- c("EBITDAMarginRatio"      ,         "ChangeInNetIncomeLossToSales"    ,"ChangeInSalesToTotalAssets" ,    
# "SalestoWorkingCapital"   ,        "DepreciationOverPlant"       ,    "AccountsReceivableTurnover"   ,  
# "DaysSalesinAccountingReceivable", "ChangeInWorkingCaitalToAssets"  , "ChangeInDepreciationOverPlant"  ,
# "ChangeInTimesInterestEarned", "DiscretionaryAccrualsBinary")  
# Results <- Results[, names(Results) %in% List]

set.seed(912340)
Results_split <- initial_split(data = Results, prop = 0.7, 
                               strata = DiscretionaryAccrualsBinary)


Results_train <- training(Results_split)
Results_test <- testing(Results_split)

# Create a recipe
svm_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary)

 # svm_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
   # step_downsample(DiscretionaryAccrualsBinary)

# Create a model specification
svm_spec <- svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune())
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
    grid = expand.grid(rbf_sigma = 10^seq(-5, -2, by = 1), cost =  10*seq(1, 10, by = 1)),
    metrics = class_metrics,
    control = control_grid(verbose = TRUE)
  )


saveRDS(svm_tune_res, "svm_tune_res.rds")
svm_tune_res <- readRDS("Tuning/svm_tune_res.rds")

svm_tune_metrics <- svm_tune_res %>%
  collect_metrics()
Metric_results <- svm_tune_metrics

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
options(scipen = 999)
svm_sens_spec <- svm_tune_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("accuracy","sensitivity", "specificity", "roc_auc")) %>%
  mutate(.metric = case_when(
    .metric == "accuracy" ~ "Accuracy",
    .metric == "sensitivity" ~ "Sensitivity",
    .metric == "specificity" ~ "Specificity",
    .metric == "roc_auc" ~ "ROC-AUC"
  )) %>%
  filter(rbf_sigma !="0.1")%>%
  ggplot(aes(x = cost, y = mean, 
             colour = .metric)) +
  geom_path() +
  facet_wrap(~ paste("Lamda =", rbf_sigma)) +
  labs(x = "Cost",y = "Metric Value", color = "Metrics:") +
  scale_color_manual(values=c("black", "blue", "green", "purple")) 

ggsave("Figures/SVMAccuracySensSpec.pdf", plot = svm_sens_spec, width = 8, height = 4, dpi = 300)


# svm_sens_spec <- svm_tune_res %>%
#   collect_metrics() %>%
#   filter(.metric %in% c("accuracy","sensitivity", "specificity", "roc_auc")) %>%
#   mutate(.metric = case_when(
#     .metric == "accuracy" ~ "Accuracy",
#     .metric == "sensitivity" ~ "Sensitivity",
#     .metric == "specificity" ~ "Specificity",
#     .metric == "roc_auc" ~ "ROC-AUC"
#   )) %>%
#   ggplot(aes(x = rbf_sigma, y = mean, 
#              colour = .metric)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(.metric ~ ., scales = "free_y")  +
#   labs(x = "Lambda",y = "Metric Value", color = "Metrics:")+
#   scale_color_manual(values=c("black", "blue", "green", "purple")) 
# 


#ggsave("Figures/SVMAccuracySensSpec.pdf", plot = svm_sens_spec, width = 6, height = 4, dpi = 300)


model <- Metric_results %>%
  pivot_wider(names_from = .metric,
              values_from = c("mean", "std_err")) %>%
  filter(.config == "Preprocessor1_Model04")




best_acc <- select(svm_tune_res, "accuracy")
best_acc
best_sens <- select_best(svm_tune_res, "sensitivity")
best_sens
best_spec <- select_best(svm_tune_res, "specificity")
best_spec


svm_final_wf <- finalize_workflow(svm_wflow, model)
svm_final_wf

svm_final_fit <- svm_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)


svm_final_fit %>%
  collect_metrics()

svm_final_fit %>% collect_predictions() %>% 
  conf_mat(truth = DiscretionaryAccrualsBinary, estimate = .pred_class) 











