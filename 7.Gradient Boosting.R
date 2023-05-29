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


Results <- as.data.frame(read.csv("Data/Filtered_Results.csv"))
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



xgb_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary)

xgb_model_tune <- 
  boost_tree(trees = tune(), tree_depth = tune(), 
             learn_rate = tune(), stop_iter = 500) %>%
  set_mode("classification") %>%
  set_engine("xgboost")


xgb_tune_wf <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_model_tune)
xgb_tune_wf

class_metrics <- metric_set(accuracy, sensitivity, 
                            specificity, roc_auc)

registerDoParallel()


set.seed(8504)
grid_max_entropy(trees(range = c(0, 10000)), 
                 learn_rate(range = c(-2, -1)), 
                 tree_depth(), size = 20)

xgb_grid <- expand.grid(trees = 500 * 1:20, 
                        learn_rate = c(0.1, 0.01), 
                        tree_depth = 1:3)

xgb_tune_res <- tune_grid(
  xgb_tune_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = class_metrics,
  control = control_grid(verbose = TRUE)
)

saveRDS(xgb_tune_res, "Tuning/xgb_tune_res.rds")
xgb_tune_res <- readRDS("Tuning/xgb_tune_res.rds")

xgb_tune_metrics <- xgb_tune_res %>%
  collect_metrics()
xgb_tune_metrics




xgb_tune_fig <- xgb_tune_res %>% 
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sensitivity", "specificity", "roc_auc")) %>%
  mutate(.metric = case_when(
    .metric == "accuracy" ~ "Accuracy",
    .metric == "sensitivity" ~ "Sensitivity",
    .metric == "specificity" ~ "Specificity",
    .metric == "roc_auc" ~ "ROC-AUC"
  )) %>%
  mutate(label = paste("Learn Rate:", learn_rate, "\nTree Depth:", tree_depth)) %>%
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_wrap(~ label) +
  labs(x = "Number of Trees",y = "Metric Value", color = "Metrics:")+
  scale_color_manual(values=c("black", "blue", "green", "purple")) 

  

ggsave("Figures/XGBAccuracySensSpec.pdf", plot = xgb_tune_fig, width = 7, height = 4, dpi = 300)


xgb_tune_metrics %>% 
  filter(tree_depth == 1, learn_rate == 0.1, trees >= 3000 & trees <= 6000) %>% 
  select(trees,learn_rate, .metric, mean) %>%
  pivot_wider(names_from = .metric,
              values_from = mean)

xgb_best <- xgb_tune_metrics %>% 
  filter(.metric == "accuracy", tree_depth == 1, learn_rate == 0.01, trees == 3500)

best_acc <- select_best(xgb_tune_res, "accuracy")
best_sens <- select_best(xgb_tune_res, "sensitivity")
best_sens
best_spec <- select_best(xgb_tune_res, "specificity")



xgb_final_wf <- finalize_workflow(xgb_tune_wf, best_sens)
xgb_final_wf

xgb_final_fit <- xgb_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)


xgb_final_fit %>%
  collect_metrics()

xgb_final_fit %>% collect_predictions() %>% 
  conf_mat(truth = DiscretionaryAccrualsBinary, estimate = .pred_class) 
