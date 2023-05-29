
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(caret)
library("themis")
library(recipes)
library(tidymodels)
library("glmnet")
library("kernlab")
library("skimr")
library(recipes)
#Load dataframe
Results <- as.data.frame(read.csv("Data/PreDimensionalityData.csv"))


#############
#histogram of all features
############

cols <- names(Results)[4:40]

# Set up the plotting window to show multiple histograms side by side
par(mfrow = c(9,4)) # change the numbers to adjust the layout

# Generate a histogram for each selected column
# Create a list to store the histograms
hist_list <- list()

# Loop through the columns and create a histogram for each
for (col in cols) {
  hist_list[[col]] <- hist(Results[[col]], main = col, xlab = "Values", plot = FALSE)
}

# Set up the LaTeX code for the figure
cat("\\begin{figure}[ht]\n")
cat("\\centering\n")

# Loop through the histograms and create subfigures
for (i in 1:length(hist_list)) {
  cat(paste0("\\begin{subfigure}[b]{0.3\\textwidth}\n"))
  cat(paste0("\\centering\n"))
  cat(paste0("\\includegraphics[width=\\textwidth]{", names(hist_list)[i], ".pdf}\n"))
  cat(paste0("\\caption{", names(hist_list)[i], "}\n"))
  cat(paste0("\\end{subfigure}\n"))
  if (i %% 3 == 0) {
    cat(paste0("\\\\ \n"))
  }
}

# Close the LaTeX code for the figure
cat("\\caption{Histograms}\n")
cat("\\end{figure}\n")

# Save each histogram as a separate PDF file
for (i in 1:length(hist_list)) {
  pdf(paste0(names(hist_list)[i], ".pdf"), height = 3, width = 3)
  plot(hist_list[[i]], main = "", xlab = "Values", ylab = "Frequency", col = "white", border = "black")
  dev.off()
}


##############

############
##EM Distribution
########

counts <- Results %>% group_by(fy, DiscretionaryAccrualsBinary) %>% summarize(count = n())
EMPlot <-ggplot(counts, aes(x = DiscretionaryAccrualsBinary, y = count, fill = factor(DiscretionaryAccrualsBinary))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +
  facet_grid(fy~., scales = "free_x") +
  labs(x = "EM Proxy", y = "Count", fill = "EM proxy") +
  scale_x_continuous(breaks = c(0, 1), labels = scales::comma_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "grey"), name = "Legend", labels = c("Moderate EM", "Extreme EM")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 350)
EMPlot


ggsave("Figures/DiscretionaryAccruals.pdf", plot = EMPlot, width = 6, height = 4, dpi = 300)

###################################################################
#Subset Selection#
###################################################################


# set seed for reproducibility
set.seed(1234567)

#Set target variables as factor
Results$DiscretionaryAccrualsBinary <- as.factor(Results$DiscretionaryAccrualsBinary)
Results$DiscretionaryAccrualsBinary <- factor(Results$DiscretionaryAccrualsBinary, levels = c("1", "0"))
#create split, stratafied on target variables
Results_split <- initial_split(Results, prop = 0.7, strata = DiscretionaryAccrualsBinary)

#Create the training and test set
Results_train <- training(Results_split)
Results_test <- testing(Results_split)


#Check if stratifying worked
Results_test %>% count(DiscretionaryAccrualsBinary) %>% 
  mutate(prop=n/sum(n))


Results_train %>% count(DiscretionaryAccrualsBinary) %>% 
  mutate(prop=n/sum(n))

#Create folds for CV
set.seed(1234567)
cv_folds <- vfold_cv(Results_train, v = 10, strata = DiscretionaryAccrualsBinary)
cv_folds


#Set recipe
glmnet_recipe <-  recipe(DiscretionaryAccrualsBinary ~ ., data = Results_train) %>%
  update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
  step_downsample(DiscretionaryAccrualsBinary)


#Set model
lasso_logreg <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

#Set wf
lasso_wf <- workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(lasso_logreg)

#Metrics of interest
class_metrics <- metric_set(accuracy, sensitivity, 
                            specificity, roc_auc)
class_metrics

#Set grid for cv validation
grid_lasso <- tibble(penalty = 10^(seq(from = -4.5, to = -0.5, length.out = 100)))

#Tuning
lasso_tune <- lasso_wf %>% 
  tune_grid(resamples = cv_folds, 
            grid = grid_lasso,
            metrics = class_metrics)

saveRDS(lasso_tune, "Tuning/Lasso_tune_res.rds")
lasso_tune <- readRDS("Tuning/Lasso_tune_res.rds")

#collect metrics 
lasso_tune_metrics <- lasso_tune %>% 
  collect_metrics()

# LassoAccuracy <- lasso_tune_metrics %>% filter(.metric == "sensitivity") %>% 
#   ggplot(aes(x = penalty, y = mean, 
#              ymin = mean - std_err, ymax = mean + std_err)) + 
#   geom_errorbar(alpha = 0.5) + 
#   geom_point() + 
#   scale_x_log10() + 
#   labs(y = "sensitivity", x = expression(lambda))

#ggsave("Figures/LassoAccuracy.pdf", plot = LassoAccuracy, width = 6, height = 4, dpi = 300)


LassoTuning <- lasso_tune_metrics %>%
  filter(.metric %in% c("accuracy", "sensitivity", "specificity", "roc_auc")) %>%
  ggplot(aes(x = penalty, y = mean, 
             colour = .metric)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free_y")  +
  scale_color_manual(values=c("black", "blue", "green", "purple")) +
  labs(x="Lambda", y="Metric")

best_acc <- select_best(lasso_tune, "accuracy")
best_acc
best_sens <- select_best(lasso_tune, "sensitivity")
best_sens
best_spec <- select_best(lasso_tune, "specificity")
best_spec


Lasso_final_wf <- finalize_workflow(lasso_wf)



set.seed(12345678)

Lasso_final_fit <- Lasso_final_wf %>%
  last_fit(Results_split, metrics = class_metrics)

Lasso_final_fit %>%
  collect_metrics()

#Collect metrics of the model
lasso_test_metrics <- Lasso_final_fit %>% collect_metrics()
lasso_test_metrics

#Set metrics
lasso_test_metrics <- lasso_test_metrics %>% 
  select(-.estimator, -.config) %>% 
  mutate(model = "lasso")

#Save features of interest 
Features <- Lasso_final_fit %>% extract_fit_parsnip() %>% 
  tidy() %>% arrange(desc(abs(estimate)))





#Select best model based on one standard error rule
lasso_1se_model <- lasso_tune %>% 
  select_best(metric = "accuracy", desc(penalty))
lasso_1se_model


#Finalize workflow by selecting the best model
lasso_wf_tuned <- 
  lasso_wf %>% 
  finalize_workflow(lasso_1se_model)
lasso_wf_tuned

#apply model 
lasso_last_fit <- lasso_wf_tuned %>% 
  last_fit(Results_split, metrics = class_metrics)

#Collect metrics of the model
lasso_test_metrics <- Lasso_final_fit %>% collect_metrics()
lasso_test_metrics

#Set metrics
lasso_test_metrics <- lasso_test_metrics %>% 
  select(-.estimator, -.config) %>% 
  mutate(model = "lasso")

#Save features of interest 
Features <- Lasso_final_fit %>% extract_fit_parsnip() %>% 
  tidy() %>% arrange(desc(abs(estimate)))





Features$RoundedEsimate <- round(Features$estimate, 3)
Features$estimate <- NULL

FeatureRemoval <- subset(Features, RoundedEsimate == 0)
NonInterestedVariables <- as.character(FeatureRemoval$term)

ResultsFiltered <- Results[,!(colnames(Results) %in% NonInterestedVariables)]



write.csv(ResultsFiltered, "Data/Filtered_Results.csv", row.names = FALSE)
