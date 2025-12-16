---
title: "Final Project - Predicting Overall Business Performance"
author: "Faith"
date: "15th December"

---

#load packages and data
library(tidyverse)
library(tidymodels)
library(dplyr)
library(ranger)

btos_data <- read_csv("final_btos_features.csv")


LAG_MAX <- 3 # Maximum lag to include in the Comprehensive Model


# Prepare Target categorical variable Perf_cat based on the continuous Performance.Score.
modeling_data_prep <- btos_data %>%
  mutate(Perf_cat = case_when(
    Performance.Score > 0.5 ~ "Above Average/Excellent",
    Performance.Score >= 0 ~ "Average",
    TRUE ~ "Below Average/Poor"
  )) %>%
  mutate(Perf_cat = factor(Perf_cat,
                           levels = c("Below Average/Poor", "Average", "Above Average/Excellent"),
                           ordered = TRUE))


# FEATURE ENGINEERING (LAGGING) 

lag_cols <- c("Performance.Score", "Net.Revenue.Q4.Index", "Net.Employee.Q5.Index",
              "Net.Hours.Q6.Index", "Net.AI.Q7.Index", "Net.AI.Q24.Index",
              "Net.Demand.Q10.Index", "Net.Prices.Q11.Index", "Net.SuppliesPrices.Q12.Index")

create_multi_lag_data <- function(data, max_lag) {
  lag_fns <- setNames(
    lapply(1:max_lag, function(n) function(x) lag(x, n = n)),
    paste0("L", 1:max_lag)
  )
  
  data %>%
    arrange(State, Survey.Year, Survey.WeekNum) %>%
    group_by(State) %>%
    mutate(across(all_of(lag_cols),
                  .fns = lag_fns,
                  .names = "Lag_{.fn}_{.col}"
    )) %>%
    ungroup() %>%
    filter(if_all(starts_with("Lag_L3"), ~ !is.na(.)))
}

# Create the full multi-lagged dataset
multi_lag_data <- create_multi_lag_data(modeling_data_prep, LAG_MAX)



#Prepare two separate datasets for modeling
#BASELINE Data: Only includes Lag_L1_Performance.Score
base_lag_data <- multi_lag_data %>%
  select(Performance.Score, Perf_cat, State, Survey.Year, Survey.WeekNum, 
         Lag_L1_Performance.Score) %>%
  rename(Lag_Perf_Prev = Lag_L1_Performance.Score)

#COMPREHENSIVE Data: Includes all L1, L2, and L3 lags
comp_lag_data <- multi_lag_data %>%
  select(Performance.Score, Perf_cat, State, Survey.Year, Survey.WeekNum, starts_with("Lag_L"))


#MODEL SETUP (Train/Validation/Test Split) 
# Create a numeric time index for proper ordering
base_lag_data <- base_lag_data %>%
  mutate(Time_Index = Survey.Year * 100 + Survey.WeekNum) %>%
  arrange(Time_Index)
comp_lag_data <- comp_lag_data %>%
  mutate(Time_Index = Survey.Year * 100 + Survey.WeekNum) %>%
  arrange(Time_Index)

# Define cutoffs for 60/20/20 split
time_cutoff_train <- quantile(base_lag_data$Time_Index, 0.60)
time_cutoff_validation <- quantile(base_lag_data$Time_Index, 0.80)

# Split BASELINE data
train_data_base <- base_lag_data %>% filter(Time_Index <= time_cutoff_train)
val_data_base <- base_lag_data %>% filter(Time_Index > time_cutoff_train & Time_Index <= time_cutoff_validation)
test_data_base <- base_lag_data %>% filter(Time_Index > time_cutoff_validation)

# Split COMPREHENSIVE data (using the same time indices)
train_data_comp <- comp_lag_data %>% filter(Time_Index <= time_cutoff_train)
val_data_comp <- comp_lag_data %>% filter(Time_Index > time_cutoff_train & Time_Index <= time_cutoff_validation)
test_data_comp <- comp_lag_data %>% filter(Time_Index > time_cutoff_validation)

cat(
  "\nData Split (Time-Based):\n",
  "Train Period:", range(train_data_base$Time_Index),
  "\nValidation Period:", range(val_data_base$Time_Index),
  "\nTest Period:", range(test_data_base$Time_Index)
)


#LINEAR REGRESSION (Predicting Continuous Performance.Score) 
reg_spec <- linear_reg() %>% set_engine("lm")


#Recipe
base_recipe <- recipe(
  Performance.Score ~ Lag_Perf_Prev + State,
  data = train_data_base) %>%
  step_center(Lag_Perf_Prev) %>%
  step_scale(Lag_Perf_Prev) %>%
  step_dummy(State)

#Workflow and Training
base_workflow <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(reg_spec)

base_fit <- fit(base_workflow, data = train_data_base)

# Evaluation (using Validation data)
base_preds_val <- predict(base_fit, val_data_base) %>%
  bind_cols(val_data_base %>% select(Performance.Score))

base_metrics_val <- base_preds_val %>%
  metrics(truth = Performance.Score, estimate = .pred)

print(base_metrics_val)


#COMPREHENSIVE MODEL (Full Features, n=1, 2, 3 Lags)

#Recipe: Full set of lagged features (L1, L2, L3) + State
comp_formula <- paste("Performance.Score ~ ", paste(names(select(train_data_comp, starts_with("Lag_L"))), collapse = " + "), " + State")

comp_recipe <- recipe(
  as.formula(comp_formula),
  data = train_data_comp) %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(State)

#Workflow and Training
comp_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(reg_spec)

comp_fit <- fit(comp_workflow, data = train_data_comp)

#Evaluation (using Validation data)
comp_preds_val <- predict(comp_fit, val_data_comp) %>%
  bind_cols(val_data_comp %>% select(Performance.Score))

comp_metrics_val <- comp_preds_val %>%
  metrics(truth = Performance.Score, estimate = .pred)

print(comp_metrics_val)

#Comparison and Summary ---
comparison_metrics_val <- bind_rows(
  base_metrics_val %>% mutate(Model = "Baseline (L1 Perf Only)"),
  comp_metrics_val %>% mutate(Model = "Comprehensive (L1, L2, L3 All Feat)")
) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(Model, rmse, rsq, mae)

print(comparison_metrics_val)



#LASSO SPECIFICATION AND SETUP
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

#LASSO workflow using the Comprehensive Recipe
lasso_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(lasso_spec)

# Create 10-fold cross-validation folds from the Training Data
set.seed(42) # for reproducibility
comp_folds <- vfold_cv(train_data_comp, v = 10)

# Create a grid of penalty values to test (lambda)
# We use 20 penalty values ranging from 10^-5 to 10^0
lasso_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

# --- 4B: TUNING THE LASSO PENALTY (Lambda) ---

cat("\n--- Starting LASSO Tuning (10-Fold CV) ---\n")

# Tune the model using the training data and CV folds
lasso_tune_results <- lasso_workflow %>%
  tune_grid(
    resamples = comp_folds,
    grid = lasso_grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(verbose = FALSE)
  )

cat("--- LASSO Tuning Complete ---\n")


# --- 4C: SELECTING THE BEST LASSO MODEL (CORRECTED) ---

# Select the best penalty (lambda) based on the lowest RMSE
# *Correction:* Pass the metric name via the 'metric' argument.
best_lasso_penalty <- lasso_tune_results %>%
  select_best(metric = "rmse") 

cat("\nOptimal LASSO Penalty (Lambda) based on min RMSE:\n")
print(best_lasso_penalty)

# Finalize the workflow with the best penalty
lasso_final_workflow <- lasso_workflow %>%
  finalize_workflow(best_lasso_penalty)

# Fit the final model to the full Training Set
lasso_final_fit <- fit(lasso_final_workflow, train_data_comp)

# Make predictions on the Validation Set (202426 - 202509)
lasso_preds_val <- predict(lasso_final_fit, val_data_comp) %>%
  bind_cols(val_data_comp %>% select(Performance.Score))

# Calculate metrics for the LASSO model on the Validation Set
lasso_metrics_val <- lasso_preds_val %>%
  metrics(truth = Performance.Score, estimate = .pred)

cat("\n--- OPTIMIZED LASSO MODEL METRICS (Validation Set) ---\n")
print(lasso_metrics_val)

# --- 4D: FINAL COMPARISON SUMMARY ---
comparison_metrics_val_final <- bind_rows(
  base_metrics_val %>% mutate(Model = "Baseline (L1 Perf Only)"),
  comp_metrics_val %>% mutate(Model = "Comprehensive (Unregularized)"),
  lasso_metrics_val %>% mutate(Model = "Optimized LASSO")
) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(Model, rmse, rsq, mae)

cat("\n--- FINAL MODEL COMPARISON (Validation Set) ---\n")
print(comparison_metrics_val_final)

