#' ---
#' title: "Train Model"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Source Load Data
source("./Bitcoin/1.003 Engineer Features.R")

#' # Train Model
features <- c("return_lag_01", "return_lag_02", "return_lag_03", "return_lag_04", "return_lag_05", 
              "return_lag_06", "return_lag_07", "return_lag_08", "return_lag_09", "return_lag_10", 
              "return_sma_01", "return_sma_02", "return_sma_03", "return_sma_04", "return_sma_05", 
              "return_sma_06", "return_sma_07", "return_sma_08", "return_sma_09", "return_sma_10")
              
#' # Create XGBoost Objects
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features]),
                       label = as.matrix(train[, "future_return"]))
xgbparams <- list(booster = "gblinear", 
                  # Tree booster parameters
                  eta = 0.1,
                  gamma = 1,
                  max_depth = 4,
                  min_child_weight = 1,
                  subsample = 0.7, 
                  colsample_by_tree = 0.7, 
                  # Learning task parameters
                  objective = "reg:linear", 
                  eval_metric = "rmse")

#' # Run Cross Validation
cv <- xgb.cv(data = xgtrain, 
             params = xgbparams, 
             # Cross validation parameters
             showsd = TRUE,
             early_stopping_rounds = 20,
             print_every_n = 20,
             nfold = 10,
             nrounds = 10000)

#' # Train Model
model_list <- list()
for (i in 1:100) { 
  print(str_c("Training model ", i, "."))
  set.seed(i)
  model_list[[i]] <- xgb.train(data = xgtrain,
                               params = xgbparams, 
                               nrounds = 60)
}


#' # Plot Importance
importance <- xgb.importance(model = model_list[[1]], feature_names = features)
xgb.plot.importance(importance)

#' # Generate Predictions
predictions_list <- list()
for (i in 1:100) { 
  print(str_c("Generating predictions for model ", i, "."))
  predictions_list[[i]] <- predictions <- predict(model_list[[i]], as.matrix(test[, features])) %>%
    matrix(nrow = nrow(test), ncol = 1)
}
predictions_list <- pmap_dbl(predictions_list, mean) %>% 
  as_tibble() %>%
  mutate(date = test[["date"]]) %>% 
  select(date, pred_future_return = value) %>% 
  left_join(test %>% select(date, future_return, future_return_sign))
ggplot(predictions_list, aes(x = pred_future_return)) +
  geom_histogram()
ggplot(predictions_list, aes(x = future_return, y = pred_future_return)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)
predictions_list <- predictions_list %>% 
  mutate(pred_future_return_sign = ifelse(pred_future_return > 0, 1, 0))
table(predictions_list$pred_future_return_sign, predictions_list$future_return_sign)
sum(predictions_list$pred_future_return_sign == predictions_list$future_return_sign, na.rm = TRUE) / 470
sum(predictions_list$pred_future_return  > 0 & predictions_list$future_return > 0, na.rm = TRUE) / 470
sum(predictions_list$pred_future_return  > 0 & predictions_list$future_return < 0, na.rm = TRUE) / 470
sum(predictions_list$pred_future_return  < 0 & predictions_list$future_return < 0, na.rm = TRUE) / 470
sum(predictions_list$pred_future_return  < 0 & predictions_list$future_return > 0, na.rm = TRUE) / 470

