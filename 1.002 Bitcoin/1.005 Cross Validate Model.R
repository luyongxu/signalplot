#' ---
#' title: "Cross Validate Model"
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
features <- c("close_01w", "close_02w", "close_03w",
              "close_01m", "close_02m", "close_03m",
              "close_04m", "close_05m", "close_06m",
              "close_07m", "close_08m", "close_09m",
              "close_10m", "close_11m", "close_12m", 
              "sd_01w", "sd_02w", "sd_03w",
              "sd_01m", "sd_02m", "sd_03m",
              "sd_04m", "sd_05m", "sd_06m",
              "sd_07m", "sd_08m", "sd_09m",
              "sd_10m", "sd_11m", "sd_12m")
features <- c("close_01w", "close_02w", "close_03w",
              "close_01m", "close_02m", "close_03m",
              "close_04m", "close_05m", "close_06m",
              "close_07m", "close_08m", "close_09m",
              "close_10m", "close_11m", "close_12m")

#' # Create XGBoost Objects
xgbparams <- list(booster = "gbtree", 
                  eta = 0.01,
                  gamma = 1,
                  max_depth = 4,
                  min_child_weight = 1,
                  subsample = 0.7, 
                  colsample_by_tree = 0.7, 
                  objective = "binary:logistic", 
                  eval_metric = "error")
nrounds <- 150

#' # Run Cross Validation With Forward Chaining
predictions <- data.frame()
for (i in 1:10) { 
  index_train_start <- 1
  index_train_end <- i * 150
  index_test_start <- index_train_end + 30
  index_test_end <- min(index_test_start + 150, nrow(train))
  print(str_c("Cross validating on fold ", i, ". ", 
              "Train index is from ", index_train_start, " to ", index_train_end, ". ", 
              "Test index is from ", index_test_start, " to ", index_test_end, "."))
  xgtrain <- xgb.DMatrix(data = as.matrix(train[index_train_start:index_train_end, features]),
                         label = as.matrix(train[index_train_start:index_train_end, "future_return_sign"]))
  xgtest <- train[index_test_start:index_test_end, ]
  model_list <- list()
  for (i in 1:10) { 
    print(str_c("Training model ", i, "."))
    set.seed(i)
    model_list[[i]] <- xgb.train(data = xgtrain,
                                 params = xgbparams, 
                                 nrounds = nrounds)
  }
  predictions_list <- list()
  for (i in 1:10) { 
    print(str_c("Generating predictions for model ", i, "."))
    predictions_list[[i]] <-  predict(model_list[[i]], as.matrix(xgtest[, features])) %>%
      matrix(nrow = nrow(xgtest), ncol = 1)
  }
  df <- pmap_dbl(predictions_list, mean) %>% 
    as_tibble() %>%
    mutate(date = xgtest[["date"]]) %>% 
    select(date, pred_future_return_sign = value) %>% 
    left_join(xgtest %>% select(date, close, close_01d, future_return, future_return_sign))
  predictions <- bind_rows(predictions, df) %>% 
    as_tibble()
}

#' # Plot Importance
importance <- xgb.importance(model = model_list[[1]], feature_names = features)
xgb.plot.importance(importance) 

#' # Plot Out Of Fold Predictions
ggplot(predictions, aes(x = pred_future_return_sign)) + 
  geom_histogram()
ggplot(predictions, aes(x = date, y = close, colour = pred_future_return_sign)) + 
  geom_line() + 
  scale_colour_gradient(low = "red", high = "blue") + 
  scale_y_log10()

#' # Plot Trading Signal
predictions <- predictions %>%
  mutate(pred_future_return_signal = ifelse(pred_future_return_sign > 0.6, 1, 0), 
         equity_curve = cumprod(1 + pred_future_return_signal * close_01d) - 1)
table(predictions$pred_future_return_signal, predictions$future_return_sign)
ggplot(predictions, aes(x = date, y = close, colour = pred_future_return_signal)) + 
  geom_line() +
  scale_colour_gradient(low = "red", high = "blue") +
  scale_y_log10()

#' # Plot Equity Curve
ggplot(predictions, aes(x = date, y = equity_curve)) + 
  geom_line(colour = "blue")


# stochastic oscillator
# macd
