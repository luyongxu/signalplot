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
source("./Bitcoin/1.005 Cross Validate Model.R")

#' # Train Model
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features]),
                       label = as.matrix(train[, "future_return_sign"]))
model <- xgb.train(data = xgtrain,
                   params = xgbparams, 
                   nrounds = nrounds)

#' # Plot Importance
importance <- xgb.importance(model = model, feature_names = features)
xgb.plot.importance(importance)

#' # Generate Predictions
predictions <- predict(model, as.matrix(test[, features])) %>%
  matrix(nrow = nrow(test), ncol = 1) %>% 
  as_tibble() %>%
  mutate(date = test[["date"]]) %>% 
  select(date, pred_future_return = V1) %>% 
  left_join(test %>% select(date, close, future_return, future_return_sign))

#' # Plot Predictions
ggplot(predictions, aes(x = pred_future_return)) +
  geom_histogram()
ggplot(predictions, aes(x = future_return, y = pred_future_return)) + 
  geom_point() + 
  geom_hline(yintercept = 0.50) + 
  geom_vline(xintercept = 0)
predictions <- predictions %>% 
  mutate(pred_future_return_sign = ifelse(pred_future_return > 0.6, 1, 0))
table(predictions$pred_future_return_sign, predictions$future_return_sign)
ggplot(predictions, aes(x = date, y = close, colour = pred_future_return)) + 
  geom_line(size = 1.5) + 
  geom_point() + 
  scale_colour_gradient(low = "red", high = "blue")
ggplot(predictions, aes(x = date, y = close, colour = pred_future_return_sign)) + 
  geom_line(size = 1.5) + 
  geom_point() + 
  scale_colour_gradient(low = "red", high = "blue")
