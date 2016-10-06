source("./Numerai/1.001 Initial Functions and Libraries.R")
library(xgboost)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. 
dtrain <- xgb.DMatrix(data = as.matrix(training[ , 1:21]), label = as.matrix(training[ , 22]))
dtest <- xgb.DMatrix(data = as.matrix(test[ , 1:21]), label = as.matrix(test[ , 22]))
watchlist <- list(train = dtrain, test = dtest)

xgb01 <- xgb.train(data = dtrain, 
                   watchlist = watchlist, 
                   objective = "binary:logistic", 
                   eval.metric = "logloss", 
                   eta = 0.02, 
                   gamma = 0, 
                   max_depth = 4, 
                   min_child_weight = 1, 
                   max_delta_step = 0, 
                   subsample = 1, 
                   colsample_bylevel = 0.8, 
                   nrounds = 350)
xgb01 <- xgb.cv(data = dtrain, 
                objective = "binary:logistic", 
                eval.metric = "logloss", 
                nrounds = 500, 
                eta = 0.001, 
                max_depth = 6, 
                nfold = 5)
ggplot(xgb01, aes(x = 1:500, y = train.logloss.mean)) + geom_point() 
ggplot(xgb01, aes(x = 1:500, y = test.logloss.mean)) + geom_point() 
xgb02 <- xgb.train(data = dtrain, 
                   watchlist = watchlist, 
                   objective = "reg:logistic", 
                   eval.metric = "logloss", 
                   eta = 0.1, 
                   colsambleby_tree = 0.4, 
                   max_depth = 4, 
                   nround = 800, 
                   booster = "gblinear")
xgb02 <- xgb.train(data = dtrain, 
                   watchlist = watchlist, 
                   objective = "reg:logistic", 
                   eval.metric = "logloss", 
                   eta = 0.3, 
                   colsambleby_tree = 0.4, 
                   max_depth = 4, 
                   nround = 800, 
                   booster = "gblinear")



p <- test %>% 
  mutate(probability = predict(xgb02, as.matrix(test[ , 1:21]))) %>% 
  select(target, probability)
results <- data.frame(model = "xgboost", ll = log_loss(p$target, p$probability))
results