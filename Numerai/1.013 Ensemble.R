source("./Numerai/1.001 Initial Functions and Libraries.R")
library(xgboost)

# 1. Load raw data.
predictions <- read.csv("./Raw Data/Numerai/example_predictions.csv")
tournament <- read.csv("./Raw Data/Numerai/numerai_tournament_data.csv")
raw_training <- read.csv("./Raw Data/Numerai/numerai_training_data.csv")

# 2. Model. 
lin01 <- lm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = raw_training)
xgb01 <- xgboost(data = as.matrix(raw_training[ , 1:21]), 
                 label = as.matrix(raw_training[, 22]), 
                 max.depth = 4, 
                 eta = 0.1, 
                 nround = 800, 
                 objective = "binary:logistic", 
                 booster = "gblinear", 
                 verbose = 1, 
                 eval.metric = "logloss")
xgb02 <- xgboost(data = as.matrix(raw_training[ , 1:21]), 
                 label = as.matrix(raw_training[, 22]), 
                 max.depth = 4, 
                 eta = 0.01, 
                 nround = 800, 
                 objective = "binary:logistic", 
                 verbose = 1, 
                 eval.metric = "logloss")


# 3. Predict. 
pred_lin01 <- predict(lin01, tournament)
pred_xgb01 <- predict(xgb01, as.matrix(tournament[, 2:22]))
pred_xgb02 <- predict(xgb02, as.matrix(tournament[, 2:22]))
pred <- data.frame(t_id = tournament$t_id, probability = (pred_lin01))
write_csv(pred, "./Objects/Numerai/Predictions/pred.csv")
