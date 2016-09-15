source("./Numerai/1.001 Initial Functions and Libraries.R")
library(randomForest)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Decision tree.
rf01 <- randomForest(target ~ ., data = training, ntree = 100)
rf02 <- randomForest(target ~ ., data = training, ntree = 250)
rf03 <- randomForest(target ~ ., data = training, ntree = 500)
rf04 <- randomForest(target ~ ., data = training, ntree = 1000)

# 3. Predict. 
rf_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test)) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
rf_predict(rf01, "rf01") #rf01 0.7002352
rf_predict(rf02, "rf02") #rf02 0.6969897
rf_predict(rf03, "rf03") #rf03 0.6961086
rf_predict(rf04, "rf04") #rf04 0.6962462
