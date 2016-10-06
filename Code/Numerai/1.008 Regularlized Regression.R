source("./Numerai/1.001 Initial Functions and Libraries.R")
library(glmnet)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Ridge regression. 
grid = 10 ^ seq(10, -5, length = 100)
rr <- glmnet(x = as.matrix(training[ , 1:21]), y = as.matrix(training[ ,22]), alpha = 0, lambda = grid)

# 3. Find optimal lambda using test set. 
rr_predict <- function(model_number, model_text, lambda_var) { 
  p <- test %>% 
    mutate(probability = predict(rr, s = lambda_var, newx = as.matrix(test[ , 1:21]))) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
results <- data.frame()
for (lambda_var in grid) { 
  temp_results <- rr_predict(rr, paste0("rr", lambda_var), lambda_var)
  results <- bind_rows(results, temp_results)
}
min(results$ll)
# 0.6916293 

# 4. Find optimal lambda using cross validation. 
cv <- cv.glmnet(as.matrix(training[ , 1:21]), as.matrix(training[ , 22]), alpha = 0)
plot(cv)
rr_predict(rr, "rr cross validation", cv$lambda.min)

# 2. Lasso regression. 
grid = 10 ^ seq(10, -5, length = 100)
lr <- glmnet(x = as.matrix(training[ , 1:21]), y = as.matrix(training[ ,22]), alpha = 1, lambda = grid)

# 3. Find optimal lambda using test set. 
lr_predict <- function(model_number, model_text, lambda_var) { 
  p <- test %>% 
    mutate(probability = predict(lr, s = lambda_var, newx = as.matrix(test[ , 1:21]))) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
results <- data.frame()
for (lambda_var in grid) { 
  temp_results <- lr_predict(rr, paste0("rr", lambda_var), lambda_var)
  results <- bind_rows(results, temp_results)
}
min(results$ll)
# 0.6916278 

# 4. Find optimal lambda using cross validation. 
cv <- cv.glmnet(as.matrix(training[ , 1:21]), as.matrix(training[ , 22]), alpha = 1)
plot(cv)
rr_predict(rr, "rr cross validation", cv$lambda.min)
