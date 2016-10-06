source("./Numerai/1.001 Initial Functions and Libraries.R")
library(leaps)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Subset selection.
ss <- regsubsets(target ~ 
                     feature1 + feature2 + feature3 + feature4 + feature5 + 
                     feature6 + feature7 + feature8 + feature9 + feature10 + 
                     feature11 + feature12 + feature13 + feature14 + feature15 + 
                     feature16 + feature17 + feature18 + feature19 + feature20 + 
                     feature21, 
                   data = training, 
                   nvmax = 21)
summary(ss)
plot(summary(ss)$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(summary(ss)$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# 3. Select best model.  
test_matrix <- model.matrix(target ~ ., data = test)
results <- data.frame()
for (i in 1:21) { 
  coef_i <- coef(ss, id = i) 
  pred <- test_matrix[ , names(coef_i)] %*% coef_i
  ll <- data.frame(model = i, ll = log_loss(test$target, pred))
  results <- bind_rows(results, ll)
}
ggplot(results, aes(x = model, y = ll)) + geom_point()
which.min(results$ll)
min(results$ll)
coef(ss, 13)

