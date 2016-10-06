source("./Numerai/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Logistic regression.
log01 <- glm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training, 
            family = "binomial")
log02 <- glm(target ~ 
              poly(feature1, 2) + poly(feature2, 2) + poly(feature3, 2) + 
              poly(feature4, 2) + poly(feature5, 2) + poly(feature6, 2) + 
              poly(feature7, 2) + poly(feature8, 2) + poly(feature9, 2) + 
              poly(feature10, 2) + poly(feature11, 2) + poly(feature12, 2) + 
              poly(feature13, 2) + poly(feature14, 2) + poly(feature15, 2) + 
              poly(feature16, 2) + poly(feature17, 2) + poly(feature18, 2) + 
              poly(feature19, 2) + poly(feature20, 2) + poly(feature21, 2), 
            data = training, 
            family = "binomial")
log03 <- glm(target ~ 
              poly(feature1, 3) + poly(feature2, 3) + poly(feature3, 3) + 
              poly(feature4, 3) + poly(feature5, 3) + poly(feature6, 3) + 
              poly(feature7, 3) + poly(feature8, 3) + poly(feature9, 3) + 
              poly(feature10, 3) + poly(feature11, 3) + poly(feature12, 3) + 
              poly(feature13, 3) + poly(feature14, 3) + poly(feature15, 3) + 
              poly(feature16, 3) + poly(feature17, 3) + poly(feature18, 3) + 
              poly(feature19, 3) + poly(feature20, 3) + poly(feature21, 3), 
            data = training, 
            family = "binomial")
log04 <- glm(target ~ 
              poly(feature1, 4) + poly(feature2, 4) + poly(feature3, 4) + 
              poly(feature4, 4) + poly(feature5, 4) + poly(feature6, 4) + 
              poly(feature7, 4) + poly(feature8, 4) + poly(feature9, 4) + 
              poly(feature10, 4) + poly(feature11, 4) + poly(feature12, 4) + 
              poly(feature13, 4) + poly(feature14, 4) + poly(feature15, 4) + 
              poly(feature16, 4) + poly(feature17, 4) + poly(feature18, 4) + 
              poly(feature19, 4) + poly(feature20, 4) + poly(feature21, 4), 
            data = training, 
            family = "binomial")
log05 <- glm(target ~ 
              poly(feature1, 5) + poly(feature2, 5) + poly(feature3, 5) + 
              poly(feature4, 5) + poly(feature5, 5) + poly(feature6, 5) + 
              poly(feature7, 5) + poly(feature8, 5) + poly(feature9, 5) + 
              poly(feature10, 5) + poly(feature11, 5) + poly(feature12, 5) + 
              poly(feature13, 5) + poly(feature14, 5) + poly(feature15, 5) + 
              poly(feature16, 5) + poly(feature17, 5) + poly(feature18, 5) + 
              poly(feature19, 5) + poly(feature20, 5) + poly(feature21, 5), 
            data = training, 
            family = "binomial")
log06 <- glm(target ~ 
              poly(feature1, 6) + poly(feature2, 6) + poly(feature3, 6) + 
              poly(feature4, 6) + poly(feature5, 6) + poly(feature6, 6) + 
              poly(feature7, 6) + poly(feature8, 6) + poly(feature9, 6) + 
              poly(feature10, 6) + poly(feature11, 6) + poly(feature12, 6) + 
              poly(feature13, 6) + poly(feature14, 6) + poly(feature15, 6) + 
              poly(feature16, 6) + poly(feature17, 6) + poly(feature18, 6) + 
              poly(feature19, 6) + poly(feature20, 6) + poly(feature21, 6), 
            data = training, 
            family = "binomial")
log07 <- glm(target ~ 
              poly(feature1, 7) + poly(feature2, 7) + poly(feature3, 7) + 
              poly(feature4, 7) + poly(feature5, 7) + poly(feature6, 7) + 
              poly(feature7, 7) + poly(feature8, 7) + poly(feature9, 7) + 
              poly(feature10, 7) + poly(feature11, 7) + poly(feature12, 7) + 
              poly(feature13, 7) + poly(feature14, 7) + poly(feature15, 7) + 
              poly(feature16, 7) + poly(feature17, 7) + poly(feature18, 7) + 
              poly(feature19, 7) + poly(feature20, 7) + poly(feature21, 7), 
            data = training, 
            family = "binomial")
log08 <- glm(target ~ 
              poly(feature1, 8) + poly(feature2, 8) + poly(feature3, 8) + 
              poly(feature4, 8) + poly(feature5, 8) + poly(feature6, 8) + 
              poly(feature7, 8) + poly(feature8, 8) + poly(feature9, 8) + 
              poly(feature10, 8) + poly(feature11, 8) + poly(feature12, 8) + 
              poly(feature13, 8) + poly(feature14, 8) + poly(feature15, 8) + 
              poly(feature16, 8) + poly(feature17, 8) + poly(feature18, 8) + 
              poly(feature19, 8) + poly(feature20, 8) + poly(feature21, 8), 
            data = training, 
            family = "binomial")
log09 <- glm(target ~ 
              poly(feature1, 9) + poly(feature2, 9) + poly(feature3, 9) + 
              poly(feature4, 9) + poly(feature5, 9) + poly(feature6, 9) + 
              poly(feature7, 9) + poly(feature8, 9) + poly(feature9, 9) + 
              poly(feature10, 9) + poly(feature11, 9) + poly(feature12, 9) + 
              poly(feature13, 9) + poly(feature14, 9) + poly(feature15, 9) + 
              poly(feature16, 9) + poly(feature17, 9) + poly(feature18, 9) + 
              poly(feature19, 9) + poly(feature20, 9) + poly(feature21, 9), 
            data = training, 
            family = "binomial")
log10 <- glm(target ~ 
              poly(feature1, 10) + poly(feature2, 10) + poly(feature3, 10) + 
              poly(feature4, 10) + poly(feature5, 10) + poly(feature6, 10) + 
              poly(feature7, 10) + poly(feature8, 10) + poly(feature9, 10) + 
              poly(feature10, 10) + poly(feature11, 10) + poly(feature12, 10) + 
              poly(feature13, 10) + poly(feature14, 10) + poly(feature15, 10) + 
              poly(feature16, 10) + poly(feature17, 10) + poly(feature18, 10) + 
              poly(feature19, 10) + poly(feature20, 10) + poly(feature21, 10), 
            data = training, 
            family = "binomial")
log11 <- glm(target ~ 
              feature1 * feature2 * feature3 * feature4 * feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training, 
            family = "binomial")
log12 <- glm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 * feature7 * feature8 * feature9 * feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training, 
            family = "binomial")
log13 <- glm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 * feature12 * feature13 * feature14 * feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training, 
            family = "binomial")
log14 <- glm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 * feature17 * feature18 * feature19 * feature20 + 
              feature21, 
            data = training, 
            family = "binomial")
log15 <- glm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training, 
            family = "binomial")

# 3. Calculate log loss. 
log_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test, type = "response")) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
(results <- bind_rows(log_predict(log01, "log01"), 
                      log_predict(log02, "log02"), 
                      log_predict(log03, "log03"), 
                      log_predict(log04, "log04"), 
                      log_predict(log05, "log05"), 
                      log_predict(log06, "log06"), 
                      log_predict(log07, "log07"), 
                      log_predict(log08, "log08"), 
                      log_predict(log09, "log09"), 
                      log_predict(log10, "log10"), 
                      log_predict(log11, "log11"), 
                      log_predict(log12, "log12"), 
                      log_predict(log13, "log13"), 
                      log_predict(log14, "log14"), 
                      log_predict(log15, "log15")))
ggplot(results, aes(x = model, y = ll)) + geom_point()
