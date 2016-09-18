source("./Posts/1.001 Initial Functions and Libraries.R")

# 1. Load libraries.
library(randomForest)
library(class)
library(kknn)

# 2. Load raw data.
predictions <- read.csv("./Raw Data/Numerai/example_predictions.csv")
tournament <- read.csv("./Raw Data/Numerai/numerai_tournament_data.csv")
raw_training <- read.csv("./Raw Data/Numerai/numerai_training_data.csv")

# 3. Split into training and test data. 
set.seed(1)
sample <- sample(1:nrow(raw_training), size = floor(0.75 * nrow(raw_training)))
training <- raw_training[sample, ]
test <- raw_training[-sample, ]

# 3. Logarithmic loss.
log_loss = function(actual, predicted, eps = 1e-15) {
  predicted <- pmax(predicted, eps)
  predicted <- pmin(predicted, 1-eps)
  ll = (-1 / length(actual)) * sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# 4. Plot data.
training_melt <- training %>% 
  mutate(row_number = row_number()) %>% 
  melt() %>% 
  filter(variable != "row_number", 
         variable != "target")
ggplot(training_melt, aes(x = value)) + geom_histogram(binwidth = 0.01) + facet_wrap(~ variable)
ggplot(training, aes(x = feature1, y = feature2)) + geom_point(alpha = 1/5, aes(colour = factor(target)))
ggplot(training, aes(x = feature2, y = feature3)) + geom_point(alpha = 1/5, aes(colour = factor(target)))
ggplot(training, aes(x = feature14, y = feature4)) + geom_point(alpha = 1/5, aes(colour = factor(target)))

# 5. Multiple linear regression.
m01 <- lm(target ~ 
            feature1 + feature2 + feature3 + feature4 + feature5 + 
            feature6 + feature7 + feature8 + feature9 + feature10 + 
            feature11 + feature12 + feature13 + feature14 + feature15 + 
            feature16 + feature17 + feature18 + feature19 + feature20 + 
            feature21, 
          data = training)
m02 <- lm(target ~ 
            feature2 * feature3 * feature4 * feature14 * feature17 * 
            feature1 + feature2 + feature3 + feature4 + feature5 + 
            feature6 + feature7 + feature8 + feature9 + feature10 + 
            feature11 + feature12 + feature13 + feature14 + feature15 + 
            feature16 + feature17 + feature18 + feature19 + feature20 + 
            feature21, 
          data = training)
m03 <- lm(target ~ 
            poly(feature1, 2) + poly(feature2, 2) + poly(feature3, 2) + 
            poly(feature4, 2) + poly(feature5, 2) + poly(feature6, 2) + 
            poly(feature7, 2) + poly(feature8, 2) + poly(feature9, 2) + 
            poly(feature10, 2) + poly(feature11, 2) + poly(feature12, 2) + 
            poly(feature13, 2) + poly(feature14, 2) + poly(feature15, 2) + 
            poly(feature16, 2) + poly(feature17, 2) + poly(feature18, 2) + 
            poly(feature19, 2) + poly(feature20, 2) + poly(feature21, 2), 
          data = training)
m04 <- lm(target ~ 
            poly(feature1, 3) + poly(feature2, 3) + poly(feature3, 3) + 
            poly(feature4, 3) + poly(feature5, 3) + poly(feature6, 3) + 
            poly(feature7, 3) + poly(feature8, 3) + poly(feature9, 3) + 
            poly(feature10, 3) + poly(feature11, 3) + poly(feature12, 3) + 
            poly(feature13, 3) + poly(feature14, 3) + poly(feature15, 3) + 
            poly(feature16, 3) + poly(feature17, 3) + poly(feature18, 3) + 
            poly(feature19, 3) + poly(feature20, 3) + poly(feature21, 3), 
          data = training)
m05 <- lm(target ~ 
            poly(feature1, 4) + poly(feature2, 4) + poly(feature3, 4) + 
            poly(feature4, 4) + poly(feature5, 4) + poly(feature6, 4) + 
            poly(feature7, 4) + poly(feature8, 4) + poly(feature9, 4) + 
            poly(feature10, 4) + poly(feature11, 4) + poly(feature12, 4) + 
            poly(feature13, 4) + poly(feature14, 4) + poly(feature15, 4) + 
            poly(feature16, 4) + poly(feature17, 4) + poly(feature18, 4) + 
            poly(feature19, 4) + poly(feature20, 4) + poly(feature21, 4), 
          data = training)
m06 <- lm(target ~ 
            poly(feature1, 5) + poly(feature2, 5) + poly(feature3, 5) + 
            poly(feature4, 5) + poly(feature5, 5) + poly(feature6, 5) + 
            poly(feature7, 5) + poly(feature8, 5) + poly(feature9, 5) + 
            poly(feature10, 5) + poly(feature11, 5) + poly(feature12, 5) + 
            poly(feature13, 5) + poly(feature14, 5) + poly(feature15, 5) + 
            poly(feature16, 5) + poly(feature17, 5) + poly(feature18, 5) + 
            poly(feature19, 5) + poly(feature20, 5) + poly(feature21, 5), 
          data = training)

# 6. Logistic regression. 
m07 <- glm(target ~ 
             feature1 + feature2 + feature3 + feature4 + feature5 + 
             feature6 + feature7 + feature8 + feature9 + feature10 + 
             feature11 + feature12 + feature13 + feature14 + feature15 + 
             feature16 + feature17 + feature18 + feature19 + feature20 + 
             feature21, 
           data = training, 
           family = "binomial")
m08 <- glm(target ~ 
             feature2 * feature3 * feature4 * feature14 * feature17 * 
             feature1 + feature2 + feature3 + feature4 + feature5 + 
             feature6 + feature7 + feature8 + feature9 + feature10 + 
             feature11 + feature12 + feature13 + feature14 + feature15 + 
             feature16 + feature17 + feature18 + feature19 + feature20 + 
             feature21, 
           data = training)
m09 <- glm(target ~ 
            poly(feature1, 2) + poly(feature2, 2) + poly(feature3, 2) + 
            poly(feature4, 2) + poly(feature5, 2) + poly(feature6, 2) + 
            poly(feature7, 2) + poly(feature8, 2) + poly(feature9, 2) + 
            poly(feature10, 2) + poly(feature11, 2) + poly(feature12, 2) + 
            poly(feature13, 2) + poly(feature14, 2) + poly(feature15, 2) + 
            poly(feature16, 2) + poly(feature17, 2) + poly(feature18, 2) + 
            poly(feature19, 2) + poly(feature20, 2) + poly(feature21, 2), 
          data = training, 
          family = "binomial")
m10 <- glm(target ~ 
            poly(feature1, 3) + poly(feature2, 3) + poly(feature3, 3) + 
            poly(feature4, 3) + poly(feature5, 3) + poly(feature6, 3) + 
            poly(feature7, 3) + poly(feature8, 3) + poly(feature9, 3) + 
            poly(feature10, 3) + poly(feature11, 3) + poly(feature12, 3) + 
            poly(feature13, 3) + poly(feature14, 3) + poly(feature15, 3) + 
            poly(feature16, 3) + poly(feature17, 3) + poly(feature18, 3) + 
            poly(feature19, 3) + poly(feature20, 3) + poly(feature21, 3), 
          data = training, 
          family = "binomial")
m11 <- glm(target ~ 
            poly(feature1, 4) + poly(feature2, 4) + poly(feature3, 4) + 
            poly(feature4, 4) + poly(feature5, 4) + poly(feature6, 4) + 
            poly(feature7, 4) + poly(feature8, 4) + poly(feature9, 4) + 
            poly(feature10, 4) + poly(feature11, 4) + poly(feature12, 4) + 
            poly(feature13, 4) + poly(feature14, 4) + poly(feature15, 4) + 
            poly(feature16, 4) + poly(feature17, 4) + poly(feature18, 4) + 
            poly(feature19, 4) + poly(feature20, 4) + poly(feature21, 4), 
          data = training, 
          family = "binomial")
m12 <- glm(target ~ 
            poly(feature1, 5) + poly(feature2, 5) + poly(feature3, 5) + 
            poly(feature4, 5) + poly(feature5, 5) + poly(feature6, 5) + 
            poly(feature7, 5) + poly(feature8, 5) + poly(feature9, 5) + 
            poly(feature10, 5) + poly(feature11, 5) + poly(feature12, 5) + 
            poly(feature13, 5) + poly(feature14, 5) + poly(feature15, 5) + 
            poly(feature16, 5) + poly(feature17, 5) + poly(feature18, 5) + 
            poly(feature19, 5) + poly(feature20, 5) + poly(feature21, 5), 
          data = training, 
          family = "binomial")

# 7. Linear discriminant analysis. Quadratic discriminent analysis.
library(MASS)
m13 <- lda(target ~ 
             feature1 + feature2 + feature3 + feature4 + feature5 + 
             feature6 + feature7 + feature8 + feature9 + feature10 + 
             feature11 + feature12 + feature13 + feature14 + feature15 + 
             feature16 + feature17 + feature18 + feature19 + feature20 + 
             feature21, 
           data = training)
m14 <- qda(target ~ 
             feature1 + feature2 + feature3 + feature4 + feature5 + 
             feature6 + feature7 + feature8 + feature9 + feature10 + 
             feature11 + feature12 + feature13 + feature14 + feature15 + 
             feature16 + feature17 + feature18 + feature19 + feature20 + 
             feature21, 
           data = training)
detach("package:MASS", unload = TRUE)

# 8. K-nearest neighbors.
knn_modified <- function(k_var) { 
  m <- data.frame(t_id = test$t_id, 
                  class = knn(training[ , 1:21], tournament[ , 2:22], training[ , 22], 
                              k = k_var, prob = TRUE), 
                  probability = attributes(knn(training[ , 1:21], tournament[ , 2:22], training[ , 22], 
                                               k = k_var, prob = TRUE))$prob) %>% 
    mutate(probability = ifelse(class == 0, 1 - probability, probability))
  m <- m[ , c(1, 3)]
  return(m)
}
knn_modified2 <- function(k_var) { 
  m <- data.frame(t_id = tournament$t_id, 
                  probability = kknn(target ~ ., training, tournament, k = k_var)$fitted.values)
  return(m)
}
m15 <- knn_modified(1)
m16 <- knn_modified(2)
m17 <- knn_modified(3)
m18 <- knn_modified(4)
m19 <- knn_modified(5)
m20 <- knn_modified(6)
m21 <- knn_modified(7)
m22 <- knn_modified(8)
m23 <- knn_modified(9)
m24 <- knn_modified(10)
m25 <- knn_modified(15)
m26 <- knn_modified(20)
m27 <- knn_modified(25)
m28 <- knn_modified(30)
m29 <- knn_modified(35)
m30 <- knn_modified(40)
m31 <- knn_modified(45)
m32 <- knn_modified(50)
m33 <- knn_modified(100)
m34 <- knn_modified(200)
m35 <- knn_modified(300)
m36 <- knn_modified(400)
m37 <- knn_modified(499)
m38 <- knn_modified2(100)
m39 <- knn_modified2(200)
m40 <- knn_modified2(300)
m41 <- knn_modified2(400)
m42 <- knn_modified2(500)
m43 <- knn_modified2(600)
m44 <- knn_modified2(700)
m45 <- knn_modified2(800)
m46 <- knn_modified2(900)
m47 <- knn_modified2(1000)
m48 <- knn_modified2(1500)
m49 <- knn_modified2(2000)

# 9. Predict data.
lm_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test)) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
log_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test, type = "response")) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
lda_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test)$posterior[ , 2]) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
results <- bind_rows(lm_predict(m01, "m01"), 
                     lm_predict(m02, "m02"), 
                     lm_predict(m03, "m03"), 
                     lm_predict(m04, "m04"), 
                     lm_predict(m05, "m05"), 
                     lm_predict(m06, "m06"), 
                     log_predict(m07, "m07"), 
                     log_predict(m08, "m08"), 
                     log_predict(m09, "m09"), 
                     log_predict(m10, "m10"), 
                     log_predict(m11, "m11"), 
                     log_predict(m12, "m12"), 
                     lda_predict(m13, "m13"), 
                     lda_predict(m14, "m14"))
ggplot(results, aes(x = model, y = ll)) + geom_point() + ylim(0.690, 0.692)




knn_predict <- function(model_number, file_name) { 
  write.csv(model_number, paste0("./Raw Data/Numerai/Predictions/", file_name, ".csv"), row.names = FALSE)
}
lm_predict(m01, "m01")
lm_predict(m02, "m02")
lm_predict(m03, "m03")
lm_predict(m04, "m04")
lm_predict(m05, "m05")
lm_predict(m06, "m06")
log_predict(m07, "m07")
log_predict(m08, "m08")
log_predict(m09, "m09")
log_predict(m10, "m10")
log_predict(m11, "m11")
log_predict(m12, "m12")
lda_predict(m13, "m13")
lda_predict(m14, "m14")
knn_predict(m15, "m15")
knn_predict(m16, "m16")
knn_predict(m17, "m17")
knn_predict(m18, "m18")
knn_predict(m19, "m19")
knn_predict(m20, "m20")
knn_predict(m21, "m21")
knn_predict(m22, "m22")
knn_predict(m23, "m23")
knn_predict(m24, "m24")
knn_predict(m25, "m25")
knn_predict(m26, "m26")
knn_predict(m27, "m27")
knn_predict(m28, "m28")
knn_predict(m29, "m29")
knn_predict(m30, "m30")
knn_predict(m31, "m31")
knn_predict(m32, "m32")
knn_predict(m33, "m33")
knn_predict(m34, "m34")
knn_predict(m35, "m35")
knn_predict(m36, "m36")
knn_predict(m37, "m37")
knn_predict(m38, "m38")
knn_predict(m39, "m39")
knn_predict(m40, "m40")
knn_predict(m41, "m41")
knn_predict(m42, "m42")
knn_predict(m43, "m43")
knn_predict(m44, "m44")
knn_predict(m45, "m45")
knn_predict(m46, "m46")
knn_predict(m47, "m47")
knn_predict(m48, "m48")
knn_predict(m49, "m49")



