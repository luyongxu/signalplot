source("./Numerai/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. LDA and QDA. 
library(MASS)
lda01 <- lda(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
lda02 <- lda(target ~ 
              poly(feature1, 2) + poly(feature2, 2) + poly(feature3, 2) + 
              poly(feature4, 2) + poly(feature5, 2) + poly(feature6, 2) + 
              poly(feature7, 2) + poly(feature8, 2) + poly(feature9, 2) + 
              poly(feature10, 2) + poly(feature11, 2) + poly(feature12, 2) + 
              poly(feature13, 2) + poly(feature14, 2) + poly(feature15, 2) + 
              poly(feature16, 2) + poly(feature17, 2) + poly(feature18, 2) + 
              poly(feature19, 2) + poly(feature20, 2) + poly(feature21, 2), 
            data = training)
lda03 <- lda(target ~ 
              poly(feature1, 3) + poly(feature2, 3) + poly(feature3, 3) + 
              poly(feature4, 3) + poly(feature5, 3) + poly(feature6, 3) + 
              poly(feature7, 3) + poly(feature8, 3) + poly(feature9, 3) + 
              poly(feature10, 3) + poly(feature11, 3) + poly(feature12, 3) + 
              poly(feature13, 3) + poly(feature14, 3) + poly(feature15, 3) + 
              poly(feature16, 3) + poly(feature17, 3) + poly(feature18, 3) + 
              poly(feature19, 3) + poly(feature20, 3) + poly(feature21, 3), 
            data = training)
lda04 <- lda(target ~ 
              poly(feature1, 4) + poly(feature2, 4) + poly(feature3, 4) + 
              poly(feature4, 4) + poly(feature5, 4) + poly(feature6, 4) + 
              poly(feature7, 4) + poly(feature8, 4) + poly(feature9, 4) + 
              poly(feature10, 4) + poly(feature11, 4) + poly(feature12, 4) + 
              poly(feature13, 4) + poly(feature14, 4) + poly(feature15, 4) + 
              poly(feature16, 4) + poly(feature17, 4) + poly(feature18, 4) + 
              poly(feature19, 4) + poly(feature20, 4) + poly(feature21, 4), 
            data = training)
lda05 <- lda(target ~ 
              poly(feature1, 5) + poly(feature2, 5) + poly(feature3, 5) + 
              poly(feature4, 5) + poly(feature5, 5) + poly(feature6, 5) + 
              poly(feature7, 5) + poly(feature8, 5) + poly(feature9, 5) + 
              poly(feature10, 5) + poly(feature11, 5) + poly(feature12, 5) + 
              poly(feature13, 5) + poly(feature14, 5) + poly(feature15, 5) + 
              poly(feature16, 5) + poly(feature17, 5) + poly(feature18, 5) + 
              poly(feature19, 5) + poly(feature20, 5) + poly(feature21, 5), 
            data = training)
lda06 <- lda(target ~ 
              poly(feature1, 6) + poly(feature2, 6) + poly(feature3, 6) + 
              poly(feature4, 6) + poly(feature5, 6) + poly(feature6, 6) + 
              poly(feature7, 6) + poly(feature8, 6) + poly(feature9, 6) + 
              poly(feature10, 6) + poly(feature11, 6) + poly(feature12, 6) + 
              poly(feature13, 6) + poly(feature14, 6) + poly(feature15, 6) + 
              poly(feature16, 6) + poly(feature17, 6) + poly(feature18, 6) + 
              poly(feature19, 6) + poly(feature20, 6) + poly(feature21, 6), 
            data = training)
lda07 <- lda(target ~ 
              poly(feature1, 7) + poly(feature2, 7) + poly(feature3, 7) + 
              poly(feature4, 7) + poly(feature5, 7) + poly(feature6, 7) + 
              poly(feature7, 7) + poly(feature8, 7) + poly(feature9, 7) + 
              poly(feature10, 7) + poly(feature11, 7) + poly(feature12, 7) + 
              poly(feature13, 7) + poly(feature14, 7) + poly(feature15, 7) + 
              poly(feature16, 7) + poly(feature17, 7) + poly(feature18, 7) + 
              poly(feature19, 7) + poly(feature20, 7) + poly(feature21, 7), 
            data = training)
lda08 <- lda(target ~ 
              poly(feature1, 8) + poly(feature2, 8) + poly(feature3, 8) + 
              poly(feature4, 8) + poly(feature5, 8) + poly(feature6, 8) + 
              poly(feature7, 8) + poly(feature8, 8) + poly(feature9, 8) + 
              poly(feature10, 8) + poly(feature11, 8) + poly(feature12, 8) + 
              poly(feature13, 8) + poly(feature14, 8) + poly(feature15, 8) + 
              poly(feature16, 8) + poly(feature17, 8) + poly(feature18, 8) + 
              poly(feature19, 8) + poly(feature20, 8) + poly(feature21, 8), 
            data = training)
lda09 <- lda(target ~ 
              poly(feature1, 9) + poly(feature2, 9) + poly(feature3, 9) + 
              poly(feature4, 9) + poly(feature5, 9) + poly(feature6, 9) + 
              poly(feature7, 9) + poly(feature8, 9) + poly(feature9, 9) + 
              poly(feature10, 9) + poly(feature11, 9) + poly(feature12, 9) + 
              poly(feature13, 9) + poly(feature14, 9) + poly(feature15, 9) + 
              poly(feature16, 9) + poly(feature17, 9) + poly(feature18, 9) + 
              poly(feature19, 9) + poly(feature20, 9) + poly(feature21, 9), 
            data = training)
lda10 <- lda(target ~ 
              poly(feature1, 10) + poly(feature2, 10) + poly(feature3, 10) + 
              poly(feature4, 10) + poly(feature5, 10) + poly(feature6, 10) + 
              poly(feature7, 10) + poly(feature8, 10) + poly(feature9, 10) + 
              poly(feature10, 10) + poly(feature11, 10) + poly(feature12, 10) + 
              poly(feature13, 10) + poly(feature14, 10) + poly(feature15, 10) + 
              poly(feature16, 10) + poly(feature17, 10) + poly(feature18, 10) + 
              poly(feature19, 10) + poly(feature20, 10) + poly(feature21, 10), 
            data = training)
lda11 <- lda(target ~ 
              feature1 * feature2 * feature3 * feature4 * feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
lda12 <- lda(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 * feature7 * feature8 * feature9 * feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
lda13 <- lda(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 * feature12 * feature13 * feature14 * feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
lda14 <- lda(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 * feature17 * feature18 * feature19 * feature20 + 
              feature21, 
            data = training)
lda15 <- lda(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
qda01 <- qda(target ~ 
               feature1 + feature2 + feature3 + feature4 + feature5 + 
               feature6 + feature7 + feature8 + feature9 + feature10 + 
               feature11 + feature12 + feature13 + feature14 + feature15 + 
               feature16 + feature17 + feature18 + feature19 + feature20 + 
               feature21, 
             data = training)
qda02 <- qda(target ~ 
               poly(feature1, 2) + poly(feature2, 2) + poly(feature3, 2) + 
               poly(feature4, 2) + poly(feature5, 2) + poly(feature6, 2) + 
               poly(feature7, 2) + poly(feature8, 2) + poly(feature9, 2) + 
               poly(feature10, 2) + poly(feature11, 2) + poly(feature12, 2) + 
               poly(feature13, 2) + poly(feature14, 2) + poly(feature15, 2) + 
               poly(feature16, 2) + poly(feature17, 2) + poly(feature18, 2) + 
               poly(feature19, 2) + poly(feature20, 2) + poly(feature21, 2), 
             data = training)
qda03 <- qda(target ~ 
               poly(feature1, 3) + poly(feature2, 3) + poly(feature3, 3) + 
               poly(feature4, 3) + poly(feature5, 3) + poly(feature6, 3) + 
               poly(feature7, 3) + poly(feature8, 3) + poly(feature9, 3) + 
               poly(feature10, 3) + poly(feature11, 3) + poly(feature12, 3) + 
               poly(feature13, 3) + poly(feature14, 3) + poly(feature15, 3) + 
               poly(feature16, 3) + poly(feature17, 3) + poly(feature18, 3) + 
               poly(feature19, 3) + poly(feature20, 3) + poly(feature21, 3), 
             data = training)
qda04 <- qda(target ~ 
               poly(feature1, 4) + poly(feature2, 4) + poly(feature3, 4) + 
               poly(feature4, 4) + poly(feature5, 4) + poly(feature6, 4) + 
               poly(feature7, 4) + poly(feature8, 4) + poly(feature9, 4) + 
               poly(feature10, 4) + poly(feature11, 4) + poly(feature12, 4) + 
               poly(feature13, 4) + poly(feature14, 4) + poly(feature15, 4) + 
               poly(feature16, 4) + poly(feature17, 4) + poly(feature18, 4) + 
               poly(feature19, 4) + poly(feature20, 4) + poly(feature21, 4), 
             data = training)
qda05 <- qda(target ~ 
               poly(feature1, 5) + poly(feature2, 5) + poly(feature3, 5) + 
               poly(feature4, 5) + poly(feature5, 5) + poly(feature6, 5) + 
               poly(feature7, 5) + poly(feature8, 5) + poly(feature9, 5) + 
               poly(feature10, 5) + poly(feature11, 5) + poly(feature12, 5) + 
               poly(feature13, 5) + poly(feature14, 5) + poly(feature15, 5) + 
               poly(feature16, 5) + poly(feature17, 5) + poly(feature18, 5) + 
               poly(feature19, 5) + poly(feature20, 5) + poly(feature21, 5), 
             data = training)
qda06 <- qda(target ~ 
               poly(feature1, 6) + poly(feature2, 6) + poly(feature3, 6) + 
               poly(feature4, 6) + poly(feature5, 6) + poly(feature6, 6) + 
               poly(feature7, 6) + poly(feature8, 6) + poly(feature9, 6) + 
               poly(feature10, 6) + poly(feature11, 6) + poly(feature12, 6) + 
               poly(feature13, 6) + poly(feature14, 6) + poly(feature15, 6) + 
               poly(feature16, 6) + poly(feature17, 6) + poly(feature18, 6) + 
               poly(feature19, 6) + poly(feature20, 6) + poly(feature21, 6), 
             data = training)
qda07 <- qda(target ~ 
               poly(feature1, 7) + poly(feature2, 7) + poly(feature3, 7) + 
               poly(feature4, 7) + poly(feature5, 7) + poly(feature6, 7) + 
               poly(feature7, 7) + poly(feature8, 7) + poly(feature9, 7) + 
               poly(feature10, 7) + poly(feature11, 7) + poly(feature12, 7) + 
               poly(feature13, 7) + poly(feature14, 7) + poly(feature15, 7) + 
               poly(feature16, 7) + poly(feature17, 7) + poly(feature18, 7) + 
               poly(feature19, 7) + poly(feature20, 7) + poly(feature21, 7), 
             data = training)
qda08 <- qda(target ~ 
               poly(feature1, 8) + poly(feature2, 8) + poly(feature3, 8) + 
               poly(feature4, 8) + poly(feature5, 8) + poly(feature6, 8) + 
               poly(feature7, 8) + poly(feature8, 8) + poly(feature9, 8) + 
               poly(feature10, 8) + poly(feature11, 8) + poly(feature12, 8) + 
               poly(feature13, 8) + poly(feature14, 8) + poly(feature15, 8) + 
               poly(feature16, 8) + poly(feature17, 8) + poly(feature18, 8) + 
               poly(feature19, 8) + poly(feature20, 8) + poly(feature21, 8), 
             data = training)
qda09 <- qda(target ~ 
               poly(feature1, 9) + poly(feature2, 9) + poly(feature3, 9) + 
               poly(feature4, 9) + poly(feature5, 9) + poly(feature6, 9) + 
               poly(feature7, 9) + poly(feature8, 9) + poly(feature9, 9) + 
               poly(feature10, 9) + poly(feature11, 9) + poly(feature12, 9) + 
               poly(feature13, 9) + poly(feature14, 9) + poly(feature15, 9) + 
               poly(feature16, 9) + poly(feature17, 9) + poly(feature18, 9) + 
               poly(feature19, 9) + poly(feature20, 9) + poly(feature21, 9), 
             data = training)
qda10 <- qda(target ~ 
               poly(feature1, 10) + poly(feature2, 10) + poly(feature3, 10) + 
               poly(feature4, 10) + poly(feature5, 10) + poly(feature6, 10) + 
               poly(feature7, 10) + poly(feature8, 10) + poly(feature9, 10) + 
               poly(feature10, 10) + poly(feature11, 10) + poly(feature12, 10) + 
               poly(feature13, 10) + poly(feature14, 10) + poly(feature15, 10) + 
               poly(feature16, 10) + poly(feature17, 10) + poly(feature18, 10) + 
               poly(feature19, 10) + poly(feature20, 10) + poly(feature21, 10), 
             data = training)
qda11 <- qda(target ~ 
               feature1 * feature2 * feature3 * feature4 * feature5 + 
               feature6 + feature7 + feature8 + feature9 + feature10 + 
               feature11 + feature12 + feature13 + feature14 + feature15 + 
               feature16 + feature17 + feature18 + feature19 + feature20 + 
               feature21, 
             data = training)
qda12 <- qda(target ~ 
               feature1 + feature2 + feature3 + feature4 + feature5 + 
               feature6 * feature7 * feature8 * feature9 * feature10 + 
               feature11 + feature12 + feature13 + feature14 + feature15 + 
               feature16 + feature17 + feature18 + feature19 + feature20 + 
               feature21, 
             data = training)
qda13 <- qda(target ~ 
               feature1 + feature2 + feature3 + feature4 + feature5 + 
               feature6 + feature7 + feature8 + feature9 + feature10 + 
               feature11 * feature12 * feature13 * feature14 * feature15 + 
               feature16 + feature17 + feature18 + feature19 + feature20 + 
               feature21, 
             data = training)
qda14 <- qda(target ~ 
               feature1 + feature2 + feature3 + feature4 + feature5 + 
               feature6 + feature7 + feature8 + feature9 + feature10 + 
               feature11 + feature12 + feature13 + feature14 + feature15 + 
               feature16 * feature17 * feature18 * feature19 * feature20 + 
               feature21, 
             data = training)
qda15 <- qda(target ~ 
               feature1 + feature2 + feature3 + feature4 + feature5 + 
               feature6 + feature7 + feature8 + feature9 + feature10 + 
               feature11 + feature12 + feature13 + feature14 + feature15 + 
               feature16 + feature17 + feature18 + feature19 + feature20 + 
               feature21, 
             data = training)
detach("package:MASS", unload = TRUE)

# 3. Calculate log loss. 
lda_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test)$posterior[ , 2]) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
(results <- bind_rows(lda_predict(lda01, "lda01"), 
                      lda_predict(lda02, "lda02"), 
                      lda_predict(lda03, "lda03"), 
                      lda_predict(lda04, "lda04"), 
                      lda_predict(lda05, "lda05"), 
                      lda_predict(lda06, "lda06"), 
                      lda_predict(lda07, "lda07"), 
                      lda_predict(lda08, "lda08"), 
                      lda_predict(lda09, "lda09"), 
                      lda_predict(lda10, "lda10"), 
                      lda_predict(lda11, "lda11"), 
                      lda_predict(lda12, "lda12"), 
                      lda_predict(lda13, "lda13"), 
                      lda_predict(lda14, "lda14"), 
                      lda_predict(lda15, "lda15"), 
                      lda_predict(qda01, "qda01"), 
                      lda_predict(qda02, "qda02"), 
                      lda_predict(qda03, "qda03"), 
                      lda_predict(qda04, "qda04"), 
                      lda_predict(qda05, "qda05"), 
                      lda_predict(qda06, "qda06"), 
                      lda_predict(qda07, "qda07"), 
                      lda_predict(qda08, "qda08"), 
                      lda_predict(qda09, "qda09"), 
                      lda_predict(qda10, "qda10"), 
                      lda_predict(qda11, "qda11"), 
                      lda_predict(qda12, "qda12"), 
                      lda_predict(qda13, "qda13"), 
                      lda_predict(qda14, "qda14"), 
                      lda_predict(qda15, "qda15")))
ggplot(results, aes(x = model, y = ll)) + geom_point()
