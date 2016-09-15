source("./Numerai/1.001 Initial Functions and Libraries.R")
library(splines)
library(gam)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Generalized additive models.
gam01 <- lm(target ~ 
              feature1 + feature2 + feature3 + feature4 + feature5 + 
              feature6 + feature7 + feature8 + feature9 + feature10 + 
              feature11 + feature12 + feature13 + feature14 + feature15 + 
              feature16 + feature17 + feature18 + feature19 + feature20 + 
              feature21, 
            data = training)
breaks <- seq(0, 1, by = 0.01)
gam02 <- lm(target ~ 
              cut(feature1, breaks) + cut(feature2, breaks) + cut(feature3, breaks) + 
              cut(feature4, breaks) + cut(feature5, breaks) + cut(feature6, breaks) + 
              cut(feature7, breaks) + cut(feature8, breaks) + cut(feature9, breaks) + 
              cut(feature10, breaks) + cut(feature11, breaks) + cut(feature12, breaks) + 
              cut(feature13, breaks) + cut(feature14, breaks) + cut(feature15, breaks) + 
              cut(feature16, breaks) + cut(feature17, breaks) + cut(feature18, breaks) + 
              cut(feature19, breaks) + cut(feature20, breaks) + cut(feature21, breaks), 
            data = training)
gam03 <- lm(target ~ 
              ns(feature1, 1) + ns(feature2, 1) + ns(feature3, 1) + 
              ns(feature4, 1) + ns(feature5, 1) + ns(feature6, 1) + 
              ns(feature7, 1) + ns(feature8, 1) + ns(feature9, 1) + 
              ns(feature10, 1) + ns(feature11, 1) + ns(feature12, 1) + 
              ns(feature13, 1) + ns(feature14, 1) + ns(feature15, 1) + 
              ns(feature16, 1) + ns(feature17, 1) + ns(feature18, 1) + 
              ns(feature19, 1) + ns(feature20, 1) + ns(feature21, 1), 
            data = training)
gam04 <- lm(target ~ 
              ns(feature1, 2) + ns(feature2, 2) + ns(feature3, 2) + 
              ns(feature4, 2) + ns(feature5, 2) + ns(feature6, 2) + 
              ns(feature7, 2) + ns(feature8, 2) + ns(feature9, 2) + 
              ns(feature10, 2) + ns(feature11, 2) + ns(feature12, 2) + 
              ns(feature13, 2) + ns(feature14, 2) + ns(feature15, 2) + 
              ns(feature16, 2) + ns(feature17, 2) + ns(feature18, 2) + 
              ns(feature19, 2) + ns(feature20, 2) + ns(feature21, 2), 
            data = training)
gam05 <- lm(target ~ 
              ns(feature1, 3) + ns(feature2, 3) + ns(feature3, 3) + 
              ns(feature4, 3) + ns(feature5, 3) + ns(feature6, 3) + 
              ns(feature7, 3) + ns(feature8, 3) + ns(feature9, 3) + 
              ns(feature10, 3) + ns(feature11, 3) + ns(feature12, 3) + 
              ns(feature13, 3) + ns(feature14, 3) + ns(feature15, 3) + 
              ns(feature16, 3) + ns(feature17, 3) + ns(feature18, 3) + 
              ns(feature19, 3) + ns(feature20, 3) + ns(feature21, 3), 
            data = training)
gam06 <- lm(target ~ 
              ns(feature1, 4) + ns(feature2, 4) + ns(feature3, 4) + 
              ns(feature4, 4) + ns(feature5, 4) + ns(feature6, 4) + 
              ns(feature7, 4) + ns(feature8, 4) + ns(feature9, 4) + 
              ns(feature10, 4) + ns(feature11, 4) + ns(feature12, 4) + 
              ns(feature13, 4) + ns(feature14, 4) + ns(feature15, 4) + 
              ns(feature16, 4) + ns(feature17, 4) + ns(feature18, 4) + 
              ns(feature19, 4) + ns(feature20, 4) + ns(feature21, 4), 
            data = training)
gam07 <- lm(target ~ 
              ns(feature1, 5) + ns(feature2, 5) + ns(feature3, 5) + 
              ns(feature4, 5) + ns(feature5, 5) + ns(feature6, 5) + 
              ns(feature7, 5) + ns(feature8, 5) + ns(feature9, 5) + 
              ns(feature10, 5) + ns(feature11, 5) + ns(feature12, 5) + 
              ns(feature13, 5) + ns(feature14, 5) + ns(feature15, 5) + 
              ns(feature16, 5) + ns(feature17, 5) + ns(feature18, 5) + 
              ns(feature19, 5) + ns(feature20, 5) + ns(feature21, 5), 
            data = training)
gam08 <- gam(target ~ 
              s(feature1, 1) + s(feature2, 1) + s(feature3, 1) + 
              s(feature4, 1) + s(feature5, 1) + s(feature6, 1) + 
              s(feature7, 1) + s(feature8, 1) + s(feature9, 1) + 
              s(feature10, 1) + s(feature11, 1) + s(feature12, 1) + 
              s(feature13, 1) + s(feature14, 1) + s(feature15, 1) + 
              s(feature16, 1) + s(feature17, 1) + s(feature18, 1) + 
              s(feature19, 1) + s(feature20, 1) + s(feature21, 1), 
            data = training)
gam09 <- gam(target ~ 
              s(feature1, 2) + s(feature2, 2) + s(feature3, 2) + 
              s(feature4, 2) + s(feature5, 2) + s(feature6, 2) + 
              s(feature7, 2) + s(feature8, 2) + s(feature9, 2) + 
              s(feature10, 2) + s(feature11, 2) + s(feature12, 2) + 
              s(feature13, 2) + s(feature14, 2) + s(feature15, 2) + 
              s(feature16, 2) + s(feature17, 2) + s(feature18, 2) + 
              s(feature19, 2) + s(feature20, 2) + s(feature21, 2), 
            data = training)
gam10 <- gam(target ~ 
              s(feature1, 3) + s(feature2, 3) + s(feature3, 3) + 
              s(feature4, 3) + s(feature5, 3) + s(feature6, 3) + 
              s(feature7, 3) + s(feature8, 3) + s(feature9, 3) + 
              s(feature10, 3) + s(feature11, 3) + s(feature12, 3) + 
              s(feature13, 3) + s(feature14, 3) + s(feature15, 3) + 
              s(feature16, 3) + s(feature17, 3) + s(feature18, 3) + 
              s(feature19, 3) + s(feature20, 3) + s(feature21, 3), 
            data = training)
gam11 <- gam(target ~ 
              s(feature1, 4) + s(feature2, 4) + s(feature3, 4) + 
              s(feature4, 4) + s(feature5, 4) + s(feature6, 4) + 
              s(feature7, 4) + s(feature8, 4) + s(feature9, 4) + 
              s(feature10, 4) + s(feature11, 4) + s(feature12, 4) + 
              s(feature13, 4) + s(feature14, 4) + s(feature15, 4) + 
              s(feature16, 4) + s(feature17, 4) + s(feature18, 4) + 
              s(feature19, 4) + s(feature20, 4) + s(feature21, 4), 
            data = training)
gam12 <- gam(target ~ 
              s(feature1, 5) + s(feature2, 5) + s(feature3, 5) + 
              s(feature4, 5) + s(feature5, 5) + s(feature6, 5) + 
              s(feature7, 5) + s(feature8, 5) + s(feature9, 5) + 
              s(feature10, 5) + s(feature11, 5) + s(feature12, 5) + 
              s(feature13, 5) + s(feature14, 5) + s(feature15, 5) + 
              s(feature16, 5) + s(feature17, 5) + s(feature18, 5) + 
              s(feature19, 5) + s(feature20, 5) + s(feature21, 5), 
            data = training)

# 3. Calculate log loss. 
gam_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = predict(model_number, test)) %>% 
    mutate(probability = ifelse(is.na(probability), 0.50, probability)) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
(results <- bind_rows(gam_predict(gam01, "gam01"), 
                      gam_predict(gam02, "gam02"), 
                      gam_predict(gam03, "gam03"), 
                      gam_predict(gam04, "gam04"), 
                      gam_predict(gam05, "gam05"), 
                      gam_predict(gam06, "gam06"), 
                      gam_predict(gam07, "gam07"), 
                      gam_predict(gam08, "gam08"), 
                      gam_predict(gam09, "gam09"), 
                      gam_predict(gam10, "gam10"),
                      gam_predict(gam11, "gam11"), 
                      gam_predict(gam12, "gam12")))
ggplot(results, aes(x = model, y = ll)) + geom_point()