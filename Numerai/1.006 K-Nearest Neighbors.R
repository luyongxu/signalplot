source("./Numerai/1.001 Initial Functions and Libraries.R")
library(kknn)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. KNN. 
knn_modified <- function(k_var, kernal_var, distance_var) { 
  df <- data.frame(probability = kknn(target ~ ., 
                                      training, 
                                      test, 
                                      k = k_var, 
                                      kernel = kernal_var, 
                                      distance = distance_var)$fitted.values)
  return(df)
}

for (kernal_var in c("rectangular", "gaussian", "optimal")) { 
  for (distance_var in c(2)) { 
    for (k_var in c(10, 20, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 
                   1250, 1500, 1750, 2000, 2500, 3000)) { 
      print(paste("Training model:", kernal_var, distance_var, k_var))
      assign(paste(kernal_var, distance_var, k_var, sep = ""), 
             knn_modified(k_var = k_var, kernal_var = kernal_var, distance_var = distance_var))
    }
  }
}

# 3. Calculate log loss. 
knn_predict <- function(model_number, model_text) { 
  p <- test %>% 
    mutate(probability = model_number$probability) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
results <- data.frame()
for (kernal_var in c("rectangular", "gaussian", "optimal")) { 
  for (distance_var in c(2)) { 
    for (k_var in c(10, 20, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 
                    1250, 1500, 1750, 2000, 2500, 3000)) { 
      results <- bind_rows(results, 
                           knn_predict(get(paste0(kernal_var, distance_var, k_var)), 
                                       paste0(kernal_var, distance_var, k_var)))
    }
  }
}

ggplot(results, aes(x = model, y = ll)) + geom_point()
# Minimum ll was optimal23000 at 0.6921725.