source("./Numerai/1.001 Initial Functions and Libraries.R")
library(pls)

# 1. Load data. 
training <- read_csv("./Output/Numerai/training.csv")
test <- read_csv("./Output/Numerai/test.csv")

# 2. Principal components analysis.
pcr <- pcr(target ~ ., data = training, validation = "CV")
validationplot(pcr, val.type = "MSEP")

# 3. Predict.
pcr_predict <- function(model_number, model_text, ncomp) { 
  p <- test %>% 
    mutate(probability = predict(pcr, test[ , 1:21], ncomp = ncomp)) %>% 
    select(target, probability)
  results <- data.frame(model = model_text, ll = log_loss(p$target, p$probability))
  return(results)
}
results <- data.frame()
for (ncomp in 1:21) {  
  temp_results <- pcr_predict(pca, paste0("pca", ncomp), ncomp)
  results <- bind_rows(results, temp_results)
}
ggplot(results, aes(x = model, ll)) + geom_point()
