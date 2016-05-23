source("./Posts/1.001 Initial Functions and Libraries.R")

# Load libraries.
library(randomForest)

# Load raw data.
predictions <- read.csv("./Raw Data/Numerai/numerai_example_predictions.csv")
tournament <- read.csv("./Raw Data/Numerai/numerai_tournament_data.csv")
training <- read.csv("./Raw Data/Numerai/numerai_training_data.csv") %>% 
  mutate(row_number = row_number())

# Plot data.
training_melt <- training %>% 
  melt() %>% 
  filter(variable != "row_number", 
         variable != "target")
ggplot(training_melt, aes(x = value)) + geom_histogram(binwidth = 0.01) + facet_wrap(~ variable) 

# Logarithmic loss.
MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}

# Train data.
m01 <- lm(target ~ 
            feature1 + feature2 + feature3 + feature4 + feature5 + 
            feature6 + feature7 + feature8 + feature9 + feature10 + 
            feature11 + feature12 + feature13 + feature14 + feature15 + 
            feature16 + feature17 + feature18 + feature19 + feature20 + 
            feature21, 
          data = training)
m02 <- glm(target ~ 
             feature1 + feature2 + feature3 + feature4 + feature5 + 
             feature6 + feature7 + feature8 + feature9 + feature10 + 
             feature11 + feature12 + feature13 + feature14 + feature15 + 
             feature16 + feature17 + feature18 + feature19 + feature20 + 
             feature21, 
           data = training, 
           family = "binomial")
m03 <- randomForest(as.factor(target) ~ 
                      feature1 + feature2 + feature3 + feature4 + feature5 + 
                      feature6 + feature7 + feature8 + feature9 + feature10 + 
                      feature11 + feature12 + feature13 + feature14 + feature15 + 
                      feature16 + feature17 + feature18 + feature19 + feature20 + 
                      feature21, 
           data = training, 
           importance = TRUE, 
           ntree = 500)
m04 <- lm(target ~ 
            feature2*feature3*feature6*feature10*feature12*feature14*feature15*feature17, 
          data = training)


# Predict data.
p01 <- tournament %>% 
  mutate(probability = predict(m01, tournament)) %>% 
  select(t_id, probability)
write.csv(p01, "./Raw Data/Numerai/Predictions/p01.csv", row.names = FALSE)
p02 <- tournament %>% 
  mutate(probability = predict(m02, tournament, type = "response")) %>% 
  select(t_id, probability)
write.csv(p02, "./Raw Data/Numerai/Predictions/p02.csv", row.names = FALSE)
p03 <- tournament %>% 
  mutate(probability = predict(m03, tournament, type = "prob")[, 2]) %>% 
  select(t_id, probability)
write.csv(p03, "./Raw Data/Numerai/Predictions/p03.csv", row.names = FALSE)
p04 <- tournament %>% 
  mutate(probability = predict(m04, tournament)) %>% 
  select(t_id, probability)
write.csv(p04, "./Raw Data/Numerai/Predictions/p04.csv", row.names = FALSE)
p05 <- data.frame(t_id = p01$t_id, 
                  probability = 
                    1/3*p01$probability + 
                    1/3*p02$probability + 
                    1/3*p04$probability)
write.csv(p05, "./Raw Data/Numerai/Predictions/p05.csv", row.names = FALSE)

