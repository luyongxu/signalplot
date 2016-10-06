source("./Numerai/1.001 Initial Functions and Libraries.R")
library(car)

# 1. Load raw data.
predictions <- read.csv("./Raw Data/Numerai/example_predictions.csv")
tournament <- read.csv("./Raw Data/Numerai/numerai_tournament_data.csv")
raw_training <- read.csv("./Raw Data/Numerai/numerai_training_data.csv")

# 2. Split into training and test data. 
set.seed(1)
sample <- sample(1:nrow(raw_training), size = floor(0.75 * nrow(raw_training)))
training <- raw_training[sample, ]
test <- raw_training[-sample, ]

# 3. Plot data.
training_plot <- training[sample(1:nrow(training), size = 200), ]
pdf(file = "./Output/Numerai/Plots/Scatterplot Matrix.pdf", width = 20, height = 20)
scatterplotMatrix(~ feature1 + feature2 + feature3 + feature4 + feature5 + 
                    feature6 + feature7 + feature8 + feature9 + feature10 + 
                    feature11 + feature12 + feature13 + feature14 + feature15 + 
                    feature16 + feature17 + feature18 + feature19 + feature20 + 
                    feature21 | target, 
                   data = training_plot)
training_melt <- training %>% 
  mutate(row_number = row_number()) %>% 
  melt() %>% 
  filter(variable != "row_number", 
         variable != "target")
ggplot(training_melt, aes(x = value)) + geom_histogram(binwidth = 0.01) + facet_wrap(~ variable)
dev.off()
detach("package:car", unload = TRUE)

# 4. Save data. 
write_csv(training, "./Output/Numerai/training.csv")
write_csv(test, "./Output/Numerai/test.csv")

