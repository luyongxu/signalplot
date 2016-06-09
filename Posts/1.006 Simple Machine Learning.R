source("./Posts/1.001 Initial Functions and Libraries.R")
library(MASS)

# 1. Query data. 
SPY <- getSymbolsYahoo("SPY") %>% 
  filter(date >= "2000-01-01")

# 2. Create returns and lagged returns.
SPY <- SPY %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         daily_return_sign = factor(ifelse(daily_return >= 0, 1, 0)), 
         daily_return_lag1 = lag(daily_return, 1), 
         daily_return_lag2 = lag(daily_return, 2), 
         daily_return_lag3 = lag(daily_return, 3), 
         daily_return_lag4 = lag(daily_return, 4), 
         daily_return_lag5 = lag(daily_return, 5), 
         daily_return_lag6 = lag(daily_return, 6), 
         daily_return_lag7 = lag(daily_return, 7), 
         daily_return_lag8 = lag(daily_return, 8), 
         daily_return_lag9 = lag(daily_return, 9), 
         daily_return_lag10 = lag(daily_return, 10))

# 3. Plots.
(p01 <- ggplot(SPY, aes(x = daily_return_lag1, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p02 <- ggplot(SPY, aes(x = daily_return_lag2, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p03 <- ggplot(SPY, aes(x = daily_return_lag3, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p04 <- ggplot(SPY, aes(x = daily_return_lag4, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p05 <- ggplot(SPY, aes(x = daily_return_lag5, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p06 <- ggplot(SPY, aes(x = daily_return_lag6, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p07 <- ggplot(SPY, aes(x = daily_return_lag7, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p08 <- ggplot(SPY, aes(x = daily_return_lag8, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p09 <- ggplot(SPY, aes(x = daily_return_lag9, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(p10 <- ggplot(SPY, aes(x = daily_return_lag10, y = daily_return)) + 
  geom_point(alpha = 1/10) + geom_smooth(method = "lm") + 
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + theme_alphaplot(base_size = 10))
(c01 <- grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09))
ggsave(file = "./Plots/1.006 SPY Lagged Returns.png", plot = c01, dpi = 300, width = 8, height = 8)

# 4. Create training and test sets.
SPY_training <- SPY %>%
  filter(date <= "2013-12-31")
SPY_test <- SPY %>% 
  filter(date >= "2014-01-01")

# 5. Train models.
m01 <- glm(daily_return_sign ~ 
             daily_return_lag1, 
           data = SPY_training, 
           family = binomial)
m02 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2, 
           data = SPY_training, 
           family = binomial)
m03 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3, 
           data = SPY_training, 
           family = binomial)
m04 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4, 
           data = SPY_training, 
           family = binomial)
m05 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5, 
           data = SPY_training, 
           family = binomial)
m06 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5 + 
             daily_return_lag6, 
           data = SPY_training, 
           family = binomial)
m07 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5 + 
             daily_return_lag6 + 
             daily_return_lag7, 
           data = SPY_training, 
           family = binomial)
m08 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5 + 
             daily_return_lag6 + 
             daily_return_lag7 + 
             daily_return_lag8, 
           data = SPY_training, 
           family = binomial)
m09 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5 + 
             daily_return_lag6 + 
             daily_return_lag7 + 
             daily_return_lag8 + 
             daily_return_lag9, 
           data = SPY_training, 
           family = binomial)
m10 <- glm(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3 + 
             daily_return_lag4 + 
             daily_return_lag5 + 
             daily_return_lag6 + 
             daily_return_lag7 + 
             daily_return_lag8 + 
             daily_return_lag9 + 
             daily_return_lag10, 
           data = SPY_training, 
           family = binomial)
m11 <- lda(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3, 
           data = SPY_training)
m12 <- qda(daily_return_sign ~ 
             daily_return_lag1 + 
             daily_return_lag2 + 
             daily_return_lag3, 
           data = SPY_training)

# 6. Predict.
SPY_test <- SPY %>% 
  filter(date >= "2014-01-01")
SPY_test <- SPY_test %>% 
  mutate(m01 = predict(m01, SPY_test, type = "response")) %>% 
  mutate(m02 = predict(m02, SPY_test, type = "response")) %>% 
  mutate(m03 = predict(m03, SPY_test, type = "response")) %>% 
  mutate(m04 = predict(m04, SPY_test, type = "response")) %>% 
  mutate(m05 = predict(m05, SPY_test, type = "response")) %>% 
  mutate(m06 = predict(m06, SPY_test, type = "response")) %>% 
  mutate(m07 = predict(m07, SPY_test, type = "response")) %>% 
  mutate(m08 = predict(m08, SPY_test, type = "response")) %>% 
  mutate(m09 = predict(m09, SPY_test, type = "response")) %>% 
  mutate(m10 = predict(m10, SPY_test, type = "response")) %>% 
  mutate(m11 = predict(m11, SPY_test)$posterior[ , 2]) %>% 
  mutate(m12 = predict(m12, SPY_test)$posterior[ , 2]) %>% 
  mutate(m01_pred = ifelse(m01 >= 0.543, 1, 0), 
         m02_pred = ifelse(m02 >= 0.543, 1, 0), 
         m03_pred = ifelse(m03 >= 0.543, 1, 0), 
         m04_pred = ifelse(m04 >= 0.543, 1, 0), 
         m05_pred = ifelse(m05 >= 0.543, 1, 0), 
         m06_pred = ifelse(m06 >= 0.543, 1, 0), 
         m07_pred = ifelse(m07 >= 0.543, 1, 0), 
         m08_pred = ifelse(m08 >= 0.543, 1, 0), 
         m09_pred = ifelse(m09 >= 0.543, 1, 0), 
         m10_pred = ifelse(m10 >= 0.543, 1, 0), 
         m11_pred = ifelse(m11 >= 0.543, 1, 0), 
         m12_pred = ifelse(m12 >= 0.543, 1, 0))
accuracy <- data.frame(model = c("m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10", "m11", "m12"), 
                       accuracy = round(c(mean(SPY_test$daily_return_sign == SPY_test$m01_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m02_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m03_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m04_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m05_pred),
                                    mean(SPY_test$daily_return_sign == SPY_test$m06_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m07_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m08_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m09_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m10_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m11_pred), 
                                    mean(SPY_test$daily_return_sign == SPY_test$m12_pred)), 4))
accuracy
table(SPY_test$daily_return_sign, SPY_test$m03_pred)


# 7. Signal.
SPY_test <- SPY_test %>% 
  mutate(m03_signal = ifelse(m03_pred == 0, 0, 1), 
         m03_return = m03_signal * daily_return, 
         m03_creturn = cumprod(1 + m03_return) - 1, 
         SPY_creturn = cumprod(1 + daily_return) - 1)
combined <- bind_rows(data.frame(date = SPY_test$date, creturn = SPY_test$SPY_creturn, ticker = "SPY"), 
                      data.frame(date = SPY_test$date, creturn = SPY_test$m03_creturn, ticker = "m03_long"))
(p11 <- ggplot(combined, aes(x = date, y = creturn)) + geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "Cumulative Return of SPY Versus Model m03", y = "Cumulative Return", x = "Date") + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous(labels = percent) + 
  theme_alphaplot(base_size = 10))
ggsave(file = "./Plots/1.006 Equity Curve.png", plot = p11, dpi = 300, width = 8, height = 5)
