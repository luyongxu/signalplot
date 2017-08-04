source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
data_raw <- getSymbolsYahooMany(c("AQRNX", "VTI", "SCHF", "VWO", "TLT", "EMLC", "EMB", "SCHP", "IAU", "DBC", 
                                  "LTPZ", "LQD", "VCLT", "HYG", "WIP", "BNDX", "IGOV", "BWX", "AGG"))

# 2. Calculate daily returns
data <- data_raw %>% 
  group_by(ticker) %>% 
  mutate(daily_return = adjusted_close / lag(adjusted_close, 1) - 1) %>% 
  filter(date >= "2010-10-01") %>% 
  dcast(date ~ ticker, value.var = "daily_return")

# 3. Train model. 
m01 <- lm(AQRNX ~ 0 + VTI + SCHF + VWO + TLT + EMLC + EMB + SCHP + IAU + DBC, data = data)
summary(m01)
sum(coef(m01))
m02 <- lm(AQRNX ~ 0 + VTI + SCHF + VWO + TLT + EMLC + EMB + SCHP + IAU + DBC + WIP, data = data)
summary(m02)
sum(coef(m02))
m03 <- lm(AQRNX ~ 0 + VTI + SCHF + VWO + TLT + EMLC + EMB + SCHP + IAU + DBC + WIP + BWX, data = data)
summary(m03)
sum(coef(m03))


# 4. Plot returns. 
data <- data %>%  
  mutate(AQRNX = ifelse(is.na(AQRNX), 0, AQRNX), 
         BNDX = ifelse(is.na(BNDX), 0, BNDX))
returns <- data %>% 
  mutate(AQR = cumprod(1 + AQRNX) - 1, 
         m01 = cumprod(1 + predict(m01, data)) - 1,
         m02 = cumprod(1 + predict(m02, data)) - 1, 
         m03 = cumprod(1 + predict(m03, data)) - 1) %>% 
  melt(id.vars = "date") %>% 
  filter(variable %in% c("AQR", "m01", "m02", "m03"))
(p1 <- ggplot(returns, aes(x = date, y = value, colour = variable)) + 
  geom_line() + 
  labs(title = "Reverse Engineering AQR's Risk Parity Strategy", 
       subtitle = "There is a 96% correlation between AQR's risk parity strategy and the synthetic strategy.", 
       y = "Return", 
       x = "Date") + 
  theme_alphaplot() + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y"))




