source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
data_raw <- getSymbolsYahooMany(c("AQRNX", "TLO", "SCHP", "SPY", "GLD", "EFA", "VWO", "EMB", "GSG"))

# 2. Calculate daily returns
data <- data_raw %>% 
  group_by(ticker) %>% 
  mutate(daily_return = adjusted_close / lag(adjusted_close, 1) - 1) %>% 
  filter(date >= "2010-10-01") %>% 
  dcast(date ~ ticker, value.var = "daily_return")

# 3. Train model. 
m01 <- lm(AQRNX ~ 0 + GLD + SCHP + SPY + TLO + EFA + VWO + EMB + GSG, data = data)
m02 <- lm(AQRNX ~ 0 + GLD + SCHP + SPY + TLO + EFA + VWO + EMB + GSG, data = data %>% filter(date <= "2013-01-01"))
m03 <- lm(AQRNX ~ 0 + GLD + SCHP + SPY + TLO + EFA + VWO + EMB + GSG, data = data %>% filter(date >= "2013-01-01"))         
summary(m01)
sum(coef(m01))

# 4. Plot returns. 
returns <- data %>% 
  mutate(AQRNX = ifelse(is.na(AQRNX), 0, AQRNX), 
         AQR = cumprod(1 + AQRNX) - 1, 
         m01 = cumprod(1 + predict(m01, data)) - 1) %>% 
  melt(id.vars = "date") %>% 
  filter(variable %in% c("AQR", "m01"))
(p1 <- ggplot(returns, aes(x = date, y = value, colour = variable)) + 
  geom_line() + 
  labs(title = "Reverse Engineering AQR's Risk Parity Strategy", 
       subtitle = "There is a 96% correlation between AQR's risk parity strategy and the synthetic strategy.", 
       y = "Return", 
       x = "Date") + 
  theme_alphaplot() + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  scale_colour_discrete("Strategy", 
                        breaks = c("AQR", "m01"), 
                        labels = c("AQR", "Synthetic")))
ggsave(file = "./Plots/1.014 AQR Risk Parity.png", plot = p1, dpi = 300, width = 8, height = 5)

# 5. AQR Portolio Holdings. 
holdings <- read_csv("./Raw Data/Risk Parity/AQR Risk Parity Fund Holdings.csv") %>% 
  select(c(1, 2, 5))
print(xtable(holdings), type = "html")


