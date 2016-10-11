source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Query data. 
CMG <- getSymbolsYahoo("CMG") %>% 
  filter(date >= "2006-01-26")
SPY <- getSymbolsYahoo("SPY") %>% 
  filter(date >= "2006-01-26")

# 2. Calculate trading signal for strategy M01. 
CMG <- CMG %>% 
  mutate(signal = ifelse(as.numeric(format(date, "%m")) <= 9, 1, -1), 
         ticker = "M01")
  
# 3. Calculte daily return, signal return, cumulative return, rolling cumulative return, drawdown, 
# and sharpe ratio.  
CMG <- CMG %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         signal_return = daily_return * signal, 
         cum_return = cumprod(1 + signal_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(signal_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 63)
SPY <- SPY %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         cum_return = cumprod(1 + daily_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(daily_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 63)
combined <- bind_rows(CMG, SPY)

# 3. Plot equity curve versus benchmark.
(p1 <- ggplot(combined, aes(x = date, y = cum_return)) + 
  geom_line(aes(colour = ticker)) + 
  labs(title = "Cumulative Return Versus Benchmark", y = "Cumulative Return", x = "Date") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Equity Curve.png", plot = p1, dpi = 300, width = 8, height = 5)

# 4. Plot signal versus underlying. 
(p2 <- ggplot(CMG, aes(x = date, y = signal)) + 
  geom_line(size = 1, colour = "blue") + 
  labs(title = "Trading Signal", y = "Position", x = "Date") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
(p3 <- ggplot(CMG, aes(x = date, y = adjusted_close)) + 
  geom_line(colour = "blue") + 
  labs(title = "CMG Closing Price", y = "Closing Price", x = "Date") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
p2 <- ggplotGrob(p2)
p3 <- ggplotGrob(p3)
p2$widths <- p3$widths
p4 <- grid.arrange(p2, p3, ncol = 1)
ggsave(file = "./Plots/1.005 Trading Signal.png", plot = p4, dpi = 300, width = 8, height = 8)

# 5. Plot underlying with signal colour. 
(p5 <- ggplot(CMG, aes(x = date, y = adjusted_close)) + 
  geom_line(aes(colour = signal)) + 
  scale_colour_gradient(low = "red") +
  labs(title = "CMG Closing Price With Trading Signal", y = "Closing Price", x = "Date") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Price and Trading Signal.png", plot = p5, dpi = 300, width = 8, height = 5)

# 6. Plot rolling returns.
(p6 <- ggplot(combined, aes(x = date, y = cum_return_12m)) + 
  geom_line(aes(colour = ticker)) + 
  labs(title = "Rolling Returns (12 Months)", y = "Return") + 
  scale_y_continuous(labels = percent, limits = c(-1, 1.5)) + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Rolling Returns 12 Months.png", plot = p6, dpi = 300, width = 8, height = 5)
(p7 <- ggplot(combined, aes(x = date, y = cum_return_3m)) + 
  geom_line(aes(colour = ticker)) + 
  labs(title = "Rolling Returns (3 Months)", y = "Return") + 
  scale_y_continuous(labels = percent, limits = c(-0.5, 0.75)) + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Rolling Returns 3 Months.png", plot = p7, dpi = 300, width = 8, height = 5)

# 7. Drawdown.
(p8 <- ggplot(combined, aes(x = date, y = drawdown)) + 
  geom_line(aes(colour = ticker)) + 
  labs(title = "Drawdown", y = "Drawdown Percentage") + 
  scale_y_continuous(labels = percent) + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Drawdown.png", plot = p8, dpi = 300, width = 8, height = 5)

# 8. Sharpe Ratio.
(p9 <- ggplot(combined, aes(x = date, y = sharpe_12m)) + 
  geom_line(aes(colour = ticker)) + 
  labs(title = "Sharpe Ratio (12 Month Rolling)", y = "Sharpe Ratio") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.005 Sharpe Ratio.png", plot = p9, dpi = 300, width = 8, height = 5)
