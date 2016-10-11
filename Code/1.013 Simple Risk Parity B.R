source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
data_raw <- getSymbolsYahooMany(c("SPY", "AGG", "GLD"))

# 2. Calculate returns.
data <- data_raw %>% 
  group_by(ticker) %>% 
  mutate(daily_return = adjusted_close / lag(adjusted_close, 1) - 1, 
         sd_ma12 = runSD(daily_return, n = 252) * sqrt(252)) %>% 
  filter(date >= "2006-01-01") %>% 
  select(date, ticker, daily_return, sd_ma12)

# 3. Calculate risk parity weightings. 
sd_ma12 <- data %>% 
  dcast(date ~ ticker, value.var = "sd_ma12") %>% 
  mutate(total = (1 / AGG) + (1 / SPY) + (1 / GLD), 
         AGG_weight = (1 / AGG) / total, 
         SPY_weight = (1 / SPY) / total,  
         GLD_weight = (1 / GLD) / total) %>% 
  select(date, AGG_weight, SPY_weight, GLD_weight)

# 4. Plots
(p1 <- ggplot(sd_ma12 %>% melt(id.vars = "date"), aes(x = date, y = value, colour = variable)) + 
  geom_line() + 
  labs(title = "Weights of Equities, Bonds, and Gold in Risk Parity Portfolio", 
       subtitle = "This risk parity strategy allocates roughly 70% to bonds, 15% to equities, and 15% to gold.", 
       y = "Weight", 
       x = "Date") + 
  theme_alphaplot() + 
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(limits = c(0, 1)) + 
  scale_colour_discrete("Asset Class", 
                        breaks = c("AGG_weight", "SPY_weight", "GLD_weight"), 
                        labels = c("Bonds", "Equities", "Gold")))
ggsave(file = "./Plots/1.013 Risk Parity Weights.png", plot = p1, dpi = 300, width = 8, height = 5)
