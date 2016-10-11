source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Import data.
SPY <- getSymbolsYahoo("SPY")
AGG <- getSymbolsYahoo("AGG")

# 2. Calculate returns.
SPY <- SPY %>% 
  mutate(daily_return_SPY = adjusted_close / lag(adjusted_close, 1) - 1) %>% 
  select(date, daily_return_SPY)
AGG <- AGG %>% 
  mutate(daily_return_AGG = adjusted_close / lag(adjusted_close, 1) - 1) %>% 
  select(date, daily_return_AGG) %>% 
  filter(!(is.na(daily_return_AGG)))
combined <- AGG %>% 
  left_join(SPY) %>% 
  mutate(daily_return_TP = 0.60 * daily_return_SPY + 0.40 * daily_return_AGG, 
         daily_return_RP = (0.20 * daily_return_SPY + 0.80 * daily_return_AGG) * 1.33, 
         cum_return_AGG = cumprod(1 + daily_return_AGG) - 1, 
         cum_return_SPY = cumprod(1 + daily_return_SPY) - 1, 
         cum_return_TP = cumprod(1 + daily_return_TP) - 1, 
         cum_return_RP = cumprod(1 + daily_return_RP) - 1)

# 3. Plots. 
p1_data <- combined %>% 
  select(date, cum_return_AGG, cum_return_SPY, cum_return_TP) %>% 
  melt(id.vars = "date")
(p1 <- ggplot(p1_data, aes(x = date, y = value, colour = variable)) + 
  geom_line() + 
  labs(title = "Returns of Equities, Bonds, and a 60/40 Portfolio", 
       subtitle = "The 60% equity, 40% bond portfolio's volatility is still dominated by the equity portion.", 
       y = "Return", 
       x = "Date") + 
  theme_alphaplot() + 
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  scale_colour_discrete("Asset Class", 
                        breaks = c("cum_return_AGG", "cum_return_SPY", "cum_return_TP"), 
                        labels = c("Bonds (AGG)", "Equities (SPY)", "60% Equities, \n40% Bonds")))
ggsave(file = "./Plots/1.012 Returns of Equities, Bonds, and 60 40 Portfolio.png", 
       plot = p1, dpi = 300, width = 8, height = 5)

p2_data <- combined %>% 
  select(date, cum_return_AGG, cum_return_SPY, cum_return_RP) %>% 
  melt(id.vars = "date")
(p2 <- ggplot(p2_data, aes(x = date, y = value, colour = variable)) + 
  geom_line() + 
  labs(title = "Returns of Equities, Bonds, and a Risk Parity Portfolio", 
       subtitle = "The risk parity portfolio volatility is not dominated by a single asset.", 
       y = "Return", 
       x = "Date") + 
  theme_alphaplot() + 
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  scale_colour_discrete("Asset Class", 
                        breaks = c("cum_return_AGG", "cum_return_SPY", "cum_return_RP"), 
                        labels = c("Bonds (AGG)", "Equities (SPY)", "27% Equities, \n106% Bonds")))
ggsave(file = "./Plots/1.012 Returns of Equities, Bonds, and Risk Parity Portfolio.png", 
       plot = p2, dpi = 300, width = 8, height = 5)

# 4. Simple risk parity implementation. 
rm(list = ls())
