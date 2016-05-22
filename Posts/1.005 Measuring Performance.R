library(quantmod)
library(dplyr)
library(ggplot2)

# 1. Query data. 
getSymbolsYahoo <- function(ticker) { 
  df <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = "1900-01-01")
  df <- as.data.frame(df) %>% 
    mutate(Date = index((df)), 
           Ticker = ticker)
  colnames(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted_Close", "Date", "Ticker")
  return(df)
}

CMG <- getSymbolsYahoo("CMG")
SPY <- getSymbolsYahoo("SPY")

# 2. Calculate trading signal, daily return, and cumulative return.
CMG <- CMG %>% 
  filter(Date >= "2006-01-26") %>% 
  mutate(signal = ifelse(as.numeric(format(Date, "%m")) <= 9, 1, -1), 
         daily_return = ifelse(row_number() == 1, 0, Adjusted_Close / lag(Adjusted_Close, 1) - 1), 
         signal_return = daily_return * signal, 
         cumulative_return = cumprod(1 + signal_return) - 1, 
         Ticker = "M01")
SPY <- SPY %>% 
  filter(Date >= "2006-01-26") %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, Adjusted_Close / lag(Adjusted_Close, 1) - 1), 
         cumulative_return = cumprod(1 + daily_return) - 1)

# 3. Plot equity curve versus benchmark.
combined <- bind_rows(CMG, SPY)
(p1 <- ggplot(combined, aes(x = Date, y = cumulative_return)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "Cumulative Return Versus Benchmark", y = "Cumulative Return") + 
  geom_hline(yintercept = 0) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.005 Equity Curve.png", plot = p1, dpi = 300, width = 8, height = 5)

