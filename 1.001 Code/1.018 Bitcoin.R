source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Query bitcoin data. 
GET("https://api.blockchain.info/charts/market-price?format=csv&timespan=all", 
    write_disk("./Raw Data/Bitcoin/bitcoin price.csv", 
    overwrite = TRUE))
bitcoin_raw <- read_csv("./Raw Data/Bitcoin/bitcoin price.csv", col_names = c("date", "price"))

# 2. Load ETF data. 
etf_raw <- read_csv("./Output/ETFs/ETF Prices.csv")
etfmeta_raw <- read_csv("./Output/ETFs/ETF Metadata.csv")

# 3. Clean bitcoin data. 
bitcoin <- bitcoin_raw %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2011-01-01")
  
# 4. Plot data. 
(p1 <- ggplot(bitcoin, aes(x = date, y = price)) + 
  geom_line(colour = "blue") + 
  labs(title = "Bitcoin Price (Linear Scale)", 
       y = "Price", 
       x = "Date") + 
  theme_alphaplot())
(p2 <- ggplot(bitcoin, aes(x = date, y = price)) + 
    geom_line(colour = "blue") + 
    scale_y_log10() + 
    labs(title = "Bitcoin Price (Log Scale)", 
         y = "Price", 
         x = "Date") + 
    theme_alphaplot())
(p3 <- grid.arrange(p1, p2, ncol = 1))
ggsave(file = "./Plots/1.018 Bitcoin Price.png", plot = p3, dpi = 300, width = 8, height = 8)

# 5. Clean ETF data. 
etf <- etf_raw %>% 
  left_join(etfmeta_raw) %>% 
  left_join(bitcoin) %>% 
  mutate(ticker_segment = paste(ticker, fundBasics.segment)) %>% 
  filter(date >= "2011-01-01", 
         date <= "2017-01-31", 
         launchDate <= "2016-01-01", 
         fundBasics.aum.value >= 10000000) %>% 
  group_by(ticker_segment) %>% 
  mutate(weekly_return_etf = adjusted_close / lag(adjusted_close, 5) - 1, 
         weekly_return_bitcoin = price / lag(price, 5) - 1)

# 6. Calculate correlation. 
correlation <- etf %>%  
  group_by(ticker_segment) %>% 
  summarise(correlation = round(cor(weekly_return_bitcoin, weekly_return_etf, use = "pairwise.complete.obs"), 4), 
            aum = mean(fundBasics.aum.value))

# 7. Plots. 
(p4 <- ggplot(correlation, aes(x = correlation)) + 
    geom_histogram(fill = "blue", binwidth = 0.005) + 
    scale_x_continuous(limits = c(-0.2, 0.2)) + 
    labs(title = "Correlation Between Bitcoin and 1,400 ETFs", 
         subtitle = "The correlation between bitcoin and other financial assets is extremely low. Most between -0.1 and +0.1.", 
         y = "Number of ETFs", 
         x = "Correlation") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.018 Bitcoin Correlation With Other Financial Assets.png", plot = p4, dpi = 300, width = 8, height = 5)

# 8. Calculate rolling correlation. 
correlation_rolling <- etf %>% 
  group_by(ticker_segment) %>% 
  filter(length(ticker_segment) > 252) %>% 
  mutate(correlation_rolling = runCor(weekly_return_bitcoin, weekly_return_etf, 252))
correlation_plot <- correlation_rolling %>% 
  filter(ticker %in% c("SPY", "EFA", "VWO", "AGG", "LQD", "VNQ", "XLF", "XLE"))
p5 <- ggplot(correlation_plot, aes(x = date, y = correlation_rolling)) + 
  geom_line(colour = "blue") + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ticker_segment, ncol = 2) + 
  labs(title = "One Year Rolling Correlation Between Bitcoin and Risky Assets", 
       subtitle = "Correlation remains low and inconsistant even when looking at the one-year rolling correlation.", 
       y = "1-Year Rolling Correlation", 
       x = "Correlation") + 
  theme_alphaplot()
ggsave(file = "./Plots/1.018 Bitcoin One Year Correlation With Risky Assets.png", plot = p5, dpi = 300, width = 8, height = 8)

correlation_plot <- correlation_rolling %>% 
  filter(ticker %in% c("GLD", "TIP", "UUP", "FXB", "FXE", "FXF", "FXY", "CEW", "CYB", "IEF"))
p6 <- ggplot(correlation_plot, aes(x = date, y = correlation_rolling)) + 
  geom_line(colour = "blue") + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ticker_segment, ncol = 2) + 
  labs(title = "One Year Rolling Correlation Between Bitcoin and Safe Assets", 
       subtitle = "Even among safe assets, correlation is low, implying that bitcoin isn't affected by many macroeconomic factors.", 
       y = "1-Year Rolling Correlation", 
       x = "Correlation") + 
  theme_alphaplot()
ggsave(file = "./Plots/1.018 Bitcoin One Year Correlation With Safe Assets.png", plot = p6, dpi = 300, width = 8, height = 8)



