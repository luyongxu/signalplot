source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Query data
SPY <- getSymbolsYahoo("SPY")
SPXL <- getSymbolsYahoo("SPXL")
SPXS <- getSymbolsYahoo("SPXS")
GDX <- getSymbolsYahoo("GDX")
NUGT <- getSymbolsYahoo("NUGT")
DUST <- getSymbolsYahoo("DUST")

# 2. Prepare data for plotting. 
df1 <- bind_rows(SPY, SPXL) %>% 
  filter(date >= "2015-01-01", date <= "2016-04-30") %>% 
  arrange(ticker, date) %>% 
  group_by(ticker) %>% 
  mutate(indexed_close = adjusted_close / adjusted_close[1])
df2 <- bind_rows(SPXL, SPXS) %>% 
  filter(date >= "2015-01-01", date <= "2016-04-30") %>% 
  arrange(ticker, date) %>% 
  group_by(ticker) %>% 
  mutate(indexed_close = adjusted_close / adjusted_close[1])
df3 <- bind_rows(SPXL, SPXS) %>% 
  filter(date <= "2013-12-31", date >= "2012-06-30") %>% 
  arrange(ticker, date) %>% 
  group_by(ticker) %>% 
  mutate(indexed_close = adjusted_close / adjusted_close[1])
df4 <- GDX %>% 
  filter(date >= "2015-01-01", date <= "2016-04-30") %>% 
  arrange(ticker, date)
df5 <- bind_rows(NUGT, DUST) %>% 
  filter(date >= "2015-01-01", date <= "2016-04-30") %>% 
  arrange(ticker, date) %>% 
  group_by(ticker) %>% 
  mutate(indexed_close = adjusted_close / adjusted_close[1])

# 3. Simulate volatility. 
outcome_master <- c()
for (i in 1:10000) { 
  temp <- data.frame(row_number = 1:1000)
  temp <- temp %>% 
    mutate(daily_return = sample(c(-0.10, 0.10), 1000, replace = TRUE))
  outcome_temp <- data.frame(outcome = 100 * cumprod(1 + temp$daily_return))[1000, 1]
  outcome_master <- c(outcome_master, outcome_temp)
}
outcome_master <- as.data.frame(outcome_master)

# 4. Plots.
(p1 <- ggplot(df1, aes(x = date, y = indexed_close)) + 
  geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "SPY and SPXL Indexed To January 2015", 
       subtitle = "SPY returned +3% over this time period, but SPXL returned -4% because of price decay inherent in all leveraged ETFs.", 
       y = "Indexed Closing Price", 
       x = "Date") + 
  scale_x_date(labels = date_format("%b %Y")) +
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.002 SPY and SPXL Shorting Leveraged ETFs.png", plot = p1, dpi = 300, width = 8, height = 5)

(p2 <- ggplot(df2, aes(x = date, y = indexed_close)) + 
  geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "SPXL and SPXS Indexed To January 2015", 
       subtitle = "Both the long 3x leveraged ETF and the short 3x leveraged ETF had a negative return over this time period.", 
       y = "Indexed Closing Price", 
       x = "Date") + 
  scale_x_date(labels = date_format("%b %Y")) +
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.002 SPXL and SPXS Shorting Leveraged ETFs.png", plot = p2, dpi = 300, width = 8, height = 5)

(p3 <- ggplot(outcome_master, aes(x = outcome_master)) + 
  geom_histogram(fill = "blue") + 
  labs(title = "Ending Price of 10,000 Simulated Leveraged ETFs", 
       subtitle = "Over 95% of simulated leveraged ETFs have an ending price below their initial price.", 
       y = "Number of Leveraged ETFs", 
       x = "Ending Price") + 
  scale_x_continuous(limits = c(-5, 100)) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.002 Leveraged ETF Simulation Shorting Leveraged ETFs.png", plot = p3, dpi = 300, width = 8, height = 5)

(p4 <- ggplot(df3, aes(x = date, y = indexed_close)) + 
  geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "SPXL and SPXS Indexed To June 2012", 
       subtitle = "Shorting equal amounts of these two leveraged ETFs would have returned -90% over this time period.", 
       y = "Indexed Closing Price", 
       x = "Date") +
  scale_x_date(labels = date_format("%b %Y")) +
  geom_hline(yintercept = 1) + 
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.002 SPXL and SPXS Shorting Leveraged ETFs Loss.png", plot = p4, dpi = 300, width = 8, height = 5)

(p5 <- ggplot(df4, aes(x = date, y = close)) + 
  geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "Market Vectors Gold Miners ETF (GDX)", y = "Closing Price") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_alphaplot())
ggsave(file = "./Plots/1.002 GDX.png", plot = p5, dpi = 300, width = 8, height = 5)

(p6 <- ggplot(df5, aes(x = date, y = indexed_close)) + 
  geom_line(aes(colour = ticker), size = 1) + 
  labs(title = "NUGT and DUST Indexed To January 2015", y = "Indexed Closing Price") +
  scale_x_date(labels = date_format("%b %Y")) +
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.002 NUGT and DUST.png", plot = p6, dpi = 300, width = 8, height = 5)
