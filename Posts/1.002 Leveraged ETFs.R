source("./Posts/1.001 Initial Functions and Libraries.R")

# Query data
SPY <- getSymbolsYahoo("SPY")
SPXL <- getSymbolsYahoo("SPXL")
SPXS <- getSymbolsYahoo("SPXS")
GDX <- getSymbolsYahoo("GDX")
NUGT <- getSymbolsYahoo("NUGT")
DUST <- getSymbolsYahoo("DUST")

# Prepare data for plotting. 
df1 <- bind_rows(SPY, SPXL) %>% 
  filter(Date >= "2015-01-01") %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  mutate(Indexed_Close = Adjusted_Close / Adjusted_Close[1])
df2 <- bind_rows(SPXL, SPXS) %>% 
  filter(Date >= "2015-01-01") %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  mutate(Indexed_Close = Adjusted_Close / Adjusted_Close[1])
df3 <- bind_rows(SPXL, SPXS) %>% 
  filter(Date <= "2013-12-31", Date >= "2012-06-01") %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  mutate(Indexed_Close = Adjusted_Close / Adjusted_Close[1])
df4 <- GDX %>% 
  filter(Date >= "2015-01-01") %>% 
  arrange(Ticker, Date)
df5 <- bind_rows(NUGT, DUST) %>% 
  filter(Date >= "2015-01-01") %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  mutate(Indexed_Close = Adjusted_Close / Adjusted_Close[1])

# Simulate volatility. 
outcome_master <- c()
for (i in 1:10000) { 
  temp <- data.frame(row_number = 1:1000)
  temp <- temp %>% 
    mutate(daily_return = sample(c(-0.10, 0.10), 1000, replace = TRUE))
  outcome_temp <- data.frame(outcome = 100*cumprod(1 + temp$daily_return))[1000, 1]
  outcome_master <- c(outcome_master, outcome_temp)
}
outcome_master <- as.data.frame(outcome_master)

# Plots.
(p1 <- ggplot(df1, aes(x = Date, y = Indexed_Close)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "SPY and SPXL Indexed To January 2015", y = "Indexed Closing Price") + 
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 SPY and SPXL.png", plot = p1, dpi = 450, width = 8, height = 5)

(p2 <- ggplot(df2, aes(x = Date, y = Indexed_Close)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "SPXL and SPXS Indexed To January 2015", y = "Indexed Closing Price") +
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 SPXL and SPXS.png", plot = p2, dpi = 450, width = 8, height = 5)

(p3 <- ggplot(outcome_master, aes(x = outcome_master)) + 
  geom_histogram(fill = "blue") + 
  labs(title = "Ending Price of 10,000 Simulated Leveraged ETFs", y = "Number of Leveraged ETFs", x = "Ending Price") + 
  xlim(0, 100) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 Leveraged ETF Simulation.png", plot = p3, dpi = 450, width = 8, height = 5)

(p4 <- ggplot(df3, aes(x = Date, y = Indexed_Close)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "SPXL and SPXS Indexed To June 2012", y = "Indexed Closing Price") +
  geom_hline(yintercept = 1) + 
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 SPXL and SPXS 2.png", plot = p4, dpi = 450, width = 8, height = 5)

(p5 <- ggplot(df4, aes(x = Date, y = Close)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "Market Vectors Gold Miners ETF (GDX)", y = "Closing Price") +
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 GDX.png", plot = p5, dpi = 450, width = 8, height = 5)

(p6 <- ggplot(df5, aes(x = Date, y = Indexed_Close)) + 
  geom_line(aes(colour = Ticker), size = 1) + 
  labs(title = "NUGT and DUST Indexed To January 2015", y = "Indexed Closing Price") +
  geom_hline(yintercept = 1) + 
  theme_alphaplot())
ggsave(file = "./Posts/Plots/1.002 NUGT and DUST.png", plot = p6, dpi = 450, width = 8, height = 5)