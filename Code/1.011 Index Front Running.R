source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Query data.
sp500 <- read_csv("./Raw Data/S&P 500 Changes/S&P 500 Changes.csv") %>% 
  select(added_ticker, announced_date, added_date) %>% 
  mutate(announced_date = mdy(announced_date), 
         added_date = mdy(added_date))
prices_raw <- getSymbolsYahooMany(sp500$added_ticker) %>% 
  group_by(ticker) %>% 
  mutate(n = row_number())

# 2. Extract -10 to +10 days around announced date and index prices. 
index <- sp500 %>% 
  left_join(select(prices_raw, date, ticker, adjusted_close, n), 
            by = c("added_ticker" = "ticker", "added_date" = "date")) %>% 
  rename(index_close = adjusted_close, 
         index_n = n)
prices <- prices_raw %>% 
  left_join(index, by = c("ticker" = "added_ticker")) %>% 
  mutate(day = n - index_n + 1, 
         price = adjusted_close / index_close) %>% 
  filter(day >= -10, day <= 10) %>% 
  mutate(announced_price = ifelse(date == announced_date, price, NA))
aggregate <- prices %>% 
  group_by(day) %>% 
  summarise(price = mean(price), 
            announced_price = mean(announced_price, na.rm = TRUE))

# 3. Plots.
p1_data <- prices %>% 
  mutate(ticker_factor = factor(ticker)) %>% 
  filter(as.numeric(ticker_factor) <= 32)
p2_data <- prices %>% 
  mutate(ticker_factor = factor(ticker)) %>% 
  filter(as.numeric(ticker_factor) > 32)
(p1 <- ggplot(p1_data, aes(x = day, y = price)) + 
  geom_line() + 
  geom_point(aes(y = announced_price), colour = "red") + 
  facet_wrap(~ticker, ncol = 4) + 
  coord_cartesian(ylim = c(0.90, 1.10)) + 
  geom_vline(xintercept = 0, colour = "red") + 
  labs(title = "Stock Prices in Response to Being Added to the S&P 500 Index", 
       subtitle = "The red dot indicates the day the stock was announced to be added. The red line indicates the day the stock was added.", 
       y = "Price", 
       x = "Days (0 Indicates Day Added to Index)") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.011 Index Front Running A1.png", plot = p1, dpi = 300, width = 8, height = 10)

(p2 <- ggplot(p2_data, aes(x = day, y = price)) + 
  geom_line() + 
  geom_point(aes(y = announced_price), colour = "red") + 
  facet_wrap(~ticker, ncol = 4) + 
  coord_cartesian(ylim = c(0.90, 1.10)) + 
  geom_vline(xintercept = 0, colour = "red") + 
  labs(title = "Stock Prices in Response to Being Added to the S&P 500 Index", 
       subtitle = "The red dot indicates the day the stock was announced to be added. The red line indicates the day the stock was added.", 
       y = "Price", 
       x = "Days (0 Indicates Day Added to Index)") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.011 Index Front Running B1.png", plot = p2, dpi = 300, width = 8, height = 10)

(p3 <- ggplot(aggregate, aes(x = day, y = price)) + 
  geom_line(size = 1)  + 
  geom_vline(xintercept = 0, colour = "red") + 
  labs(title = "Stock Prices in Response to Being Added to the S&P 500 Index", 
       subtitle = "On average, stock prices rise in anticipation of the stock being added, then falls after it becomes part of the index.", 
       y = "Price", 
       x = "Days (0 Indicates Day Added to Index)") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.011 Index Front Running C.png", plot = p3, dpi = 300, width = 8, height = 5)

