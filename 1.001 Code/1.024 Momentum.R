#' ---
#' title: "Bitcoin Price Response to Macroeconomic and Geopolitical Events"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#' ---

#' # 1. Source Initial Functions and Libraries
source("./1.001 Code/1.023 Initial Functions and Libraries.R")

#' # 2. Download SPY Data
spy <- getSymbolsYahoo("SPY")

#' # 3. Implement Momentum Strategy
train <- spy %>% 
  select(date, adjusted_close) %>% 
  mutate(close_return_01d = adjusted_close / lag(adjusted_close, 1) - 1, 
         close_return_12m = adjusted_close / lag(adjusted_close, 252) - 1, 
         signal = ifelse(close_return_12m > 0, 1, -1))

#' # 4. Calculate returns
train <- train %>% 
  filter(date >= "1994-01-27") %>% 
  mutate(return_buyhold = cumprod(1 + close_return_01d) - 1, 
         return_momentum = cumprod(1 + close_return_01d * signal) - 1)
  
#' # 4. Plot
(p1 <- ggplot(train, aes(x = date, y = adjusted_close, colour = signal)) + 
    geom_line() +  
    labs(title = "S&P 500 Index (SPY) With Momentum Trading Signal", 
         subtitle = "A simple momentum strategy retains the upside while going short during bear markets.", 
         y = "Price", 
         x = "Date") + 
    theme_signalplot())
ggsave(file = "./Plots/1.024 SPY With Momentum Trading Strategy.png", plot = p1, dpi = 300, width = 8, height = 5)

(p2 <- ggplot(train, aes(x = date)) + 
    geom_line(aes(y = return_buyhold), colour = "blue") + 
    geom_line(aes(y = return_momentum), colour = "red") + 
    annotate("text", x = as.Date("2009-01-01"), y = 12, label = "Momentum Strategy", colour = "red") + 
    annotate("text", x = as.Date("2009-01-01"), y = 0, label = "SPY Buy-and-Hold Return", colour = "blue") + 
    labs(title = "Equity Curve of Momentum Strategy vs SPY Return", 
         subtitle = "A simple momentum strategy (16% annualized return) vastly outperforms the S&P 500 (9% annualized return).", 
         y = "Return", 
         x = "Date") + 
    theme_signalplot())
ggsave(file = "./Plots/1.024 Momentum Trading Strategy Equity Curve.png", plot = p2, dpi = 300, width = 8, height = 5)
