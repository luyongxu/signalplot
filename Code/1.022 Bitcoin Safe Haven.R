#' ---
#' title: "Bitcoin Price Response to Macroeconomic and Geopolitical Events"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Source Initial Functions and Libraries
source("./Code/1.021 Initial Functions and Libraries.R")

#' # CBOE Volatility Index (VIX)
d_vix <- Quandl("CBOE/VIX") %>% 
  arrange(Date) %>% 
  as_tibble()
colnames(d_vix) <- c("date", "open", "high", "low", "close")
ggplot(d_vix, aes(x = date, y = close)) + geom_line()

#' # Bitcoin Exchange Rate 
d_bitcoin <- Quandl("BCHARTS/BITSTAMPUSD") %>% 
  arrange(Date) %>% 
  as_tibble() %>% 
  mutate(ticker = "Bitcoin")
colnames(d_bitcoin) <- c("date", "open", "high", "low", "close", 
                       "volume_btc", "volume_currency", "weighted_price","ticker")
ggplot(d_bitcoin, aes(x = date, y = close)) + geom_line()

#' # Gold
d_gld <- getSymbolsYahoo("GLD")
ggplot(d_gld, aes(x = date, y = adjusted_close)) + geom_line()

#' # S&P 500
d_spy <- getSymbolsYahoo("SPY")
ggplot(d_spy, aes(x = date, y = adjusted_close)) + geom_line()

#' # US 10-Year Yield
d_us10yr <- Quandl("FRED/DGS10") %>% 
  arrange(Date) %>% 
  as_tibble() %>% 
  mutate(ticker = "US 10-Year Yield")
colnames(d_us10yr) <- c("date", "close", "ticker")
ggplot(d_us10yr, aes(x = date, y = close)) + geom_line()

#' # Combined
d_combined <- bind_rows(d_bitcoin %>% select(date, close, ticker), 
                      d_gld %>% select(date, close = adjusted_close, ticker), 
                      d_spy %>% select(date, close = adjusted_close, ticker), 
                      d_us10yr %>% select(date, close, ticker))

#' # calculate Biggest One-Day Changes in VIX Since 2012
d_vix_sorted <- d_vix %>% 
  filter(date >= "2012-01-01") %>% 
  mutate(close_change = close / lag(close) - 1) %>% 
  arrange(desc(close_change))
d_gld_sorted <- d_gld %>% 
  filter(date >= "2012-01-01") %>% 
  mutate(close_change = close / lag(close) - 1) %>% 
  arrange(desc(close_change))


#' # Plot Bitcoin Function 
plot_bitcoin <- function(df, event, title, subtitle) { 
  event <- as.Date(event)
  plot <- df %>% 
    mutate(dot = ifelse(date == event, close, NA)) %>% 
    filter(date >= event - weeks(4), date <= event + weeks(4)) %>% 
    ggplot(aes(x = date, y = close)) + 
    geom_line(colour = "blue") + 
    geom_point(colour = "blue", size = 1) + 
    geom_point(aes(x = date, y = dot), colour = "red") + 
    geom_vline(xintercept = as.numeric(event), linetype = 2) + 
    theme_alphaplot() + 
    labs(title = title, 
         subtitle = subtitle, 
         y = "Price", 
         x = "Date") + 
    scale_x_date(date_breaks = "14 days", date_labels =  "%b %d") + 
    facet_wrap(~ ticker, ncol = 2, scales = "free")
  return(plot)
}

#' # Bitcoin and SPY Response To Brexit
(p1 <- plot_bitcoin(d_combined, 
                    "2016-06-24", 
                    "Bitcoin's Response to Brexit", 
                    "Bitcoin rallied sharply on the day of and weeks prior to the UK's referendum to leave the EU."))
ggsave(file = "./Plots/1.021 Bitcoin Brexit.png", plot = p1, dpi = 300, width = 8, height = 5)

#' # Bitcoin and SPY Response to Greek Default Fears
(p2 <- plot_bitcoin(d_combined, 
                    "2015-06-29",
                    "Bitcoin's Response to Greek Default Fears", 
                    "Bitcoin responded positively to increased probability of Greek default and exit from Euro."))
ggsave(file = "./Plots/1.021 Bitcoin Greek Default.png", plot = p2, dpi = 300, width = 8, height = 5)

#' # Bitcoin's Response to Cypriot Banking Crisis
(p3 <- plot_bitcoin(d_combined, 
                    "2013-03-16",
                    "Bitcoin's Response to Cypriot Banking Crisis", 
                    "Bitcoin rallied sharply in response to forced haircuts on bank deposits and capital controls."))
ggsave(file = "./Plots/1.021 Bitcoin Cypriot Banking Crisis.png", plot = p3, dpi = 300, width = 8, height = 5)
