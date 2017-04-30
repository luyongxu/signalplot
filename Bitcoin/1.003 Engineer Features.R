#' ---
#' title: "Engineer Features"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Source Load Data
source("./Bitcoin/1.002 Load Data.R")

#' # Clean Bitcoin Price
#' Data errors are cleaned by using last observation carried forward. 
print("Engineering features.")
bitcoin_price[bitcoin_price == 0] <- NA
bitcoin_price <- bitcoin_price %>% 
  mutate(low = ifelse(low == 1.5, NA, low)) %>% 
  map_df(na.locf)

#' # Define Target
bitcoin_price <- bitcoin_price %>% 
  mutate(future_return = lead(close, 30) / close - 1, 
         future_return_sign = ifelse(lead(close, 30) / close - 1 > 0, 1, 0))

#' # Calculate Bitcoin Price Change
#' # Only closing prices are plotted.
bitcoin_price <- bitcoin_price %>% 
  mutate(close_01m = close / lag(close, 30) - 1, 
         close_02m = close / lag(close, 60) - 1, 
         close_03m = close / lag(close, 90) - 1, 
         close_04m = close / lag(close, 120) - 1, 
         close_05m = close / lag(close, 150) - 1, 
         close_06m = close / lag(close, 180) - 1, 
         close_07m = close / lag(close, 210) - 1, 
         close_08m = close / lag(close, 240) - 1, 
         close_09m = close / lag(close, 270) - 1, 
         close_10m = close / lag(close, 300) - 1, 
         close_11m = close / lag(close, 330) - 1, 
         close_12m = close / lag(close, 365) - 1)

#' # Spread Bitcoin Data
bitcoin_data_spread <- bitcoin_data %>% 
  select(date, value, code) %>% 
  spread(code, value)
bitcoin_data_spread_ema <- bitcoin_data %>% 
  group_by(code) %>% 
  mutate(value = EMA(value, 14)) %>% 
  select(date, ema = code, value) %>% 
  spread(ema, value, sep = ".")
bitcoin_data_spread_01m <- bitcoin_data %>% 
  group_by(code) %>% 
  mutate(value = value / lag(value, 30) - 1) %>% 
  select(date, change01 = code, value) %>% 
  spread(change01, value, sep = ".")
colnames(bitcoin_data_spread) <- make.names(colnames(bitcoin_data_spread))
colnames(bitcoin_data_spread_ema) <- make.names(colnames(bitcoin_data_spread_ema))
colnames(bitcoin_data_spread_01m) <- make.names(colnames(bitcoin_data_spread_01m))

#' # Combine Bitcoin Price and Bitcoin Data
bitcoin_combined <- bitcoin_price %>% 
  left_join(bitcoin_data_spread) %>% 
  left_join(bitcoin_data_spread_ema) %>% 
  left_join(bitcoin_data_spread_01m)
