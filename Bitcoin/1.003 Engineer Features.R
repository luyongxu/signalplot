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
  map_df(na.locf)

#' # Define Target
bitcoin_price <- bitcoin_price %>% 
  mutate(future_return = lead(close, 30) / close - 1, 
         future_return_sign = ifelse(future_return > 0, 1, 0))

#' # Calculate Bitcoin Price Change
bitcoin_price <- bitcoin_price %>% 
  mutate(close_01d = close / lag(close, 1) - 1, 
         close_02d = close / lag(close, 2) - 1, 
         close_03d = close / lag(close, 3) - 1, 
         close_04d = close / lag(close, 4) - 1, 
         close_05d = close / lag(close, 5) - 1, 
         close_06d = close / lag(close, 6) - 1, 
         close_01w = close / lag(close, 7) - 1, 
         close_02w = close / lag(close, 14) - 1, 
         close_03w = close / lag(close, 21) - 1, 
         close_01m = close / lag(close, 30) - 1, 
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

#' # Rolling Standard Deviation of Lagged Return
bitcoin_price <- bitcoin_price %>% 
  mutate(sd_01d = roll_sdr(close_01d, 1), 
         sd_02d = roll_sdr(close_01d, 2), 
         sd_03d = roll_sdr(close_01d, 3), 
         sd_04d = roll_sdr(close_01d, 4), 
         sd_05d = roll_sdr(close_01d, 5), 
         sd_06d = roll_sdr(close_01d, 6), 
         sd_01w = roll_sdr(close_01d, 7), 
         sd_02w = roll_sdr(close_01d, 14), 
         sd_03w = roll_sdr(close_01d, 21), 
         sd_01m = roll_sdr(close_01d, 30), 
         sd_02m = roll_sdr(close_01d, 60), 
         sd_03m = roll_sdr(close_01d, 90), 
         sd_04m = roll_sdr(close_01d, 120), 
         sd_05m = roll_sdr(close_01d, 150), 
         sd_06m = roll_sdr(close_01d, 180), 
         sd_07m = roll_sdr(close_01d, 210), 
         sd_08m = roll_sdr(close_01d, 240), 
         sd_09m = roll_sdr(close_01d, 270), 
         sd_10m = roll_sdr(close_01d, 300), 
         sd_11m = roll_sdr(close_01d, 330), 
         sd_12m = roll_sdr(close_01d, 365))

#' # Spread Bitcoin Blockchain Data
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
  mutate(value = EMA(value, 3) / lag(value, 30) - 1) %>% 
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

#' # Split Into Train and Test Sets
train <- bitcoin_price %>% 
  filter(date <= "2015-12-31")
test <- bitcoin_price %>% 
  filter(date >= "2016-01-01")
