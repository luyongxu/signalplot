#' ---
#' title: "Load Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Source Load Packages
source("./Bitcoin/1.001 Load Packages.R")

#' # Quandl Functions 
#' The quandl_tidy function is a wrapper around the Quandl function that returns a cleaner tibble.  
Quandl.api_key(Sys.getenv("quandl_key"))
quandl_tidy <- function(code, name) { 
  df <- Quandl(code) %>% 
    mutate(code = code, 
           name = name) %>% 
    arrange(Date) %>% 
    rename(date = Date, value = Value) %>% 
    as_tibble()
  return(df)
}

#' # Bitcoin Exchange Rate Data
print("Downloading bitcoin exchange rate data.")
bitcoin_price <- Quandl("BCHARTS/BITSTAMPUSD") %>% 
  arrange(Date) %>% 
  as_tibble()
colnames(bitcoin_price) <- c("date", "open", "high", "low", "close", "volume_btc", "volume_currency", "weighted_price")

#' # Bitcoin Indicators
#' Data about bitcoin activity, transaction fees, and mining. 
print("Downloading bitcoin indicators.")
code_list <- list(c("BCHAIN/TOTBC", "Total Bitcoins"), 
                  c("BCHAIN/MKTCP", "Bitcoin Market Capitalization"), 
                  c("BCHAIN/NADDU", "Bitcoin Number of Unique Addresses Used"), 
                  c("BCHAIN/ETRAV", "Bitcoin Estimated Transaction Volume"), 
                  c("BCHAIN/ETRVU", "Bitcoin Estimated Transaction Volume USD"), 
                  c("BCHAIN/TRVOU", "Bitcoin USD Exchange Trade Volume"), 
                  c("BCHAIN/NTRAN", "Bitcoin Number of Transactions"), 
                  c("BCHAIN/NTRAT", "Bitcoin Total Number of Transactions"), 
                  c("BCHAIN/NTREP", "Bitcoin Number of Transactions Excluding Popular Addresses"), 
                  c("BCHAIN/NTRBL", "Bitcoin Number of Tansaction per Block"), 
                  c("BCHAIN/ATRCT", "Bitcoin Median Transaction Confirmation Time"), 
                  c("BCHAIN/TRFEE", "Bitcoin Total Transaction Fees"), 
                  c("BCHAIN/TRFUS", "Bitcoin Total Transaction Fees USD"), 
                  c("BCHAIN/CPTRA", "Bitcoin Cost Per Transaction"), 
                  c("BCHAIN/CPTRV", "Bitcoin Cost % of Transaction Volume"), 
                  c("BCHAIN/BLCHS", "Bitcoin api.blockchain Size"), 
                  c("BCHAIN/AVBLS", "Bitcoin Average Block Size"), 
                  c("BCHAIN/TOUTV", "Bitcoin Total Output Volume"), 
                  c("BCHAIN/HRATE", "Bitcoin Hash Rate"), 
                  c("BCHAIN/MIREV", "Bitcoin Miners Revenue"))
bitcoin_data <- tibble()
for (i in seq_along(code_list)) { 
  print(str_c("Downloading data for ", code_list[[i]][1], "."))
  df <- quandl_tidy(code_list[[i]][1], code_list[[i]][2])
  bitcoin_data <- bind_rows(bitcoin_data, df)
}

#' # Google Trends
bitcoin_trend <- gtrendsR::gtrends(keyword = "bitcoin", time = "today+5-y")[["interest_over_time"]] %>% 
  select(date, hits) %>% 
  mutate(date = as.Date(date)) %>% 
  as_tibble()

#' # Clean Workspace
rm(df, code_list, i, quandl_tidy)

