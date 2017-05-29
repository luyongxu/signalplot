#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Source Load Data
source("./Bitcoin/1.003 Engineer Features.R")

#' # Plot Bitcoin Price Data
plot_bitcoin <- function(y, title) { 
  p <- ggplot(bitcoin_price, aes_string(x = "date", y = y)) + 
          geom_line(colour = "blue") + 
    labs(title = title) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  return(p)
}
plot_bitcoin("close", "Bitcoin Close")
plot_bitcoin("high", "Bitcoin High")
plot_bitcoin("low", "Bitcoin Low")
plot_bitcoin("volume_btc", "Bitcoin Volume (BTC)")
plot_bitcoin("volume_currency","Bitcoin Volume (USD)")
plot_bitcoin("weighted_price", "Bitcoin Weighted Price")

#' # Plot Bitcoin Price Change
plot_bitcoin2 <- function(y, title) { 
  p <- ggplot(bitcoin_price, aes_string(x = "date", y = y)) + 
    geom_line(colour = "red") + 
    geom_hline(yintercept = 0) + 
    coord_cartesian(ylim = c(-0.5, 2.0)) +
    labs(title = title,  x = "") + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  return(p)
}
plot_bitcoin2("close_01m", "Bitcoin 1 Month Change")
plot_bitcoin2("close_03m", "Bitcoin 3 Month Change")
plot_bitcoin2("close_06m", "Bitcoin 6 Month Change")
plot_bitcoin2("close_12m", "Bitcoin 12 Month Change")

#' # Plot Bitcoin Price and Bitcoin Price Change
gridExtra::grid.arrange(plot_bitcoin("close", "Bitcoin Close"), 
                        plot_bitcoin2("close_01m", "Bitcoin 1 Month Change"), 
                        ncol = 1, widths = 2)
gridExtra::grid.arrange(plot_bitcoin("close", "Bitcoin Close"), 
                        plot_bitcoin2("close_03m", "Bitcoin 3 Month Change"), 
                        ncol = 1, widths = 2)
gridExtra::grid.arrange(plot_bitcoin("close", "Bitcoin Close"), 
                        plot_bitcoin2("close_06m", "Bitcoin 6 Month Change"), 
                        ncol = 1, widths = 2)

#' # Plot Bitcoin Data
plot_bitcoin3 <- function(y, title) { 
  p <- ggplot(bitcoin_combined, aes_string(x = "date")) + 
    geom_line(aes_string(y = y), colour = "blue") + 
    geom_line(aes_string(y = str_c("ema", y, sep = ".")), colour = "red") + 
    labs(title = title) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  return(p)
}
plot_bitcoin3("BCHAIN.TOTBC", "Total Bitcoins")
plot_bitcoin3("BCHAIN.MKTCP", "Bitcoin Market Capitalization")
plot_bitcoin3("BCHAIN.NADDU", "Bitcoin Number of Unique Addresses Used")
plot_bitcoin3("BCHAIN.ETRAV", "Bitcoin Estimated Transaction Volume")
plot_bitcoin3("BCHAIN.ETRVU", "Bitcoin Estimated Transaction Volume USD")
plot_bitcoin3("BCHAIN.TRVOU", "Bitcoin USD Exchange Trade Volume")
plot_bitcoin3("BCHAIN.NTRAN", "Bitcoin Number of Transactions")
plot_bitcoin3("BCHAIN.NTRAT", "Bitcoin Total Number of Transactions")
plot_bitcoin3("BCHAIN.NTREP", "Bitcoin Number of Transactions Excluding Popular Addresses")
plot_bitcoin3("BCHAIN.NTRBL", "Bitcoin Number of Tansaction per Block")
plot_bitcoin3("BCHAIN.ATRCT", "Bitcoin Median Transaction Confirmation Time")
plot_bitcoin3("BCHAIN.TRFEE", "Bitcoin Total Transaction Fees")
plot_bitcoin3("BCHAIN.TRFUS", "Bitcoin Total Transaction Fees USD")
plot_bitcoin3("BCHAIN.CPTRA", "Bitcoin Cost Per Transaction")
plot_bitcoin3("BCHAIN.CPTRV", "Bitcoin Cost % of Transaction Volume")
plot_bitcoin3("BCHAIN.BLCHS", "Bitcoin api.blockchain Size")
plot_bitcoin3("BCHAIN.AVBLS", "Bitcoin Average Block Size")
plot_bitcoin3("BCHAIN.TOUTV", "Bitcoin Total Output Volume")
plot_bitcoin3("BCHAIN.HRATE", "Bitcoin Hash Rate")
plot_bitcoin3("BCHAIN.MIREV", "Bitcoin Miners Revenue")

#' # Plot Bitcoin Data One Month Change
plot_bitcoin4 <- function(y, title) { 
  p <- ggplot(bitcoin_combined, aes_string(x = "date")) + 
    geom_line(aes_string(y = str_c("change01", y, sep = ".")), colour = "blue") + 
    geom_hline(yintercept = 0) + 
    coord_cartesian(ylim = c(-0.5, 2)) + 
    labs(title = str_c(title, " 1 Month Change")) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  return(p)
}
plot_bitcoin4("BCHAIN.TOTBC", "Total Bitcoins")
plot_bitcoin4("BCHAIN.MKTCP", "Bitcoin Market Capitalization")
plot_bitcoin4("BCHAIN.NADDU", "Bitcoin Number of Unique Addresses Used")
plot_bitcoin4("BCHAIN.ETRAV", "Bitcoin Estimated Transaction Volume")
plot_bitcoin4("BCHAIN.ETRVU", "Bitcoin Estimated Transaction Volume USD")
plot_bitcoin4("BCHAIN.TRVOU", "Bitcoin USD Exchange Trade Volume")
plot_bitcoin4("BCHAIN.NTRAN", "Bitcoin Number of Transactions")
plot_bitcoin4("BCHAIN.NTRAT", "Bitcoin Total Number of Transactions")
plot_bitcoin4("BCHAIN.NTREP", "Bitcoin Number of Transactions Excluding Popular Addresses")
plot_bitcoin4("BCHAIN.NTRBL", "Bitcoin Number of Tansaction per Block")
plot_bitcoin4("BCHAIN.ATRCT", "Bitcoin Median Transaction Confirmation Time")
plot_bitcoin4("BCHAIN.TRFEE", "Bitcoin Total Transaction Fees")
plot_bitcoin4("BCHAIN.TRFUS", "Bitcoin Total Transaction Fees USD")
plot_bitcoin4("BCHAIN.CPTRA", "Bitcoin Cost Per Transaction")
plot_bitcoin4("BCHAIN.CPTRV", "Bitcoin Cost % of Transaction Volume")
plot_bitcoin4("BCHAIN.BLCHS", "Bitcoin api.blockchain Size")
plot_bitcoin4("BCHAIN.AVBLS", "Bitcoin Average Block Size")
plot_bitcoin4("BCHAIN.TOUTV", "Bitcoin Total Output Volume")
plot_bitcoin4("BCHAIN.HRATE", "Bitcoin Hash Rate")
plot_bitcoin4("BCHAIN.MIREV", "Bitcoin Miners Revenue")

#' # Plot Bitcoin Price and Bitcoin Data
ggplot(bitcoin_combined, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") 
ggplot(bitcoin_combined, aes(x = date)) + 
  geom_line(aes(y = BCHAIN.NTRAN), colour = "blue") + 
  geom_line(aes(y = EMA(BCHAIN.NTRAN, 30)), colour = "red")
ggplot(bitcoin_combined, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = ema.BCHAIN.NTRAN / 230), colour = "red")
ggplot(bitcoin_combined, aes(x = date)) + 
  geom_line(aes(y = lead(close_01m, 30)), colour = "blue") + 
  geom_line(aes(y = EMA(change01.BCHAIN.NTRAN, 14)), colour = "red") + 
  coord_cartesian(ylim = c(-0.5, 2))

