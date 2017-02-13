# 1. Load libraries.
library(IBrokers)
library(Quandl)
library(quantmod)
library(scales)
library(gridExtra)
library(TTR)
library(jsonlite)
library(xtable)
library(gtable)
library(grid)

# 2. Load Hadley libraries. 
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(httr)
library(readxl)
library(lubridate)
library(reshape2)
library(tidyverse)

# 3. Set working directory
setwd("C:/Users/luyon/Desktop/Projects/SignalPlot")

# 4. Quandl authentication key. Please don't use my key. 
Quandl.api_key("QEayyyTZLrL2TftSWDM8")

# 5. ggplot2 theme.
theme_alphaplot <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size,
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                        debug = FALSE),
    
    axis.line = element_blank(),
    axis.text = element_text(face = "bold", colour = "#535353"),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =  element_blank(),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =  element_text(face = "bold", colour = "#535353", 
                                 margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)),
    axis.title.y = element_text(face = "bold", colour = "#535353", angle = 90,
                                margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)),
    
    legend.background = element_rect(fill = "#F0F0F0", colour = NA),
    legend.spacing = unit(0.2, "cm"),
    legend.key = element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction =  NULL,
    legend.justification = "center",
    legend.box = NULL,
    
    panel.background = element_rect(fill = "#F0F0F0", colour = NA),
    panel.border = element_rect(fill = NA, colour = "#F0F0F0"), 
    panel.grid.major = element_line(colour = "#D0D0D0", size = 0.60),
    panel.grid.minor = element_line(colour = "#D0D0D0", size = 0.60),
    panel.spacing = unit(half_line, "pt"),
    panel.ontop = FALSE,
    
    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    
    plot.background = element_rect(fill = "#F0F0F0", colour = NA),
    plot.title = element_text(face = "bold", hjust = 0, vjust = 2, colour = "#3C3C3C", size = rel(1.5), 
                              margin = margin(b = half_line * 1.2)),
    plot.margin = margin(half_line, half_line * 3, half_line, half_line),
    
    complete = TRUE
  )
}

# 6. Plot two timeseries with their own y-axis using base graphics. 
dualplot <- function(x, 
                     y1, 
                     y2,
                     y1_range = range(y1),
                     y2_range = range(y2), 
                     title = NULL, 
                     x_text = NULL, 
                     y1_text = NULL,
                     y2_text = NULL, 
                     legend_pos = "topright") {
  par(mar = c(3.5, 3.75, 2, 3.75), 
      mgp = c(2, 1, 0), 
      bg = "#F0F0F0", 
      cex.axis = 0.85, 
      cex.lab = 1, 
      cex.main = 1.2,
      col.axis = "#535353",
      col.lab = "#535353",
      fg = "#F0F0F0", 
      col.main = "#3C3C3C", 
      font.axis = 2, 
      font.lab = 2, 
      font.main = 2)
  plot(x, 
       y1, 
       type = "l",
       col = "blue", 
       xlab = x_text,
       ylab = "", 
       yaxt = "n",
       ylim = y1_range, 
       panel.first = abline(v = axis.Date(1, x), col = "#D0D0D0", lty = 2, lwd = 1))
  title(title, cex = 1.1, adj = 0)
  axis(2, col.axis = "blue", las = 2)
  mtext(y1_text, col = "blue", side = 2, line = 2.25)
  grid(NA, NULL, col = "#D0D0D0", lty = 2, lwd = 1)
  par(new = TRUE)
  plot(x, 
       y2, 
       type = "l",
       col = "red", 
       xlab = "", 
       ylab = "", 
       axes = FALSE, 
       ylim = y2_range)
  axis(4, col.axis = "red", las = 2)
  mtext(y2_text, col = "red", side = 4, line = 2.25)
  legend(legend_pos,
         col = c("blue", "red"), 
         text.col = c("blue", "red"), 
         pch = 19, 
         bty = "n", 
         legend = c(y1_text, y2_text), 
         cex = 0.8)
}

# 7. Wrapper function for quantmod's getSymbols. 
getSymbolsYahoo <- function(ticker) { 
  df <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = "1900-01-01")
  df <- as.data.frame(df) %>% 
    mutate(date = index((df)), 
           ticker = ticker)
  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted_close", "date", "ticker")
  return(df)
}

# 8. Get multiple tickers at once. 
getSymbolsYahooMany <- function(ticker_list) {
  df <- data.frame()
  n <- 1
  for (ticker in ticker_list) {
    print(paste0("Ticker number ", n, ": ", ticker))
    df_temp <- tryCatch(getSymbolsYahoo(ticker), 
                        error = function(e) {
                          print(paste0("Could not download data for ", ticker))
                          return(data.frame())
                        })
    df <- bind_rows(df, df_temp)
    n <- n + 1
  }
  return(df)
}


