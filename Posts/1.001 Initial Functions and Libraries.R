# Load libraries.
library(IBrokers)
library(Quandl)
library(quantmod)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)
library(readxl)

# Set working directory
setwd("C:/Users/Coke/Desktop/Projects/AlphaPlot/")

# Quandl authentication key.
Quandl.api_key("QEayyyTZLrL2TftSWDM8")

# Load functions.
theme_alphaplot <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain", colour = "black", size = base_size,
                                      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                                      debug = FALSE),
    
    axis.line =          element_blank(),
    axis.text =          element_text(face = "bold", colour = "#535353"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =         element_blank(),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(face = "bold", colour = "#535353", 
                                      margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)),
    axis.title.y =       element_text(face = "bold", colour = "#535353", angle = 90,
                                      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)),
    
    legend.background =  element_rect(fill = "#F0F0F0", colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = "#F0F0F0", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "#F0F0F0"), 
    panel.grid.major =   element_line(colour = "#D0D0D0", size = 0.60),
    panel.grid.minor =   element_line(colour = "#D0D0D0", size = 0.60),
    panel.margin =       unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,
    
    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    
    plot.background =    element_rect(fill = "#F0F0F0", colour = NA),
    plot.title =         element_text(face = "bold", hjust = 0, vjust = 2, colour = "#3C3C3C", size = rel(1.5), 
                                      margin = margin(b = half_line * 1.2)),
    plot.margin =        margin(half_line, half_line * 3, half_line, half_line),
    
    complete = TRUE
  )
}

lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

getSymbolsYahoo <- function(ticker) { 
  df <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = "1900-01-01")
  df <- as.data.frame(df) %>% 
    mutate(Date = index((df)), 
           Ticker = ticker)
  colnames(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted_Close", "Date", "Ticker")
  return(df)
}


