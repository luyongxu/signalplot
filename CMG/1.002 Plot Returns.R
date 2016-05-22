# Query Quandl data
Quandl.auth("QEayyyTZLrL2TftSWDM8")
stockPrice <- Quandl("WIKI/CMG")
colnames(stockPrice) <- c("Date", "Open", "High", "Low", "Close", "Volume", "ExDivided", "SplitRatio", "Adj.Open", 
                          "Adj.High", "Adj.Low", "Adj.Close", "Adj.Volume")
stockPrice <- stockPrice %>% 
  arrange(Date) %>% 
  mutate(dailyReturn = c(rep(NA, 1), diff(log(Adj.Close), lag = 1)), 
         m1Return = c(rep(NA, 21), diff(log(Adj.Close), lag = 21)), 
         m3Return = c(rep(NA, 63), diff(log(Adj.Close), lag = 63)), 
         m6Return = c(rep(NA, 126), diff(log(Adj.Close), lag = 126)), 
         m12Return = c(rep(NA, 252), diff(log(Adj.Close), lag = 252)), 
         m1ReturnSmoothed = (log(Adj.Close) - SMA(log(Adj.Close) , n = 32)) * (12/(1/21)) / ((32 - 1) / 2), 
         m3ReturnSmoothed = (log(Adj.Close) - SMA(log(Adj.Close) , n = 95)) * (12/(1/21)) / ((95 - 1) / 2),         
         m6ReturnSmoothed = (log(Adj.Close) - SMA(log(Adj.Close) , n = 189)) * (12/(1/21)) / ((189 - 1) / 2),         
         m12ReturnSmoothed = (log(Adj.Close) - SMA(log(Adj.Close) , n = 378)) * (12/(1/21)) / ((378 - 1) / 2))

# Create plots
ggplotreturn <- function(dataVar, xVar, yVar) { 
  ggplot(dataVar, aes_string(x = xVar, y = yVar)) + 
    geom_line(colour = "#00BFC4") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + 
    theme_alphaplot()
}

(p1 <- ggplotreturn(stockPrice, "Date", y = "Adj.Close") + 
  labs(title = "Chipotle Mexican Grill (CMG) Closing Price", y = "Closing Price") + 
  scale_y_continuous(limits = c(0, 800)))

(p2 <- ggplotreturn(stockPrice, "Date", "m1ReturnSmoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "1 Month Log Return Annualized", y = "Return"))

(p3 <- ggplotreturn(stockPrice, "Date", "m3ReturnSmoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "3 Month Log Return Annualized", y = "Return"))

(p4 <- ggplotreturn(stockPrice, "Date", "m6ReturnSmoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "6 Month Log Return Annualized", y = "Return"))

(p5 <- ggplotreturn(stockPrice, "Date", y = "m12ReturnSmoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "12 Month Log Return Annualized", y = "Return"))

combined <- arrangeGrob(p1, p2, p3, p4, p5, ncol = 1)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
ggsave(file = "./CMG/CMG.png", plot = combined, width = 10, height = 18)

# Calculate annualized change over various periods
