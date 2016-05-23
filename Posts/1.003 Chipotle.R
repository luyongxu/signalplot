source("./Posts/1.001 Initial Functions and Libraries.R")

# 1. Query data.
CMG <- getSymbolsYahoo("CMG")
income <- read_excel("./CMG/Raw Data/CMG.xlsx", sheet = "Income")

# 2. Calculate returns.
CMG <- CMG %>% 
  arrange(Date) %>% 
  mutate(daily_return = c(rep(NA, 1), diff(log(Adjusted_Close), lag = 1)), 
         m1_return = c(rep(NA, 21), diff(log(Adjusted_Close), lag = 21)), 
         m3_return = c(rep(NA, 63), diff(log(Adjusted_Close), lag = 63)), 
         m6_return = c(rep(NA, 126), diff(log(Adjusted_Close), lag = 126)), 
         m12_return = c(rep(NA, 252), diff(log(Adjusted_Close), lag = 252)), 
         m1_return_smoothed = (log(Adjusted_Close) - SMA(log(Adjusted_Close) , n = 32)) * (12/(1/21)) / ((32 - 1) / 2), 
         m3_return_smoothed = (log(Adjusted_Close) - SMA(log(Adjusted_Close) , n = 95)) * (12/(1/21)) / ((95 - 1) / 2), 
         m6_return_smoothed = (log(Adjusted_Close) - SMA(log(Adjusted_Close) , n = 189)) * (12/(1/21)) / ((189 - 1) / 2), 
         m12_return_smoothed = (log(Adjusted_Close) - SMA(log(Adjusted_Close) , n = 378)) * (12/(1/21)) / ((378 - 1) / 2))

# 3. Create plots.
ggplotreturn <- function(dataVar, xVar, yVar) { 
  ggplot(dataVar, aes_string(x = xVar, y = yVar)) + 
    geom_line(colour = "#00BFC4") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + 
    theme_alphaplot()
}
gglinePlot <- function(dataVar, xData, yData) { 
  ggplot(dataVar, aes_string(x = xData, y = yData)) + 
    geom_line(colour = "#00BFC4", size = 1.5) + 
    geom_point() + 
    scale_y_continuous(labels = percent) + 
    theme_alphaplot()
}

# 4. Plot returns.
(p1 <- ggplotreturn(CMG, "Date", y = "Adjusted_Close") + 
  labs(title = "Chipotle Mexican Grill (CMG) Closing Price", y = "Closing Price") + 
  scale_y_continuous(limits = c(0, 800)))
(p2 <- ggplotreturn(CMG, "Date", "m1_return_smoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "1 Month Log Return Annualized", y = "Return"))
(p3 <- ggplotreturn(CMG, "Date", "m3_return_smoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "3 Month Log Return Annualized", y = "Return"))
(p4 <- ggplotreturn(CMG, "Date", "m6_return_smoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "6 Month Log Return Annualized", y = "Return"))
(p5 <- ggplotreturn(CMG, "Date", y = "m12_return_smoothed") + 
  geom_line(colour = "#F8766D") + 
  labs(title = "12 Month Log Return Annualized", y = "Return"))
combined <- arrangeGrob(p1, p2, p3, p4, p5, ncol = 1)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
ggsave(file = "./CMG/CMG.png", plot = combined, width = 8, height = 18)

# 5. Clean income statement.
income <- income %>% 
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>% 
  filter(Date <= "2016-12-30")

# 6. Plot income statement.
(p6 <- gglinePlot(income, "Date", "Revenue") + 
  labs(title = "Revenue (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p7 <- gglinePlot(income, "Date", "FoodBeverageAndPackaging") + 
  labs(title = "Food, Beverage, and Packaging Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p8 <- gglinePlot(income, "Date", "Labor") + 
  labs(title = "Labor Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p9 <- gglinePlot(income, "Date", "Occupancy") + 
  labs(title = "Occupancy Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p10 <- gglinePlot(income, "Date", "OtherOperatingCosts") + 
  labs(title = "Other Operating Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p11 <- gglinePlot(income, "Date", "GeneralAndAdministrativeExpenses") + 
  labs(title = "General and Administrative Expenses (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p12 <- gglinePlot(income, "Date", "DepreciationAndAmortization") + 
  labs(title = "Depreciation and Amortization (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p13 <- gglinePlot(income, "Date", "PreOpeningCosts") + 
  labs(title = "Pre-Opening Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p14 <- gglinePlot(income, "Date", "LossOnDisposalOfAssets") + 
  labs(title = "Loss on Disposal of Assets (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(p15 <- gglinePlot(income, "Date", "TotalOperatingExpenses") + 
  labs(title = "Total Operating Expenses (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
combined <- arrangeGrob(p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol = 2)
grid.arrange(p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol = 2)
ggsave(file = "./CMG/Income Level.png", plot = combined, width = 10, height = 18)


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


# Load data.
income <- read_excel("./CMG/Raw Data/CMG.xlsx", sheet = "Income")
income <- income %>% 
  mutate(Date = as.Date(Date, origin = "1899-12-30"), 
         RevenueP =                    Revenue / Revenue, 
         FoodCostsP =                  FoodCosts / Revenue, 
         LaborCostsP =                 LaborCosts / Revenue, 
         OccupancyCostsP =             OccupancyCosts / Revenue, 
         OtherCostsP =                 OtherCosts / Revenue, 
         GAExpensesP =                 GAExpenses / Revenue, 
         DepreciationAmortizationP =   DepreciationAmortization / Revenue, 
         PreOpeningCostsP =            PreOpeningCosts / Revenue, 
         LossOnDisposalP =             LossOnDisposal / Revenue, 
         TotalOperatingExpensesP =     TotalOperatingExpenses / Revenue, 
         IncomeFromOperationsP =       IncomeFromOperations / Revenue, 
         InterestOtherIncomeNetP =     InterestOtherIncomeNet / Revenue, 
         IncomeBeforeTaxesP =          IncomeBeforeTaxes / Revenue, 
         ProvisionForTaxesP =          ProvisionForTaxes / Revenue, 
         NetIncomeP =                  NetIncome / Revenue, 
         RevenueQ = Revenue / lag(Revenue, 1) - 1, 
         RevenueY = Revenue / lag(Revenue, 4) - 1, 
         FoodCostsQ = FoodCosts / lag(FoodCosts, 1) - 1, 
         FoodCostsY = FoodCosts / lag(FoodCosts, 4) - 1, 
         LaborCostsQ = LaborCosts / lag(LaborCosts, 1) - 1, 
         LaborCostsY = LaborCosts / lag(LaborCosts, 4) - 1, 
         OccupancyCostsQ = OccupancyCosts / lag(OccupancyCosts, 1) - 1, 
         OccupancyCostsY = OccupancyCosts / lag(OccupancyCosts, 4) - 1, 
         GAExpensesQ = GAExpenses / lag(GAExpenses, 1 ) - 1, 
         GAExpensesY = GAExpenses / lag(GAExpenses, 4) - 1, 
         OtherCostsQ = OtherCosts / lag(OtherCosts, 1) - 1, 
         OtherCostsY = OtherCosts / lag(OtherCosts, 4) - 1) %>% 
  filter(Date >= "2005-01-01")

# Plot data.
gglinePlot <- function(dataVar, xData, yData) { 
  ggplot(dataVar, aes_string(x = xData, y = yData)) + 
    geom_line(colour = "#00BFC4", size = 1.5) + 
    geom_point() + 
    scale_y_continuous(labels = percent) + 
    theme_alphaplot()
}

(r1 <- gglinePlot(income, "Date", "Revenue") + labs(title = "Revenue (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(r2 <- gglinePlot(income, "Date", "RevenueP") + labs(title = "Revenue (% of Revenue)", y = "% of Revenue"))
(r3 <- gglinePlot(income, "Date", "RevenueY") + labs(title = "Revenue (Y/Y Growth)", y = "Growth"))


(f1 <- gglinePlot(income, "Date", "FoodCosts") + labs(title = "Food Costs (Level)", y = "Dollars (Thousands)") + 
  scale_y_continuous(labels = dollar))
(f2 <- gglinePlot(income, "Date", "FoodCostsP") + labs(title = "Food Costs (% of Revenue)", y = "% of Revenue"))
(f3 <- gglinePlot(income, "Date", "FoodCostsY") + labs(title = "Food Costs (Y/Y Growth)", y = "Growth"))



(p2 <- gglinePlot(income, "Date", "FoodCostsP") + labs(title = "Food and Beverage Costs", y = "% of Revenue"))
(p3 <- gglinePlot(income, "Date", "LaborCostsP") + labs(title = "Labor Costs", y = "% of Revenue"))
(p4 <- gglinePlot(income, "Date", "OccupancyCostsP") + labs(title = "Occupancy Costs", y = "% of Revenue"))
(p5 <- gglinePlot(income, "Date", "GAExpensesP") + labs(title = "General and Administrative Expenses", y = "% of Revenue"))
(p6 <- gglinePlot(income, "Date", "OtherCostsP") + labs(title = "Other Costs", y = "% of Revenue"))
(p7 <- gglinePlot(income, "Date", "IncomeFromOperationsP") + labs(title = "Income From Operations", y = "% of Revenue"))

combined <- arrangeGrob(p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 2)
grid.arrange(p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 2)


# Load data.
revenue <- read_excel("./CMG/Raw Data/CMG.xlsx", sheet = "Restaurant")
revenue <- revenue %>% 
  mutate(Date = as.Date(Date, origin = "1899-12-30"), 
         CompOutlookAvg = (CompOutlookLow +CompOutlookHigh) / 2) %>% 
  filter(Date >= "2005-01-01")

# Plot data.
gglinePlot <- function(dataVar, xData, yData) { 
  ggplot(dataVar, aes_string(x = xData, y = yData)) + 
    geom_line(colour = "#00BFC4", size = 1.5) + 
    geom_point() + 
    scale_y_continuous(labels = percent) + 
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("years")) + 
    theme_alphaplot()
}

(c1 <- gglinePlot(revenue, "Date", "ComparableRestaurantSales") + 
  labs(title = "Comparable Restaurant Sales", y = "Comparable Restaurant Sales") + 
  geom_line(data = revenue, aes(x = Date, y = CompOutlookAvg), colour = "#F8766D", size = 1.5) + 
  geom_point(data = revenue, aes(x = Date, y = CompOutlookAvg)))



