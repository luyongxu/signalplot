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
