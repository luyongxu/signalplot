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
               
               
    