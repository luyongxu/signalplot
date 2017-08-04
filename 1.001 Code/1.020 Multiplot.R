source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Load data.
GLD <- getSymbolsYahoo("GLD")
SLV <- getSymbolsYahoo("SLV")

# 2. Combine data. 
combined <- GLD %>% 
  rename(GLD = adjusted_close) %>% 
  select(date, GLD) %>% 
  left_join(SLV) %>% 
  rename(SLV = adjusted_close) %>% 
  select(date, GLD, SLV) %>% 
  filter(date >= "2011-01-01")

# 3. Test
png("./Plots/1.020 Two Timeseries Test.png", width = 8, height = 5, units = "in", res = 300)
dualplot(x = combined$date, 
         y1 = combined$GLD, 
         y2 = combined$SLV, 
         y1_range = c(100, 180), 
         y2_range = c(10, 39), 
         title = "Gold Prices Versus Silver Prices", 
         x_text = "Date", 
         y1_text = "Gold Price", 
         y2_text = "Silver Price", 
         legend_pos = "topright")
dev.off()
