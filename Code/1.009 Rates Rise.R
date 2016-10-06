source("./Posts/1.001 Initial Functions and Libraries.R")

# 1. Download 10 year.
us10yr <- Quandl("FRED/DGS10") %>% 
  rename(Date = DATE, Value = VALUE) %>% 
  mutate(Country = "US")
uk10yr <- Quandl("BOE/IUDMNZC") %>% 
  mutate(Country = "UK")
de10yr <- Quandl("BUNDESBANK/BBK01_WT1010") %>% 
  mutate(Country = "Germany")
jp10yr <- Quandl("MOFJ/INTEREST_RATE_JAPAN_10Y") %>% 
  mutate(Country = "Japan")
df10yr <- bind_rows(us10yr, uk10yr, de10yr, jp10yr) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(Value = Value / 100)

# 2. Plot 10 year. 
(p1 <- ggplot(df10yr, aes(x = Date, y = Value)) + 
  geom_line(aes(color = Country), size = 1) + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "Global 10-Year Bond Yields", 
       subtitle = "Bond yields increased dramatically in 2013 as investors feared a tightening of monetary policy.", 
       y = "10-Year Bond Yield") + 
  scale_y_continuous(labels = percent) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.009 Global 10-Year Bond Yields.png", plot = p1, dpi = 300, width = 8, height = 5)

# 3. Load ETF metadata. 
etfmeta_raw <- read_csv("./Output/ETFs/ETF Metadata.csv")
etfmeta <- etfmeta_raw %>% 
  filter(launchDate <= "2013-05-01", 
         is.na(analysis.leveragedFactor)) %>% 
  group_by(fundBasics.segment) %>% 
  arrange(fundBasics.segment, desc(fundBasics.aum.value)) %>% 
  filter(row_number() == 1) %>% 
  mutate(tickersegment = paste(ticker, fundBasics.segment)) %>% 
  select(fundBasics.segment, ticker, tickersegment)

# 4. Calculate change in U.S. 10-year. 
us10yr_change <- us10yr %>% 
  mutate(us10yr_change = Value - lead(Value, 1)) %>% 
  rename(date = Date) %>% 
  select(date, us10yr_change)

# 5. ETF Return.
etfprice <- read_csv("./Output/ETFs/ETF Prices.csv")
etfprice_return <- etfprice %>% 
  filter(date >= "2013-05-01" & date <= "2013-12-31") %>% 
  inner_join(etfmeta) %>% 
  group_by(tickersegment) %>% 
  mutate(return = round(adjusted_close[length(adjusted_close)] / adjusted_close[1] - 1, 3)) %>% 
  filter(row_number() == 1) %>% 
  select(ticker, fundBasics.segment, tickersegment, return)

# 6. ETF Correlation.
etfprice_corr <- etfprice %>% 
  mutate(etf_return = adjusted_close / lag(adjusted_close) - 1) %>% 
  filter(date >= "2013-05-01" & date <= "2013-12-31") %>% 
  inner_join(etfmeta) %>% 
  left_join(us10yr_change) %>% 
  group_by(fundBasics.segment, tickersegment) %>% 
  summarise(correlation = round(cor(etf_return, us10yr_change, use = "pairwise.complete.obs"), 2))

# 7. Combined.
combined <- etfprice_return %>% 
  left_join(etfprice_corr) %>% 
  ungroup() %>% 
  filter(return <= -0.10 | return >= 0.05, 
         return * correlation >= 0)

# 8. Plots and tables.
(p2 <- ggplot(combined  %>% filter(correlation <= -0.30), aes(x = reorder(ticker, correlation), y = correlation)) + 
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "ETFs That Did Poorly Last Time Global Yields Surged", 
       subtitle = "Long-term bonds (soverign, inflation-linked, corporate, and municipal), U.S. real estate, precious metals, \nand emerging market equities all did poorly.",  
       y = "Correlation", 
       x = "ETF Ticker") + 
  theme_alphaplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)))
ggsave(file = "./Plots/1.009 ETFs That Did Poorly Last Time Global Yields Surged.png", 
       plot = p2, dpi = 300, width = 8, height = 5)
print(xtable(combined %>% 
               arrange(correlation) %>% 
               filter(correlation <= -0.30) %>% 
               select(ticker, fundBasics.segment, return, correlation)), 
      type = "html")

(p3 <- ggplot(combined  %>% filter(correlation >= 0.05 & correlation <= 0.25), aes(x = reorder(ticker, correlation), y = correlation)) + 
  geom_bar(stat = "identity", fill = "green") + 
  labs(title = "ETFs That Did Well Last Time Global Yields Surged", 
       subtitle = "Most ETFs have very little correlation to changes in the U.S. 10-year yield. The only exception is the U.S. financial sector.",  
       y = "Correlation", 
       x = "ETF Ticker") + 
  theme_alphaplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)))
ggsave(file = "./Plots/1.009 ETFs That Did Well Last Time Global Yields Surged.png", 
       plot = p3, dpi = 300, width = 8, height = 5)
print(xtable(combined %>% 
               arrange(correlation) %>% 
               filter(correlation >= 0.05 & correlation <= 0.25) %>% 
               select(ticker, fundBasics.segment, return, correlation)), 
      type = "html")
