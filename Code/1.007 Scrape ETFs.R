source("./Code/1.001 Initial Functions and Libraries.R")

# 1. www.etf.com API is in JSON format. The 0 refers to begin at the 0th element. 
# The 2000 represents number of elements to retrieve. Not sure what the 1 means. 
url <- GET("http://www.etf.com/etf-finder-funds-api//-aum/0/2000/1")
etf <- fromJSON(content(url, as = "text"), flatten = TRUE) %>% 
  mutate(score = analysis.efficiencyScore * 0.5 + analysis.tradabilityScore * 0.3 + analysis.fitScore * 0.2, 
         fundBasics.issuer = str_replace_all(fundBasics.issuer, "(\\<.*\\>)(.*)(\\<.*\\>)", "\\2"), 
         fundBasics.segment = str_replace_all(fundBasics.segment, "\\s+", " "), 
         fundBasics.expenseRatio.value = fundBasics.expenseRatio.value / 10000) %>% 
  arrange(desc(score))

# 2. Visualizations. 
ggplot(etf, aes(x = fundBasics.expenseRatio.value)) + geom_histogram(binwidth = 0.05) + xlim(0, 2.0)
ggplot(etf, aes(x = fundBasics.aum.value)) + geom_histogram() + scale_x_log10()
ggplot(etf, aes(x = fundBasics.spreadPct.value)) + geom_histogram() + xlim(0, 0.02)
ggplot(etf, aes(x = analysis.efficiencyScore)) + geom_histogram()
ggplot(etf, aes(x = analysis.tradabilityScore)) + geom_histogram()
ggplot(etf, aes(x = analysis.fitScore)) + geom_histogram()
ggplot(etf, aes(x = analysis.avgDailyDollarVolume)) + geom_histogram() + scale_x_log10()
ggplot(etf, aes(x = analysis.avgDailyShareVolume)) + geom_histogram() + scale_x_log10()
ggplot(etf, aes(x = analysis.spread.value)) + geom_histogram() + xlim(0, 0.5)
ggplot(etf, aes(x = fundamentals.dividendYield)) + geom_histogram() + xlim(0, 0.1)
ggplot(etf, aes(x = fundamentals.fixedIncome.duration)) + geom_histogram()

# 3. Biggest segments by AUM. 
segment <- etf %>% 
  group_by(fundBasics.segment) %>% 
  summarise(aum = sum(fundBasics.aum.value, na.rm = TRUE) / 1000000000) %>% 
  arrange(desc(aum)) %>% 
  filter(aum >= 10)
ggplot(segment, aes(x = reorder(fundBasics.segment, -aum), y = aum)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 4. Top 5 per segment. 
clean <- etf %>% 
  group_by(fundBasics.segment) %>% 
  top_n(n = 1, wt = score)
ggplot(clean, aes(x = ticker, y = score)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  facet_wrap(~ fundBasics.segment) + coord_cartesian(ylim = c(75, 100))

# 5. Plots for publication. 
(p1 <- ggplot(etf, aes(x = fundBasics.expenseRatio.value)) + 
  geom_histogram(binwidth = 0.0005, fill = "blue") + 
  scale_x_continuous(limits = c(0, 0.02), labels = percent) + 
  labs(title = "ETF Expense Ratio", 
       subtitle = "Most ETFs have an expense ratio between 0% and 1%.", 
       y = "Number of ETFs", x = "Expense Ratio") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.007 ETF Expense Ratio Histogram.png", plot = p1, dpi = 300, width = 8, height = 5)

(p2 <- ggplot(etf, aes(x = fundBasics.spreadPct.value)) + 
  geom_histogram(binwidth = 0.0001, fill = "blue") + 
  scale_x_continuous(limits = c(0, 0.02), labels = percent) + 
  labs(title = "ETF Bid-Ask Spread", 
       subtitle = "Most ETFs are easily traded with a bid-ask spread between 0% and 0.25%.", 
       y = "Number of ETFs", x = "Bid-Ask Spread") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.007 ETF Bid-Ask Spread.png", plot = p2, dpi = 300, width = 8, height = 5)

