source("./Posts/1.001 Initial Functions and Libraries.R")

# 1. Load data. 
etf <- read_csv("./Output/ETFs/ETF Metadata.csv")

# 2. Calculate scores.
etf <- etf %>% 
  mutate(score_passive = round(0.7 * analysis.efficiencyScore + 0.2 * analysis.tradabilityScore + 0.1 * analysis.fitScore, 2), 
         score_active = round(0.2 * analysis.efficiencyScore + 0.7 * analysis.tradabilityScore + 0.1 * analysis.fitScore, 2))
etf_passive <- etf %>% 
  group_by(fundBasics.segment) %>% 
  arrange(desc(score_passive)) %>% 
  filter(row_number() == 1) %>% 
  select(fundBasics.segment, ticker_passive = ticker, score_passive)
etf_active <- etf %>% 
  group_by(fundBasics.segment) %>% 
  arrange(desc(score_active)) %>% 
  filter(row_number() == 1) %>% 
  select(fundBasics.segment, ticker_active = ticker, score_active)
aum <- etf %>% 
  group_by(fundBasics.segment) %>% 
  summarise(aum_total = sum(fundBasics.aum.value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(aum_total = round(aum_total / 1000000000, 2)) %>%  
  arrange(desc(aum_total)) %>% 
  left_join(etf_passive) %>% 
  left_join(etf_active) %>% 
  filter(!is.na(score_passive), 
         !(is.na(score_active)))
aum_table <- xtable(aum)
align(aum_table) <- "l|l|l|r|r|r|r|"
print(aum_table, type = "html")


# 3. Plots
(p1 <- ggplot(etf, aes(x = analysis.efficiencyScore, y = analysis.tradabilityScore)) + 
  geom_point(alpha = 1/2, aes(colour = classification.assetClass)) + 
  labs(title = "ETF Efficiency Versus Tradability By Asset Class", 
       subtitle = "Equity funds generally have the best scores, followed by fixed income, commodity, currency, and alternatives.", 
       y = "Tradability Score", 
       x = "Efficiency Score", 
       colour = "Asset Class") + 
  xlim(0, 100) + 
  theme_alphaplot())
ggsave(file = "./Plots/1.010 Optimal ETFs Scatter.png", plot = p1, dpi = 300, width = 8, height = 5)
 
df <- data.frame()
ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) + theme_alphaplot()
