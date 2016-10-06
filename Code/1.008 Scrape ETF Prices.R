source("./Posts/1.001 Initial Functions and Libraries.R")

# 1. www.etf.com API is in JSON format. The 0 refers to begin at the 0th element. 
# The 2000 represents number of elements to retrieve. Not sure what the 1 means. 
url <- GET("http://www.etf.com/etf-finder-funds-api//-aum/0/2000/1")
etf <- fromJSON(content(url, as = "text"), flatten = TRUE) %>% 
  mutate(fundBasics.issuer = str_replace_all(fundBasics.issuer, "(\\<.*\\>)(.*)(\\<.*\\>)", "\\2"), 
         fundBasics.segment = str_replace_all(fundBasics.segment, "\\s+", " "), 
         fundBasics.expenseRatio.value = fundBasics.expenseRatio.value / 10000)

#2. Download data. Some ETFs with limited historical data produce an error. 
ticker_list <- etf$ticker
df <- data.frame()
n <- 1
for (ticker in ticker_list) {
  print(paste0("ETF number ", n, ": ", ticker))
  df_temp <- tryCatch(getSymbolsYahoo(ticker), 
                      error = function(e) {
                        print(paste0("Could not download data for ", ticker))
                        return(data.frame())
                      })
  df <- bind_rows(df, df_temp)
  n <- n + 1
  Sys.sleep(runif(1, 1, 2))
}

#3. Save data. 
write.csv(etf, "./Output/ETFs/ETF Metadata.csv", row.names = FALSE)
write.csv(df, "./Output/ETFs/ETF Prices.csv", row.names = FALSE)

#4. Load data. 
etf <- read_csv("./Output/ETFs/ETF Metadata.csv")
df <- read_csv("./Output/ETFs/ETF Prices.csv")
temp <- df %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  filter(!(date >= "2000-01-01" & n < 5))
(p1 <- ggplot(temp, aes(x = date, y = n)) + 
  geom_line(colour = "blue") + 
  labs(title = "Number of ETFs Over Time", 
       subtitle = "Most ETFs are pretty new.", 
       y = "Number of ETFs", x = "Date") + 
  theme_alphaplot())
ggsave(file = "./Plots/1.008 Number of ETFs.png", plot = p1, dpi = 300, width = 8, height = 5)

