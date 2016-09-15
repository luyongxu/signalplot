library(jsonlite)
library(httr)
library(ggplot2)

# 1. www.etf.com API is in JSON format. The 0 refers to begin at the 0th element. 
# The 2000 represents number of elements to retrieve. Not sure what the 1 means. 
url <- GET("http://www.etf.com/etf-finder-funds-api//-aum/0/2000/1")
etf <- fromJSON(content(url, as = "text"))

# 2. Visualizations. 
ggplot(etf, aes(x = fundBasics$expenseRatio$value)) + geom_histogram() + xlim(0, 300)
ggplot(etf, aes(x = fundBasics$aum$value)) + geom_histogram()
ggplot(etf, aes(x = fundBasics$spreadPct$value)) + geom_histogram() + xlim(0, 0.02)

