source("./Code/1.001 Initial Functions and Libraries.R")

# 1. Query data. 
GET("http://www.econ.yale.edu/~shiller/data/ie_data.xls", 
    write_disk("./Raw Data/Shiller/ie_data.xls"), 
    overwrite = TRUE)
shiller_raw <- read_excel("./Raw Data/Shiller/ie_data.xls", sheet = "Data")

# 2. Clean data. 
col_names <- c("date", "price", "dividend", "earnings", "cpi", "date_fraction", "long_rate", "real_price", "real_dividend", "real_earnings", "cape")
colnames(shiller_raw) <- col_names
shiller <- shiller_raw %>% 
  filter(row_number() >= 8) %>% 
  filter(!is.na(date)) %>% 
  mutate_each_(funs(as.numeric), col_names) %>% 
  mutate(year = str_split_fixed(date, "\\.", n = 2)[, 1], 
         month = str_split_fixed(date, "\\.", n = 2)[, 2], 
         month = ifelse(month == "1", "10", month), 
         date = ymd(paste0(year, month, "01")) + months(1) - days(1))

# 3. Bubble peaks. 
shiller <- shiller %>% 
  mutate(bubble = ifelse(date == "2007-10-31", price, NA), 
         bubble = ifelse(date == "2000-08-31", price, bubble), 
         bubble = ifelse(date == "1990-06-30", price, bubble), 
         bubble = ifelse(date == "1987-08-31", price, bubble), 
         bubble = ifelse(date == "1973-01-31", price, bubble))


# 3. Plot data. 
ggplot(shiller %>% filter(date >= "1950-01-01"), aes(x = date, y = price)) + 
  geom_line(colour = "blue") + 
  geom_point(aes(y = bubble), colour = "red") + 
  scale_y_log10()



