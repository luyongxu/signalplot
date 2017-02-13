source("./Code/1.001 Initial Functions and Libraries.R")

income_raw <- Quandl("RAYMOND/CMG_REVENUE_Q") 
income <- income_raw %>% 
  arrange(Date) %>% 
  mutate(quarter = (Value / lag(Value, 1))^4 - 1, 
         year = Value / lag(Value, 4) - 1)
ggplot(income, aes(x = Date, y = Value)) + geom_line()

ggplot(income, aes(x = Date, y = quarter)) + geom_line() + geom_point()

ggplot(income, aes(x = Date)) + 
  geom_line(aes(y = quarter), colour = "blue") + geom_point(aes(y = quarter), colour = "blue") + 
  geom_line(aes(y = year), colour = "red") + geom_point(aes(y = year), colour = "red")


temp <- ts(income$Value, frequency = 4, start = c(2010, 2))
temp2 <- seas(temp)


income <- income %>% 
  mutate(Value_sadj = final(temp2)) %>% 
  mutate(quarter_sadj = (Value / lag(Value_sadj, 1))^4 - 1, 
         year_sadj = Value / lag(Value_sadj, 4) - 1)
  
ggplot(income, aes(x = Date)) + 
  geom_line(aes(y = quarter_sadj), colour = "blue") + geom_point(aes(y = quarter_sadj), colour = "blue") + 
  geom_line(aes(y = year_sadj), colour = "red") + geom_point(aes(y = year_sadj), colour = "red")
