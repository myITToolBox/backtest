# devtools::install_github("jessevent/crypto")
library(crypto)
library(tidyverse)

fee_buy_percent <- 0.1
fee_sell_percent <- 0.1
interest_percent <- 1

investment <- 20

# return <- investment*(1-fee_buy_percent/100) * (1+interest_percent/100) * (1-fee_sell_percent/100)
# 
# profit_percent <- ((return-investment)/investment)*100

return <- investment

for (i in 1:200) {
  
  return <- return*(1-fee_buy_percent/100) * (1+interest_percent/100) * (1-fee_sell_percent/100)
  
  profit_percent <- ((return-investment)/investment)*100
  print(profit_percent)
  
}

print(return)

print(profit_percent)

total_secured_per_bot_usd <- 900
bots_qty <- 21
usd_for_safe <- total_secured_per_bot*bots_qty
daily_return_usd <- 40
(daily_return_percent <- daily_return_usd/usd_for_safe)

return_percent <- 0
tot_return_usd <- 0

for (i in 1:365) {
  
  (tot_return_usd <- tot_return_usd + daily_return_usd)
  
}

