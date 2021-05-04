set_config_table <- function(fee_percent, bot_config, coin_digits = 1, deal_start_condition, price_coin1) {

  
  base_order = bot_config$BO
  safety_order = bot_config$SO
  so_volume_scale = bot_config$OS
  so_step_scale = bot_config$MSC
  margin_profit = bot_config$TP
  max_orders = bot_config$MaxSO
  price_dev = bot_config$SOS
  
  #build the table
order=c(0:max_orders)
deviation_percent=c(0,cumsum(price_dev*so_step_scale^c(0:(max_orders-1))))

#The actual calculations in 3commas vary for each coin according to price, I believe. I can?t reproduce it here.
order_volume_coin2=c(base_order,safety_order*(c(1,so_volume_scale^c(1:(max_orders-1)))))

price_coin1=price_coin1*(1-(deviation_percent/100))
#The actual calculations in 3commas vary for each coin according to price, I believe. I can?t reproduce it here.
order_size_coin1=round(((order_volume_coin2*(1-(fee_percent/100)))/price_coin1)+0.45/10^coin_digits, digits=coin_digits)
#recalculate the total paid based on the bought coins!!
order_volume_coin2=order_size_coin1*price_coin1

total_volume_coin2=cumsum(order_volume_coin2)

total_size_coin1=cumsum(order_size_coin1)

taking_profit = total_volume_coin2*(1+(margin_profit/100))*(1+(fee_percent/100))

req_price_coin1=taking_profit/total_size_coin1

req_change=(req_price_coin1/price_coin1)

average_price=total_volume_coin2/total_size_coin1

  (price_coin1[1:max_orders]*order_size_coin1[1:max_orders]) + (price_coin1[c(1,1:(max_orders-1))]*order_size_coin1[1:max_orders])

config_table <- data.frame(order
                           ,deviation_percent
                           ,order_size_coin1
                           ,order_volume_coin2
                           ,price_coin1
                           ,average_price
                           ,req_price_coin1
                           ,req_change
                           ,total_size_coin1
                           ,total_volume_coin2
                           ,deal_start_condition
                           ) %>% 
  filter(deviation_percent < 95)



return(config_table)

}

check_deal_status <- function(deals_data) {
  
  if (is.null(deals_data)) {
    
    open_deals <- F

  } else {
    
     open_deals <-ifelse(dim(deals_data[is.na(deals_data$close_date),])[1]!=0, T, F) 
    
  }
  
 return(open_deals)
   
}

check_deal_start_condition <- function(testedperiod, deal_start_condition) {
  
  if (deal_start_condition == "ASAP") {
    
    command <- "buy"

  } else {
    
    command <- "hold"
    
  }
  
}

buy_base_order <- function(period, bot_config, deals_data) {

  price_coin1 <- period$close
  # cat("coin bought at", period$close)
  config_table <<- set_config_table(fee_percent,
                                    bot_config,
                                    coin_digits,
                                    deal_start_condition,
                                    price_coin1)
  
  plot_deals_new <- tibble(date=period$date , action = "Buy Base Order", value = config_table$price_coin1[1])
  plot_deals <<- rbind(plot_deals, plot_deals_new)
  
  deals_data_new <- data.frame(deal_id=ifelse(is.null(deals_data), 1, max(deals_data$deal_id)+1)
                           ,start_condition=config_table$deal_start_condition[1]
                           ,pair=period$symbol
                           ,price_unit_paid=price_coin1
                           ,avg_price_bought=config_table$price_coin1[1]
                           ,price_to_sell=config_table$req_price_coin1[1]
                           ,deal_start_dt=period$date
                           ,deal_close_dt=period$date
                           ,profit_percent=NA
                           ,coins_bought=config_table$order_size_coin1[1]
                           ,value_paid=config_table$order_volume_coin2[1]
                           ,value_received=NA
                           ,final_profit=NA
                           ,time_spent_m=NA
                           ,so_used=0
                           ,close_date=as.Date(NA, format = c("%d/%m/%Y")))
    
}


deal_starting <- function(period, deal_start_condition, bot_config, deals_data) {
  
  dealcheck <- check_deal_start_condition(period, deal_start_condition)
  
  if (dealcheck == "buy") {
    
    # Buy the base order
    new_deal <- buy_base_order(period, bot_config, deals_data)
    deals_data <- rbind(deals_data, new_deal)
    # print("1 deal created")
    
    return(deals_data)
    #If NO DEALS IS OPEN and DEAL_CHECK is HOLD, go to the next period
    
  } else { 
    
    # print("next goin!")
    return(deals_data)
    next
    
  } 
  
}

sell_coins <- function(deals_data, period, open_deal) {
  # print("Sell coins")
  
  plot_deals_new <- tibble(date=period$date , action = "Sell", value = deals_data$price_to_sell[open_deal])
  plot_deals <<- rbind(plot_deals, plot_deals_new)
  
  deals_data$deal_close_dt[open_deal] <- as.POSIXct(period$date, tz="EST")
  deals_data$value_received[open_deal] <- deals_data$price_to_sell[open_deal] * deals_data$coins_bought[open_deal]
  deals_data$final_profit[open_deal] <- deals_data$value_received[open_deal] - deals_data$value_paid[open_deal]
  deals_data$profit_percent[open_deal] <- (deals_data$final_profit[open_deal] / deals_data$value_paid[open_deal]) * 100
  deals_data$time_spent_m[open_deal] <- difftime(deals_data$deal_close_dt[open_deal], deals_data$deal_start_dt[open_deal], units = "mins")
  deals_data$close_date[open_deal] <- as.Date(deals_data$deal_close_dt[open_deal], format = c("%d/%m/%Y"))
  
  return(deals_data)
  
}

check_coin_price <- function(period, config_table, deals_data) {
  
  open_deal <- deals_data$deal_id[which(is.na(deals_data$close_date))]
  check_so <- max(which(period$low <= config_table$price_coin1))
  check_so<- ifelse(check_so == -Inf, deals_data$so_used[open_deal]+1, check_so)


  
  #Check if the higher price is bigger then sell condition
  if (period$high >= deals_data$price_to_sell[open_deal]) {
        #YES, sell,  and close the deal
    deals_data <- sell_coins(deals_data, period, open_deal)
        #Start another deal if the condition applies
    deals_data <- deal_starting(period, deal_start_condition, bot_config, deals_data)
    
    # print(deals_data)
    
    return(deals_data)
    
    # NO, check if the lower price is smaller than any buy price in the config_table
  } else if (deals_data$so_used[open_deal] < config_table$order[check_so]) {

    plot_deals_new <- tibble(date=period$date , action = "Buy Safe Order", value = config_table$price_coin1[check_so])
    plot_deals <<- rbind(plot_deals, plot_deals_new)
    
      deals_data$avg_price_bought[open_deal] <- config_table$average_price[check_so]
      deals_data$price_to_sell[open_deal] <- config_table$req_price_coin1[check_so]

      deals_data$coins_bought[open_deal] <- config_table$total_size_coin1[check_so]
      deals_data$value_paid[open_deal] <- config_table$total_volume_coin2[check_so]
      deals_data$so_used[open_deal] <- config_table$order[check_so]
      
      return(deals_data)
      #if it?s not the case, nothing happens
    } else {
     # print("nothing happens")
      return(deals_data) 
      
    }
    
    
  }
  
compare_bot <- function(bot_config, timeseries, coin_digits=2, fee_percent=0.075, deal_start_condition="ASAP") {
  
  #Filter period
  start_dt <- as.POSIXct(bot_config$start_date, tz= "EST")
  end_dt <- as.POSIXct(bot_config$end_date, tz="EST")
  
  testedperiod <<- timeseries %>% 
    filter(date >= start_dt, date <= end_dt) %>% 
    arrange(date)
  
  #Create the table that will keep all deals
  deals_data <- NULL
  
  #create the table that will allow ploting the deals
  plot_deals <<- NULL
  
  #Check each period
  for (n in 1:length(testedperiod$date)) {
    
    #filter the period
    period <- testedperiod[n,]
    
    #check if you HAVE open deals - the answer is always TRUE or FALSE
    open_deals <- check_deal_status(deals_data)
    
    # Check deal status 
    #NO OPEN DEALS:
    if(open_deals == FALSE) {
      
      # print(paste("No open deals on ", testedperiod$date[n]))
      
      #If you have no open deals, it should check and open one
      deals_data <- deal_starting(period, deal_start_condition, bot_config, deals_data)
      
      #IF You have open deals
      
    } else {
      
      # print(paste("Open deals on ",  testedperiod$date[n]))
      
      deals_data <- check_coin_price(period, config_table, deals_data)
      
    }
    
  }
  
  deals_data$bot_id = bot_config$bot_id[[1]]
  
  deals_data_result <- filter(deals_data, !is.na(close_date))
  
  backtest_result <- select(deals_data_result, deal_id
                            ,deal_start_dt
                            , deal_close_dt
                            , bot_id
                            , pair
                            , duration = time_spent_m
                            , used_safety_orders = so_used
                            , final_profit
                            , close_date
  ) %>% 
    group_by(bot_id
             , pair) %>%
    summarise(start_date = min(deal_start_dt)
              ,end_date = max(deal_close_dt)
              ,tot_deals = n()
              ,avg_duration_m = mean(duration, na.rm = T)
              ,max_duration_m = max(duration, na.rm = T)
              ,tot_days_deals = n_distinct(close_date, na.rm = T)
              ,max_safe_orders = max(used_safety_orders)
              ,final_profit = sum(final_profit, na.rm = T)
    ) %>% 
    arrange(start_date, desc(tot_deals))
  
  real_result_data <- select(bot_config, bot_id, pair, start_date, end_date, tot_deals, avg_duration_m, max_duration_m, tot_days_deals, max_safe_orders, final_profit)
  validation <- rbind(real_result_data, backtest_result)
  
  return(list(deals_data=deals_data, backtest_result=backtest_result, validation=validation, testedperiod=testedperiod))
  
}
  






