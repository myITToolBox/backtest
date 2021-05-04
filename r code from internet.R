library(tidyverse)
library(crypto)


back_testing<-function(symbol="BTC", consecutive=7, SL=0.1, TP=0.1, start_date = "20180101") {
  
  
  
  df<-crypto_history(coin = symbol, start_date = start_date)
  
  
  
  df<-df%>%mutate(Sign = ifelse(close>lag(close),"up", "down"))%>%
    mutate(Streak=sequence(rle(Sign)$lengths))
  
  
  df<-df%>%select(symbol, date, open, high, low, close, Sign, Streak)%>%na.omit()%>%
    mutate(Signal = case_when(lag(Sign)=="up" & lag(Streak)%%consecutive==0~'short',
                              lag(Sign)=="down" & lag(Streak)%%consecutive==0~'long',
                              TRUE~""), Dummy=TRUE
    )
  
  
  Trades<-df%>%filter(Signal!="")%>%select(Open_Position_Date=date, Open_Position_Price=open, Dummy, Signal)
  Trades
  
  
  Portfolios<-Trades%>%inner_join(df%>%select(-Signal), by="Dummy")%>%filter(date>Open_Position_Date)%>%select(-Dummy)%>%mutate(Pct_Change=open/Open_Position_Price-1)%>%
    mutate(Alert = case_when(Signal=='long'& Pct_Change>TP~'TP',
                             Signal=='long'& Pct_Change< -SL~'SL',
                             Signal=='short'& Pct_Change>TP~'SL',
                             Signal=='short'& Pct_Change< -SL~'TP'
    )
    )%>%group_by(Open_Position_Date)%>%mutate(Status=ifelse(sum(!is.na(Alert))>0, 'Closed', 'Active'))
  
  
  Active<-Portfolios%>%filter(Status=='Active')%>%group_by(Open_Position_Date)%>%arrange(date)%>%slice(n())%>%
    mutate(Profit=case_when(Signal=='short'~Open_Position_Price-open, 
                            Signal=='long'~open-Open_Position_Price))%>%
    select(symbol, Status, Signal, date, Open_Position_Date, Open_Position_Price, open, Profit)
  
  Closed<-Portfolios%>%filter(Status=='Closed')%>%na.omit()%>%group_by(Open_Position_Date)%>%arrange(date)%>%slice(1)%>%
    mutate(Profit=case_when(Signal=='short'~Open_Position_Price-open, 
                            Signal=='long'~open-Open_Position_Price))%>%
    select(symbol, Status, Signal, date, Open_Position_Date, Open_Position_Price, open, Profit)
  
  final<-bind_rows(Closed,Active)%>%ungroup()%>%arrange(date)%>%mutate(ROI=Profit/Open_Position_Price, Running_ROI=cumsum(Profit)/cumsum(Open_Position_Price))
  
  
  return(final)
  
  
}