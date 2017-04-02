library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(prophet)
library(extrafont)

poop <- read_csv('https://docs.google.com/spreadsheets/d/1Ow1OTP3QfyQyi1K2jeyqjv5eg_AzqF36Tn3sn5NSQQw/pub?gid=444517334&single=true&output=csv')

poop$time <- format(as.POSIXct(strptime(poop$Timestamp,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
poop$ds  <- format(as.POSIXct(strptime(poop$Timestamp,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")

day <- poop %>% group_by(ds) %>% summarise(y = n())
is.na(day)

m <- prophet(day)


future <- make_future_dataframe(m, periods = 15)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)


fb <-ggplot(forecast, aes(ds)) + geom_ribbon(aes(ymin = forecast$yhat_lower, ymax = forecast$yhat_upper), fill = "#3b5998") +
  geom_line(aes(y = yhat), size =1, color = "#dfe3ee") +
  geom_point(aes(y =yhat), color = "#dfe3ee") +  
  xlab("Date") + ylab("Projected Number") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) +
  theme(panel.background = element_rect(fill = "gray77")) +
  ggtitle("Using Facebook's Prophet to make Predictions")

ggplotly(fb)