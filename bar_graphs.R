library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(prophet)
library(extrafont)

poop <- read_csv('https://docs.google.com/spreadsheets/d/1Ow1OTP3QfyQyi1K2jeyqjv5eg_AzqF36Tn3sn5NSQQw/pub?gid=444517334&single=true&output=csv') 

poop$time <- format(as.POSIXct(strptime(poop$Timestamp,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
poop$time <- as.POSIXct(poop$time, "%m/%d/%Y %H:%M:%S",tz="")

poop$day  <- format(as.POSIXct(strptime(poop$Timestamp,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")

freqdata <- poop %>% group_by(day) %>% summarise(y = n())

byday <- ggplot(plotdata, aes(x=day, y=y)) + geom_bar(aes(fill = ..y..), stat = "identity") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Date") +ylab("Number of Movements") +
  ggtitle("Daily Counts") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_gradient(low = "burlywood", high = "burlywood4") + 
  theme(legend.position="none") + geom_vline(xintercept = 40.5,  linetype = "longdash")

ggplotly(byday)


qualitydata <- poop %>% group_by(day) %>% summarise(mean = mean(`How Bad Is It?`))
qualitydata$mean <- 9 -qualitydata$mean

quality <- ggplot(qualitydata, aes(x=day, y=mean)) + geom_bar(aes(fill = ..y..), stat = "identity") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Date") +ylab("Higher Values = Worse") +
  ggtitle("How Bad Is It?") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_gradient(low = "gray87", high = "firebrick1") + 
  theme(legend.position="none") + geom_vline(xintercept = 40.5,  linetype = "longdash")

ggplotly(quality)

poop$t2 <- as.POSIXct(poop$Timestamp, "%m/%d/%Y %H:%M:%S",tz="")
poop$hour <- hour(poop$t2)

byhour <- poop %>% group_by(hour) %>% summarise(y =n()) %>% mutate(pct = y/sum(y))

hourplot <- ggplot(byhour, aes(x=hour, y=pct*100)) + geom_bar(aes(fill=pct), stat="identity") +  
  xlab("Time of Day") +ylab("Percent of Movements") +
  ggtitle("Which Time of the Day is Worst?") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_gradient(low = "deepskyblue4", high = "deepskyblue") + 
  theme(legend.position="none")  +
  scale_x_continuous(limits = c(.5,5.5), breaks = c(1,2,3,4,5), labels = c("10pm-6am", "7am-10am", "11am-2pm", "3pm-7pm", "8p-10pm"))

ggplotly(hourplot)

poop$daysweek <- strftime(poop$ds,'%A')

daysweek <- poop %>% group_by(daysweek) %>% summarise(y =n()) %>% mutate(pct = y/sum(y))

daysweek$daysweek <- factor(daysweek$daysweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


dayweekplot <- ggplot(daysweek, aes(x=daysweek, y=pct*100)) + geom_bar(aes(fill=pct), stat="identity") +  
  xlab("Time of Day") +ylab("Percent of Movements") +
  ggtitle("Which Day of the Week is Worst?") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_gradient(low = "darkorchid4", high = "darkorchid1") + 
  theme(legend.position="none")  

ggplotly(dayweekplot)

poop$wkcount <- (poop$weeks - min(poop$weeks))/dweeks(1)
poop$wkcount <- round(poop$wkcount, 0)

weekly <- poop %>% group_by(wkcount) %>% summarise(total.count =n()) %>% mutate(pct = total.count/sum(total.count))


weeklyplot <- ggplot(weekly, aes(x=wkcount, y=total.count)) + geom_bar(aes(fill=total.count), stat="identity") +  
  xlab("Grouped by Week") +ylab("Movements that Week") +
  ggtitle("Is the Remicade Wearing off?") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_gradient(low = "darkorange4", high = "darkorange1") + 
  theme(legend.position="none")  

ggplotly(weeklyplot)

