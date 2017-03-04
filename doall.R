data <- read.csv("activity.csv")
daysteps <- tapply(data$steps, data$date, "sum", na.rm=TRUE)
hist( daysteps, main="Steps per Day", xlab="" )
mean(daysteps)
median(daysteps)
intervalsteps <- tapply(data$steps, data$interval, "mean", na.rm=TRUE)
plot( as.numeric(names(intervalsteps)), intervalsteps, xlab="Interval", ylab="Steps",
      main="Average Steps per Time-of-Day Interval", type="l")
names(which.max(intervalsteps))
sum(is.na(data$steps))
newdata <- data
s = intervalsteps[as.factor(data$interval)]
nas = is.na(data$steps)
for (i in 1:length(nas)) { if (nas[[i]]) { newdata$steps[i] <- s[i] } }
newdaysteps <- tapply(newdata$steps, newdata$date, "sum", na.rm=TRUE)
hist( newdaysteps, main="Steps per Day", xlab="" )
mean(newdaysteps)
median(newdaysteps)
days <- weekdays(as.POSIXct(data$date))
f = factor( days %in% c("Saturday","Sunday"), labels=c("weekday","weekend") )
library(lattice)

wddata = newdata[f=="weekday",]
wedata = newdata[f=="weekend",]
wdsteps <- tapply(wddata$steps, wddata$interval, "mean")
westeps <- tapply(wedata$steps, wedata$interval, "mean")
allsteps = rbind( cbind( wdsteps, "weekday", names(intervalsteps)), 
                  cbind( westeps, "weekend", names(intervalsteps)) )
xyplot(as.numeric(allsteps[,1]) ~ as.numeric(allsteps[,3]) | allsteps[,2], 
                  layout = c(1, 2), type='l', xlab='Interval', ylab="Steps",
                  main="Average Steps per Time-of-Day Interval")

