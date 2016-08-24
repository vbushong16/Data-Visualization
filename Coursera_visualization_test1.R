
library(Hmisc)
library(dplyr)
library(tidyr)

### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\Data-Visualization"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))


data = read.csv("RAW_TABLE.csv",stringsAsFactors = TRUE)
# head(data)

colnames(data)
data[data$years_until_expiration<=0,23]


data[data$unique_id==1906,c(6,33)]



data[data$unique_id==1906,c(33)]-data[data$unique_id==1906,c(6)]

mean(data[data$exercise_year >=2011,"option_exercised"])
summary(data[data$exercise_year >=2011,"option_exercised"])


# #testing stats by exercise date
# output = by(data[,"option_exercised"],data[,"exercise_date"],sum)
# class(output)
# plot(output,type="h")
# as.list(output)
# test = data.frame(do.call("rbind", as.list(output)))
# test = cbind(rownames(test),test)
# colnames(test) <- c("exercise_date","quantity")
# test[,"exercise_date"] <- as.Date(test[,"exercise_date"],"%d-%B-%y")
# test = test[order(test[,"exercise_date"]),]
# plot(test,type="h")
# summary(test)

#testing stats by exercise quarter
#output = by(data[,"option_exercised"],data[,c("exercise_year","exercise_quarter")],sum)
out = data %>%
  group_by(exercise_year,exercise_quarter) %>%
  summarize(option_exercised=sum(option_exercised)) 
out

out[[3]]

#testing stats by exercise price by quarter
options(dplyr.width = Inf)
out1 = data %>%
  group_by(exercise_year,exercise_quarter) %>%
  summarize(market_price_at_exercise=mean(market_price_at_exercise))
out1

y2 <- out[[3]]/1000000
y2 <- out[[3]]/sum(out[[3]])*100

# y2 = out[[3]][2:5]/sum(out[[3]][2:5])

library(plotrix)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(out1[[3]],type="l",xaxt='n',lwd = 2,xlab ="Quarters",col = "Blue",ylim = c(0,max(out1[[3]])+10),xlim = c(1,26),ylab ="Stock value",main = "Investor Confidence")
axis(1, at= 1:26, labels= out[[2]])
axis(4, at= seq(0,100,3.85), labels= round(y2[order(round(y2,2))],2),cex = .25)
mtext("Option Exercised", side=4, line=-1.5)
text(x=seq(3,26,4), y = rep(100,7), labels= seq(2009,2014))
ablineclip(h = NULL, v = seq(2,26,4), col = "red",lty = 3,lwd=2,y1 =0 ,y2 = 100)
lines(1:26,7*y2,type="h",col="black", lwd = 3)
legend(27,120,c("Stock price","Option exercised (in Million)"),cex = .5,lty=c(1,1), lwd=c(2,2),col=c("blue","black"))

samp <- rnorm(50)
d1 <- density(samp)
d2 <- density(samp, adj = 0.5)
d3 <- density(samp, adj = 2.0)
plot( c(d1$x, d2$x, d3$x), c(d1$y, d2$y, d3$y), type = "n")
lines( d1$x, d1$y, lty = 2 )  # maybe lines(d1, lty = 2) would work
lines( d2$x, d2$y, lty = 3 )
lines( d3$x, d3$y, lty = 4 )

#testing counts
# out = count(data,vars = "exercise_date") 
# colnames(out)

y1 <- out1[[3]]
y2 <- 100*(out[[3]]/sum(out[[3]]))
#y2 <- out[[3]]

plot(1:26,y1,type="l",col="red")
lines(1:26,y2,type="l",col="green")

df <- data.frame(1:26,y1,y2)

require(ggplot2)

ggplot(df, aes(x = 1:26, y = y2)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_bar(stat="identity", colour="green")


# out[,"exercise_date"] <- as.Date(out[,"exercise_date"],"%d-%B-%y")
# out = out[order(out[,"exercise_date"]),]
# 
# plot(out[,"freq"], type = "h" )


