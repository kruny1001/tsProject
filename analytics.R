## https://www.datacamp.com/community/blog/r-xts-cheat-sheet
## https://www.r-bloggers.com/plot-xts-is-wonderful/

## Extracting 
library(dplyr)
library(lubridate)

Order <- setRefClass("Order",
  fields = list(orgData = "ANY"),
  methods = list(
    readCSV = function(filePath){
      orgData <<- read.csv(filePath, header=T, sep=",")
      dates <- orgData %>%  select("Order.Date")
      dates = na.omit(dates)
      target = as.character(dates$Order.Date)
      targetNew <- as.Date(target, format = "%m-%d-%Y")
      
      orgData$Order.Month <<- month(targetNew)
      orgData$Order.Year <<- year(targetNew)
      orgData$Order.Date <<- targetNew
    },
    extractMonth = function(month, year){
      target = orgData %>% 
        filter(Order.Month == month & Order.Year ==year) %>%
        filter(Order.Status != "Refunded" & Order.Status != "Declined" & Order.Status!="Disputed" ) %>%
        select(Order.Date, Order.Status, Product.Details, OrderTotal) 
      return (target)
    },
    extractYear = function(year){
      target = orgData %>% 
        filter(Order.Year ==year) %>%
        filter(Order.Status != "Refunded" & Order.Status != "Declined" & Order.Status!="Disputed" ) %>%
        select(Order.Date, Order.Status, Product.Details, OrderTotal) 
      return (target)
    },
    drawPlot = function(){
      
    }
  )  
)

library(padr)
orderData <- read.csv("./orders-2017-11-19-2.csv")
nrow(orderData)
length(colnames(orderData))
colnames(orderData) <- c("orderId", "status", "date", "time", "total", 
  "custID", "prodDetail", "tranID")
orderData$date = gsub("/","-", orderData$date)
orderData$date = as.Date(orderData$date, "%m-%d-%Y")
normOrder <- orderData %>% mutate(dateAll = date)
normOrder$dateAll = as.POSIXlt(normOrder$dateAll)

library(xts)
library(lubridate)


newNorm <- normOrder %>% select(total, dateAll)
newNorm$total <- as.numeric(newNorm$total)
ts <- xts(newNorm$total, order.by=newNorm$dateAll)
yearly <- to.yearly(ts)
daily <- to.daily(ts)
monthly <- to.monthly(ts)
quarterly <- to.quarterly(ts)
period <- to.period(ts,period="years")

dd = ts(quarterly[,1])
plot(daily[,2])
plot(monthly[,2])

plot(monthly, main="Time Series monthly")
plot(quarterly, main="Time Series quarterly")
plot(period, main="Time Series quarterly")

y2014 <- ts["2014/2014-12"]
y2015 <- ts["2015/2015-12"]
y2016 <- ts["2016-10"]
sum(y2016)

y2017.01 <- ts["2017-01"]
y2017.02 <- ts["2017-02"]
y2017.03 <- ts["2017-03"]
y2017.04 <- ts["2017-04"]
y2017.05 <- ts["2017"]

test <- to.daily(ts["2017"])
plot(daily[,4])
plot(y2017.01)
plot(y2017.02)
plot(y2017.03)
plot(y2017.05)

plot(ts["2017-04"], 
  type='l',
  xlim=as.POSIXct(c("2017-04-01 00:00:00","2017-04-30 00:00:00"))
)
plot(ts["2017-05"], 
  type='l',
  xlim=as.POSIXct(c("2017-05-01 00:00:00","2017-05-31 00:00:00"))
)
plot(ts["2017-06"], 
  type='l',
  xlim=as.POSIXct(c("2017-06-01 00:00:00","2017-06-30 00:00:00"))
)
plot(ts["2017-07"], 
  type='l',
  xlim=as.POSIXct(c("2017-06-01 00:00:00","2017-06-30 00:00:00"))
)
plot(ts["2017-08"], 
  type='l',
  xlim=as.POSIXct(c("2017-06-01 00:00:00","2017-06-30 00:00:00"))
)





# plot a subset of the data
plot(ts, subset="2014-04-01/2014-04-02")

# function to compute simple returns
simple.ret <- function(x, col.name){
  x[,col.name] / lag(x[,col.name]) - 1
}

https://www.datacamp.com/community/blog/r-xts-cheat-sheet

# plot a subset of the data
plot(ts, subset="2017-11-01")
plot(ts, subset="2017-11-02")
plot(ts, subset="2017-11-03")
plot(ts, subset="2017-11-04")
plot(ts, subset="2017-11-05")
plot(ts, subset="2017-11-01/2017-11-05")

require(PerformanceAnalytics)

y2016 <- ts["2016"]
cumYear <- do.call(rbind,  lapply(split(ts,"years"),  cumsum))


par(mfrow=c(3,1))
plot(cumYear, subset="2015", at="chic", plot.type="s", ylim=c(0, 250000))
lines(cumYear["2016"], subset="2016", at="chic", plot.type="s", ylim=c(0, 250000))
plot(cumYear, subset="2017", at="chic", plot.type="s", ylim=c(0, 250000))

par(mfrow=c(4,1))
plot(ts, subset="2017-07", at="chic", plot.type="s", cex.axis=1.00)
plot(ts, subset="2017-08", at="chic", plot.type="s", cex.axis=0.75)
plot(ts, subset="2017-09", at="chic", plot.type="s", cex.axis=1.00)
plot(ts, subset="2017-10", at="chic", plot.type="s", cex.axis=1.25)

# plot the close and add a panel with the simple returns
plot(sample.xts[,"Close"])
R <- simple.ret(sample.xts, "Close")
lines(R, type="h", on=NA)



y14<- OrderObj$extractYear(2014)
y15<- OrderObj$extractYear(2015)
y16<- OrderObj$extractYear(2016)
y17<- OrderObj$extractYear(2017)


nrow(y14)
nrow(y15)
nrow(y16)
nrow(y17)


D1 <- OrderObj$extractMonth(1, 2014)
D2 <- OrderObj$extractMonth(2, 2014)
D3 <- OrderObj$extractMonth(3, 2014)
D4 <- OrderObj$extractMonth(4, 2014)
D5 <- OrderObj$extractMonth(5, 2014)
D6 <- OrderObj$extractMonth(6, 2014)
D7 <- OrderObj$extractMonth(7, 2014)
D8 <- OrderObj$extractMonth(8, 2014)
D9 <- OrderObj$extractMonth(9, 2014)
D10 <- OrderObj$extractMonth(10, 2014)
D11 <- OrderObj$extractMonth(11, 2014)
D12 <- OrderObj$extractMonth(12, 2014)

MonthlyTotal2014 <- c(
  Jan=sum(D1$OrderTotal),
  Fab=sum(D2$OrderTotal),
  Mar=sum(D3$OrderTotal),
  Apr=sum(D4$OrderTotal),
  May=sum(D5$OrderTotal),
  Jun=sum(D6$OrderTotal),
  Jul=sum(D7$OrderTotal),
  Aug=sum(D8$OrderTotal),
  Sep=sum(D9$OrderTotal),
  Oct=sum(D10$OrderTotal),
  Nov=sum(D11$OrderTotal),
  Dec=sum(D12$OrderTotal)
)

D1 <- OrderObj$extractMonth(1, 2015)
D2 <- OrderObj$extractMonth(2, 2015)
D3 <- OrderObj$extractMonth(3, 2015)
D4 <- OrderObj$extractMonth(4, 2015)
D5 <- OrderObj$extractMonth(5, 2015)
D6 <- OrderObj$extractMonth(6, 2015)
D7 <- OrderObj$extractMonth(7, 2015)
D8 <- OrderObj$extractMonth(8, 2015)
D9 <- OrderObj$extractMonth(9, 2015)
D10 <- OrderObj$extractMonth(10, 2015)
D11 <- OrderObj$extractMonth(11, 2015)
D12 <- OrderObj$extractMonth(12, 2015)

MonthlyTotal2015 <- c(
  Jan=sum(D1$OrderTotal),
  Fab=sum(D2$OrderTotal),
  Mar=sum(D3$OrderTotal),
  Apr=sum(D4$OrderTotal),
  May=sum(D5$OrderTotal),
  Jun=sum(D6$OrderTotal),
  Jul=sum(D7$OrderTotal),
  Aug=sum(D8$OrderTotal),
  Sep=sum(D9$OrderTotal),
  Oct=sum(D10$OrderTotal),
  Nov=sum(D11$OrderTotal),
  Dec=sum(D12$OrderTotal)
)

D1 <- OrderObj$extractMonth(1, 2016)
D2 <- OrderObj$extractMonth(2, 2016)
D3 <- OrderObj$extractMonth(3, 2016)
D4 <- OrderObj$extractMonth(4, 2016)
D5 <- OrderObj$extractMonth(5, 2016)
D6 <- OrderObj$extractMonth(6, 2016)
D7 <- OrderObj$extractMonth(7, 2016)
D8 <- OrderObj$extractMonth(8, 2016)
D9 <- OrderObj$extractMonth(9, 2016)
D10 <- OrderObj$extractMonth(10, 2016)
D11 <- OrderObj$extractMonth(11, 2016)
D12 <- OrderObj$extractMonth(12, 2016)

MonthlyTotal2016 <- c(
  Jan=sum(D1$OrderTotal),
  Fab=sum(D2$OrderTotal),
  Mar=sum(D3$OrderTotal),
  Apr=sum(D4$OrderTotal),
  May=sum(D5$OrderTotal),
  Jun=sum(D6$OrderTotal),
  Jul=sum(D7$OrderTotal),
  Aug=sum(D8$OrderTotal),
  Sep=sum(D9$OrderTotal),
  Oct=sum(D10$OrderTotal),
  Nov=sum(D11$OrderTotal),
  Dec=sum(D12$OrderTotal)
)

D1 <- OrderObj$extractMonth(1, 2017)
D2 <- OrderObj$extractMonth(2, 2017)
D3 <- OrderObj$extractMonth(3, 2017)
D4 <- OrderObj$extractMonth(4, 2017)
D5 <- OrderObj$extractMonth(5, 2017)
D6 <- OrderObj$extractMonth(6, 2017)
D7 <- OrderObj$extractMonth(7, 2017)
D8 <- OrderObj$extractMonth(8, 2017)
D9 <- OrderObj$extractMonth(9, 2017)

MonthlyTotal2017 <- c(
  Jan=sum(D1$OrderTotal),
  Fab=sum(D2$OrderTotal),
  Mar=sum(D3$OrderTotal),
  Apr=sum(D4$OrderTotal),
  May=sum(D5$OrderTotal),
  Jun=sum(D6$OrderTotal),
  Jul=sum(D7$OrderTotal),
  Aug=sum(D8$OrderTotal),
  Sep=sum(D9$OrderTotal),
  Oct=0,
  Nov=0,
  Dec=0
)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
barplot(MonthlyTotal2014, main="Revenues 2014", ylim=c(0, 30000))
barplot(MonthlyTotal2015, main="Revenues 2015", ylim=c(0, 30000))
barplot(MonthlyTotal2016, main="Revenues 2016", ylim=c(0, 30000))
barplot(MonthlyTotal2017, main="Revenues 2017", ylim=c(0, 30000))



sum(MonthlyTotal2014)
sum(MonthlyTotal2015)
sum(MonthlyTotal2016)
sum(MonthlyTotal2017)
ddd = ts["201601"]

par(mfrow=c(3,4)) 
plot(table(.indexhour(ts["201601"])))
plot(table(.indexhour(ts["201602"])))
plot(table(.indexhour(ts["201603"])))
plot(table(.indexhour(ts["201604"])))
plot(table(.indexhour(ts["201605"])))
plot(table(.indexhour(ts["201606"])))
plot(table(.indexhour(ts["201607"])))
plot(table(.indexhour(ts["201608"])))
plot(table(.indexhour(ts["201609"])))
plot(table(.indexhour(ts["201610"])))
plot(table(.indexhour(ts["201611"])))
plot(table(.indexhour(ts["201612"])))




plot(table(.indexwday(ts["2015"])), ylim=c(0,700))
plot(table(.indexwday(ts["2016"])), ylim=c(0,700))
plot(table(.indexwday(ts["2017"])), ylim=c(0,700))

head(ddd)
head(to.daily(ddd) )
to.daily(ts["2014-5"])
.indexwday(ts["2014-5"])
