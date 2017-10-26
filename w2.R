rm(list = ls())
library(quantmod)

### 获取数据
dat <- getSymbols('GOOGL', from = '2012-09-27', to = '2017-09-27')
dim(GOOGL)
head(GOOGL)
#da <- read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL.csv")

### 时间序列图-价格
chartSeries(GOOGL, theme = 'white')
#addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
#addRSI(n = 14, maType = "EMA", wilder = TRUE)
chartSeries(GOOGL, theme = 'white', TA = NULL) #without volumn

#library(tseries)
tdx <- c(1:length(GOOGL[,6]))/250 + 2012.75
plot(tdx, GOOGL[,6], type = 'l', xlab = 'year', ylab = 'daily price', main = 'GOOGL [2012/09/27-2017/09/27]')
grid()
abline(v = 2016.75, lwd = 2, lty = 1, col = 'grey')
abline(h = 720, lwd = 3, lty = 3, col = 'grey30')

### 数据预处理
GOOGL.simrtn <- diff(GOOGL[,6])/c(1, GOOGL[1:1256,6])
GOOGL.logrtn <- diff(log(GOOGL[,6]))
head(GOOGL.simrtn)
head(GOOGL.logrtn)


### 时间序列图-收益率对比
chartSeries(GOOGL.simrtn, theme = 'white')
chartSeries(GOOGL.logrtn, theme = 'white')
simrtn <- as.numeric(GOOGL.simrtn[2:1257])
logrtn <- as.numeric(GOOGL.logrtn[2:1257])
#时间序列图-对数收益率
tdx2 <- c(1:length(logrtn))/250 + 2012.75
par(mfrow = c(2,1))
plot(tdx2, simrtn, type = 'l', xlab = 'year', ylab = 'simple return')
plot(tdx2, logrtn, type = 'l', xlab = 'year', ylab = 'log return')

plot(tdx2, logrtn, type = 'l', xlab = 'year', ylab = 'log return')
grid()


### 基本统计量
library(fBasics)
basicStats(logrtn)#图形之外的说明收益率序列特征的数据
#mean()
#var()
#sd()
#skewness()
#kurtosis()
normalTest(logrtn,method='jb')
adf.test(logrtn)


#概率密度图
#概率直方图与正态概率密度图的对比
hist(logrtn, breaks = 60, col = 'grey', xlab = '', main = 'log return', prob=TRUE)
x <- seq(min(logrtn), max(logrtn), 0.001)
y <- dnorm(x, mean = mean(logrtn), sd = sd(logrtn))
#dens <- density(logrtn)
#plot(dens$x, dens$y, xlab = 'logrtn', ylab = 'density', type = 'l')
lines(x, y, col = 'brown', lwd = 2, lty = 2)

# Q-Q plot
qqnorm(logrtn, col="brown", ylab='quantile of log returns', xlab='normal quantile')
qqline(logrtn)

# ACF plot
acf(logrtn,lag=40,main="log returns")




