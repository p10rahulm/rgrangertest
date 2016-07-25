# Loading Data. Sourced from 
# a) prisoner data: https://commons.wikimedia.org/wiki/File:U.S._incarceration_rates_1925_onwards.png
# b) gdp data: http://www.multpl.com/us-gdp-inflation-adjusted/table
# saved as prisonervgdp.csv

Paroledata = read.csv("prisonervgdp.csv")

# convert data into usable form
Paroledata$Total = as.numeric(gsub(",", "", Paroledata$Total))
Paroledata$MaleTotal = as.numeric(gsub(",", "", Paroledata$MaleTotal))
Paroledata$Female.Total = as.numeric(gsub(",", "", Paroledata$Female.Total))
Paroledata$GDP = Paroledata$GDP/10000000000000
#get summary of data
str(Paroledata)
#plot the data
par(mfrow = c(2,2))
plot(Paroledata$GDP,x = Paroledata$Year)
plot(Paroledata$Total,x = Paroledata$Year)
plot(Paroledata$Incarceration.Rate,x = Paroledata$Year)
plot(Paroledata$Female.Rate,x = Paroledata$Year)
#The plot provide that some correlation exists between the variables but no further
# Lets view the data to get further understanding
View(Paroledata)

#for checking stationary data using the forecast package
install.packages("forecast")
library(forecast)
# test for unit root and number of differences required, you can also test for seasonality with nsdiffs
ndiffs(Paroledata$Incarceration.Rate, alpha=0.05, test=c("kpss")) 
ndiffs(Paroledata$GDP, alpha=0.05, test=c("kpss")) 

# > [1] 2 
# Non zero answer so ok

dgdp <- diff(Paroledata$GDP)
dincarceration <- diff(Paroledata$Incarceration.Rate)
dtotal <- diff(Paroledata$Total)
par(mfrow = c(2,1))
plot.ts(dtotal)
plot.ts(dgdp)

# do changes in prisoners granger-cause gdp change?
install.packages("lmtest")
library(lmtest)
grangertest(dgdp ~ dincarceration, order=3)
grangertest(dincarceration ~ dgdp, order=3)
grangertest(dgdp ~ dtotal, order=3)

grangertest(GDP ~ Total, order=3,data = Paroledata)
grangertest(GDP ~ Total, order=8,data = Paroledata)

# we therefore reject that incarceration rates do not cause changes in gdp
# Let us check for different number of years

pvalues = vector(mode = "numeric",length = 12L)

for (lag in 1:12){
  pvalues[lag] = grangertest(dgdp ~ dincarceration, order=lag)[4][,1][2]
}

pvalues2 = vector(mode = "numeric",length = 12L)

for (lag in 1:12){
  pvalues2[lag] = grangertest(dgdp ~ dtotal, order=lag)[4][,1][2]
}


mypar = par(mfrow = c(1,1))
plot(pvalues2, main="P-value for rejecting hypothesis that gdp changes is not
     caused by total  number of prisoners", sub="Rejected Hypothesis",
     xlab="Lag in Years", ylab="p values",
     xlim=c(0, 12), ylim=c(0, 0.10))
abline(a = 0.01,b = 0,col=2,lty=1)
abline(a = 0.05,b = 0,col=3,lty=1)
abline(v = 5,col=2,lty=1)
abline(v = 10,col=3,lty=1)

# Most lines lie within p value of 0.1 where we can reject the hypothesis that 
# incarceration caused changes in gdp!
