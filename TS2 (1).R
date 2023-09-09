#install.packages("astsa")
library(astsa)
#install.packages("xts")
library(xts)
#install.packages("TTR")
library(TTR)

############################
# extracting data from Yahoo

#djia = getYahooData("^DJI",start=20060101,end=20201231,freq="daily")
#as.data.frame(djia)
#save(djia,file="djia")

#ASX200 = getYahooData("^AXJO",start=20060101,end=20201231,freq="daily")
#as.data.frame(ASX200)
#save(ASX200,file="ASX200")

######################
# plotting the returns
load("djia") # loads the data
djiar = diff(log(djia$Close))[-1] # computes the returns
plot(djiar,ylab="DJIA Returns",type="n",
     main="The daily returns of the Dow Jones Industrial Average (DJIA) from 1 January 2006 to 31 December 2020")
pdf("TS2-DJ.pdf",width=14,height=7)
lines(djiar)
dev.off()

load("ASX200") # loads the data
ASX200r = diff(log(ASX200$Close))[-1] # computes the returns
plot(ASX200r,ylab="ASX 200 Returns",type="n",
     main="The daily returns of the ASX 200 from 1 January 2006 to 31 December 2020")
pdf("TS2-ASX.pdf",width=14,height=7)
lines(ASX200r)
dev.off()

##########################
# Let us compare with lags

diffr0<-ASX200r-djiar
plot(diffr0,ylab="Difference of Returns",type="n",
     main="The difference in daily returns (no lag) between the ASX 200 and the DJIA from 1 January 2006 to 31 December 2020")
pdf("TS2-diffr0.pdf",width=14,height=7)
lines(diffr0)
dev.off()

diffr1<-lag(ASX200r,-1)-djiar
#diffr1<-reclass(diffr1, match.to="djiar")
plot(diffr1,ylab="Difference of Returns",type="n",
     main="The difference in daily returns (with lag 1) between the ASX 200 and the DJIA from 1 January 2006 to 31 December 2020")
pdf("TS2-diffr1.pdf",width=14,height=7)
lines(diffr1)
dev.off()

diffr2<-lag(ASX200r,-2)-djiar
#diffr2<-reclass(diffr2, match.to="djiar")
plot(diffr2,ylab="Difference of Returns",type="n",
     main="The difference in daily returns (with lag 2) between the ASX 200 and the DJIA from 1 January 2006 to 31 December 2020")
pdf("TS2-diffr2.pdf",width=14,height=7)
lines(diffr2)
dev.off()


