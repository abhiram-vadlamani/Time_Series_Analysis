summary(fit <- lm(chicken ~ time(chicken), na.action = NULL))
# Fitting results
plot(chicken, ylab = "US cents per pound", col = "blue", lwd = 2,
     main = "Price of chicken with fitted linear trend line 
     (spot price, Georgia docks, 08/01-07/16)")  
abline(fit, col = "red", lwd = 2) # add the fitted line

##Example: Pollution, Temperature and Mortality

par(mfrow = c(3, 1)) # plot the data
plot(cmort, main = "Cardiovascular Mortality", xlab = "", ylab = "",
     col = "red", lwd = 2)
plot(tempr, main = "Temperature", xlab = "", ylab = "", col = "blue",
     lwd = 2)
plot(part, main = "Particulates", xlab = "", ylab = "", col = "green",
     lwd = 2)
ts.plot(cmort, tempr, part, col = c("red", "blue", "green")) #all on the same graph

## Scatter plots: 

pairs(cbind(Mortality = cmort, Temperature = tempr, Particulates = part))
temp <- tempr - mean(tempr) # center temperature
temp2 <- temp^2
trend <- time(cmort) # time
#There is possible linear relation between mortality and particulates

fit <- lm(cmort ~ trend + temp + temp2 + part, na.action = NULL) 
summary(fit) # regression results

# Note: Each addition is significant.

summary(aov(lm(cmort ~ cbind(trend, temp, temp2, part)))) # Table 2.1

num <- length(cmort) # sample size 
AIC(fit)/num - log(2 * pi) # AIC

BIC(fit)/num - log(2 * pi) # BIC

AICc <- log(sum(resid(fit)^2)/num) + (num + 5)/(num - 5 - 2) # AICc
AICc

## Detrending Via regression

par( mfrow = c(3,1))

plot(chicken, ylab = "US cents per pound", col = "blue", lwd = 2,
     main = "Price of chicken with fitted linear trend line 
     (spot price, Georgia docks, 08/01-07/16)") 
abline(fit, col = "red", lwd = 2) # add the fitted line

fit <- lm(chicken ~ time(chicken), na.action = NULL) # regress chicken 
plot(resid(fit), type = "l", main = "Price of chicken detrended via regression (spot price, Georgia docks, 08/01-07/16)")
dev.off()

## Differencing Global Temperatures

plot(globtemp, type = "o", ylab = "Global Temperature Deviations",
     main = "Yearly average global temperature deviations (1880-2015) in degrees centrigrade (1950-1980")

par(mfrow = c(2, 1))
plot(diff(globtemp), type = "l", main = "Global temperature detrended via first difference") 
acf(diff(gtemp), 48)
dev.off()

## Chicken prices detrended via differencing

plot(diff(chicken), type = "l", main = "Price of chicken detrended via first differenece(spot price, Georgia docks, 08/01-07/16)")

##Comparing detrending via differencing and regression 

par(mfrow = c(3, 1)) # plot ACFs
acf(chicken, 48, main = "chicken")
acf(resid(fit), 48, main = "detrended via regression") 
acf(diff(chicken), 48, main = "detrended via first difference")

## SMOOTHING IN TIME SERIES CONTEXT

# MOVING AVERGAE SMOOTHER 

#This particular method removes (filters out) the obvious annual temperature cycle 
#helps emphasize the El Niño cycle

wgts <- c(0.5, rep(1, 11), 0.5)/12
soif <- filter(soi, sides = 2, filter = wgts)
par()
plot(soi, main = "Moving average smoother of SOI.The insert shows the shape of the moving average (\"boxcar\") kernel [not drawn to scale]") 
lines(soif, lwd = 3, col = "blue")
par(fig = c(0.65, 1, 0.65, 1), mar = c(5, 3.5, 4.1, 2.1), new = TRUE) # the insert 
nwgts <- c(rep(0, 20), wgts, rep(0, 20))
plot(nwgts, type = "l", ylim = c(-0.02, 0.1), xaxt = "n", yaxt = "n", ann = FALSE)




