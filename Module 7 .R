# Gaussian white noise series and its 3-point moving average
~
w <- rnorm(500, 0, 1) # 500 N(0,1) variates
plot.ts(w, ylim = c(-3, 3), main = "white noise")
v <- filter(w, sides = 2, filter = rep(1/3, 3)) # moving average
plot.ts(v, ylim = c(-3, 3), main = "moving average")

# Autoregression example 
~
w <- rnorm(550, 0, 1) # 50 extra to avoid startup problems
x <- filter(w, filter = c(1, -0.9), method = "recursive")[-(1:50)] # remove first 50
plot.ts(x, ylab = "autogression", main = "Autoregressive series generated from model x_t=x_{t-1}-0.9x_{t-2}+w")
y <- filter(w, filter = c(1, -0.3), method = "recursive")[-(1:50)] # remove first 50
plot.ts(y, ylab = "autogression", main = "Autoregressive series generated from model x_t=x_{t-1}-0.3x_{t-2}+w")

# Random Walk 
~
  set.seed(155) # so you can reproduce the results
w <- rnorm(200)
x <- cumsum(w)
wd <- w + 0.2
xd <- cumsum(wd)
plot.ts(xd, ylim = c(-5, 55), main = "random walk", ylab = "")
lines(x, col = 4)
abline(h = 0, col = 4, lty = 2)

# SOI VS RECRUITMENT DATA 

acf(soi, main = "Sample autocorrelation function (SACF) of SOI")

r <- round(acf(soi, 6, plot = FALSE)$acf[-1], 3) # first 6 sample acf values

## Scatter plot for data 1 month and 6 months apart
par(mfrow = c(1, 2), mar = c(3, 3, 3, 0.5), mgp = c(1.6, 0.6, 0))
plot(lag(soi, -1), soi, main = "SOI pairs of values 1 month apart")
legend("topleft", legend = r[1])
plot(lag(soi, -6), soi, main = "SOI pairs of values 6 months apart")

## Prewhitening
~
  set.seed(1492)
num <- 120
t <- 1:num
X <- ts(2 * cos(2 * pi * t/12) + rnorm(num), freq = 12)
Y <- ts(2 * cos(2 * pi * (t + 5)/12) + rnorm(num), freq = 12)
par(mfrow = c(1, 2), mgp = c(1.6, 0.6, 0), mar = c(3, 3, 1, 1))
plot(X)
plot(Y)
## Seasonality observed through ACF's and CCF suggests cross-correlation despite independence
par(mfrow = c(3, 2), mgp = c(1.6, 0.6, 0), mar = c(3, 3, 1, 1))
acf(X, 48, ylab = "ACF(X)")
acf(Y, 48, ylab = "ACF(Y)")
ccf(X, Y, 24, ylab = "CCF(X,Y)")
## Running a regression on sin and cos functions 
par(mgp = c(1.6, 0.6, 0), mar = c(3, 3, 1, 1))
Yw <- resid(lm(Y ~ cos(2 * pi * t/12) + sin(2 * pi * t/12), na.action = NULL))
ccf(X, Yw, 24, ylab = "CCF(X,Yw)", ylim = c(-0.3, 0.3))

## Regression with lagged variables

fish <- ts.intersect(rec, soiL6 = lag(soi, -6), dframe = TRUE)
summary(fit1 <- lm(rec ~ soiL6, data = fish, na.action = NULL))

lag1.plot(soi, 12)
dev.off() 

## Continued 

# we saw that the relationship is nonlinear and different when SOI is positive or negative. 
#In this case, we may consider adding a dummy variable to account for this change

dummy = ifelse(soi<0, 0, 1)
soiL6 <- lag(soi,-6)
fish = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))

plot(soiL6, rec, main = "Recruitment (Rt) vs SOI lagged 6 months (St−6) with the fitted values of the regression as points (+) and a lowess fit (−)")
lines(lowess(soiL6, rec), col = "blue", lwd = 3)
points(soiL6, fitted(fit), pch = "+", col = 2, cex = 1.5) 
