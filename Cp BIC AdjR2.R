#B6. (a) (g)
res <- function(n){
  return (1 - (1 - 1/n)**n)
}
k <- 1:100000
plot(k, res(k), type = 'n')


#B6. (b)
X = rnorm(100)
eps = rnorm(100)

beta0 = 5
beta1 = 2
beta2 = -5
beta3 = 13
Y = beta0 + beta1 * X + beta2 * X * X + beta3 * X * X * X + eps

library(leaps)
dataxy = data.frame(y = Y, x = X)
mp = regsubsets(y ~ poly(x, 10, raw = T), data = dataxy, nvmax = 10)
mpx = summary(mp)

which.min(mpx$cp)
which.min(mpx$bic)
which.max(mpx$adjr2)

plot(mpx$cp, xlab = "Subset", ylab = "Cp", type = "l")
points(3, mpx$cp[3], pch = 1, col = "red", lwd = 5)

plot(mpx$bic, xlab = "Subset", ylab = "BIC", type = "l")
points(3, mpx$bic[3], pch = 1, col = "red", lwd = 5)

plot(mpx$adjr2, xlab = "Subset", ylab = "Adj R2", type = "l")
points(4, mpx$adjr2[4], pch = 1, col = "red", lwd = 5)

coef(mp, id = 3)
