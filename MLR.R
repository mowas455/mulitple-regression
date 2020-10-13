
data(iris)
head(iris)

data = as.matrix(iris)

head(data)


x = apply(data[,1:3], MARGIN = 2, FUN = as.numeric)

y = as.numeric(data[,4])

xt = t(x)

xtx = crossprod(x,x)
ixtx = solve(crossprod(x,x))# LU Decomposition

#ixtx2 = qr.solve(xtx) # QR Decomposition
xty = crossprod(x,y)

beta = ixtx %*% xty

print(beta) # Beta

yhat = x %*% beta

residual = y - yhat
# Residual Variance


df=(nrow(x)-ncol(x))

residual.variance = (crossprod(residual,residual))/df

class(residual.variance)

dim(residual.variance)

beta.variance = residual.variance %*% diag(ixtx)

names(beta.variance) = names(ixtx[1,])


names(x)

