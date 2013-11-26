## Homework 6.2

library(foreign)

inDF = read.table("http://work.caltech.edu/data/in.dta",FALSE)
names(inDF) = c("x1","x2","y")
summary(inDF)

outDF = read.table("http://work.caltech.edu/data/out.dta",FALSE)
names(outDF) = c("x1","x2","y")
summary(outDF)

phi = function(xDF) {
	N = dim(xDF)[1]
	x1 = xDF$x1; x2 = xDF$x2
	zDF = data.frame(z0=rep(1,N), z1=x1, z2=x2, 
		z3=x1*x1, z4=x2*x2, z5=x1*x2, z6=abs(x1-x2), z7=abs(x1+x2))
	zDF
}

zDF = phi(inDF)
Z = as.matrix(zDF)
	
w_hat = solve( t(Z) %*% Z ) %*% t(Z) %*% y	
y_hat = sign(Z %*% w_hat)
y = as.matrix(inDF$y)
# in sample error
sum(y!=y_hat) / length(y_hat)


zDF2 = phi(outDF)
Z2 = as.matrix(zDF2)
y_hat2 = sign(Z2 %*% w_hat)
y2 = as.matrix(outDF$y)
# out of sample error
sum(y2!=y_hat2) / length(y_hat2)

## Homework 6.3
error_lambda = function(k) {
	print(k)
	lambda = 10^k
	w_hat = solve( t(Z) %*% Z + lambda * diag(dim(Z)[2]) ) %*% t(Z) %*% y
	
	y_hat = sign(Z %*% w_hat)
	y = as.matrix(inDF$y)
	# in sample error
	print(sum(y!=y_hat) / length(y_hat))
	
	y_hat2 = sign(Z2 %*% w_hat)
	y2 = as.matrix(outDF$y)
	# out of sample error
	print(sum(y2!=y_hat2) / length(y_hat2))
	sum(y2!=y_hat2) / length(y_hat2)
}

error_lambda(-3)

## Homework 6.4
error_lambda(3)

## Homework 6.5, 6.6
error_lambda(2)
error_lambda(1)
error_lambda(0)
error_lambda(-1)
error_lambda(-2)






