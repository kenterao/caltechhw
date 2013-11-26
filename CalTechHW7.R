## Homework 7

inDF = read.table("http://work.caltech.edu/data/in.dta",FALSE)
names(inDF) = c("x1","x2","y")
summary(inDF)
dim(inDF)

outDF = read.table("http://work.caltech.edu/data/out.dta",FALSE)
names(outDF) = c("x1","x2","y")
summary(outDF)
dim(outDF)

phi = function(xDF) {
	N = dim(xDF)[1]
	x1 = xDF$x1; x2 = xDF$x2
	zDF = data.frame(z0=rep(1,N), z1=x1, z2=x2, 
		z3=x1*x1, z4=x2*x2, z5=x1*x2, z6=abs(x1-x2), z7=abs(x1+x2))
	zDF
}

## Homework 7.1, 7.2
z_in = phi(inDF)[1:25,]
z_val = phi(inDF)[26:35,]
z_out = phi(outDF)
y_in = inDF$y[1:25]
y_val = inDF$y[26:35]
y_out = outDF$y

in_vec = c()
val_vec = c()
out_vec = c()
for( k in 3:7 ) {
	Z_in = as.matrix(z_in)[,1:(k+1)]
	Z_val = as.matrix(z_val)[,1:(k+1)]
	Z_out = as.matrix(z_out)[,1:(k+1)]
	
	w_in_hat = solve( t(Z_in) %*% Z_in ) %*% t(Z_in) %*% y_in
	y_in_hat = sign(Z_in %*% w_in_hat)
	y_val_hat = sign(Z_val %*% w_in_hat)
	y_out_hat = sign(Z_out %*% w_in_hat)
	
	# validation error
	in_err = sum(y_in!=y_in_hat) / length(y_in)
	val_err = sum(y_val!=y_val_hat) / length(y_val)
	out_err = sum(y_out!=y_out_hat) / length(y_out)
	
	in_vec = c(in_vec,in_err)
	val_vec = c(val_vec,val_err)
	out_vec = c(out_vec,out_err)
}
print(in_vec)
print(val_vec)
print(out_vec)



## Homework 7.3, 7.4, 7.5
zDF = phi(inDF)
z_in = zDF[26:35,]
z_val = zDF[1:25,]
z_out = phi(outDF)
y_in = inDF$y[26:35]
y_val = inDF$y[1:25]
y_out = outDF$y

in_vec = c()
val_vec = c()
out_vec = c()
for( k in 3:7 ) {
	Z_in = as.matrix(z_in)[,1:(k+1)]
	Z_val = as.matrix(z_val)[,1:(k+1)]
	Z_out = as.matrix(z_out)[,1:(k+1)]
	
	w_in_hat = solve( t(Z_in) %*% Z_in ) %*% t(Z_in) %*% y_in
	y_in_hat = sign(Z_in %*% w_in_hat)
	y_val_hat = sign(Z_val %*% w_in_hat)
	y_out_hat = sign(Z_out %*% w_in_hat)
	
	# validation error
	in_err = sum(y_in!=y_in_hat) / length(y_in)
	val_err = sum(y_val!=y_val_hat) / length(y_val)
	out_err = sum(y_out!=y_out_hat) / length(y_out)
	
	in_vec = c(in_vec,in_err)
	val_vec = c(val_vec,val_err)
	out_vec = c(out_vec,out_err)
}
print(in_vec)
print(val_vec)
print(out_vec)


## Homework 7.6
e1 = runif(1000)
e2 = runif(1000)
e_min = pmin(e1,e2)

mean(e1)
mean(e2)
mean(e_min)

## Homework 7.7
rho_vec = c(0,0,0,0)
rho_vec[1] = sqrt(sqrt(3)+4)
rho_vec[2] = sqrt(sqrt(3)-1)
rho_vec[3] = sqrt(9 + 4*sqrt(6))
rho_vec[4] = sqrt(9-sqrt(6))

for(rho in rho_vec) {
	print(rho)
	
	X = data.frame( x=c(-1,rho,1), y=c(0,1,0))
	print(X)
	
	index = 1:dim(X)[1]
	
	SSE = 0
	for( k in index) {
		regk = lm(y ~ 1, data=X[-k,])
		yk_hat = predict(regk, newdata=X[k,])
		yk = X$y[k]
		SSE = SSE + (yk_hat - yk)^2
	}
	print(SSE)
	
	SSE = 0
	for( k in index) {
		regk = lm(y ~ x + 1, data=X[-k,])
		yk_hat = predict(regk, newdata=X[k,])
		yk = X$y[k]
		SSE = SSE + (yk_hat - yk)^2
	}
	print(SSE)
}


## Homework 7.8
library(e1071)
getRandomHyperplane = function() {
	r = runif(4)*2 - 1	
	x1 = r[1]; y1 = r[2]; x2 = r[3]; y2 = r[4]
	w0 = y1*x2 - x1*y2
	w1 = y2 - y1
	w2 = x1 - x2
	W = matrix(c(w0,w1,w2),3,1)
}

getRandomPoints = function(W,N) {
	df = data.frame( x0=rep(1,N), x1=runif(N)*2-1, x2=runif(N)*2-1 )
	df$y = sign(as.matrix(df) %*% W)
	df
}

plane = getRandomHyperplane()
df_in = getRandomPoints(plane,10)
df_out = getRandomPoints(plane,1000)

svm.model = svm(y ~ x1 + x2, data=df_in)
yhat_out = predict(svm.model, newdata=df_out)
y_out = df_out$y
E_out = sum(y_out!=yhat_out) / length(y_out)
E_out


w_old = matrix(c(1,1,1),3,1)
w_new = matrix(c(1,0,0),3,1)
nu = 0.01
for(ii in 1:2) {
	w_old = w_new
	## permute matrix here
	N = dim(df)[1]
	for(jj in 1:N) {
		y_n = df[jj,"y"] 
		x_n = df[jj,1:3]
		e_n  = (-1 * y_n * x_n) / (1 + exp( y_n * as.matrix(x_n) %*% w_new ) )	
		w_new = w_new - nu * e_n
		w_new = as.matrix(as.numeric(w_new))
	}
	print(w_old)
	print(w_new)
}








