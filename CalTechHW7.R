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









