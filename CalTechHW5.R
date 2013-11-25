## Homework #5

d = 8
sigma = 0.1
delta = 0.008

N = (sigma^2 / (sigma^2 - delta)) * (d+1)
N


f_N = 10:1000

f_delta = sigma^2 * (1 - (d+1)/f_N)

plot(f_N,f_delta)


## Homework 5.5


E_uv = function(u,v) { 
	val = ( u*exp(v) - 2*v*exp(-u) )^2
	return(val)
}

grad_E_uv = function(u,v) {
	dE_du = 2 * (u*exp(v) - 2*v*exp(-u)) * (exp(v) + 2*v*exp(-u))
	dE_dv = 2 * (u*exp(v) - 2*v*exp(-u)) * (u*exp(v) - 2*exp(-u))
	del_E = c(dE_du, dE_dv)
	return(del_E)
}

myW = c(1,1)
nu = 0.1
print(myW)
print(E_uv(myW[1],myW[2]))

for( ii in 1:20 ) {
	del_E = grad_E_uv(myW[1], myW[2])
	myW = myW - nu*del_E
	myE = E_uv(myW[1], myW[2])
	print(ii)
	print(myW)
	print(myE)
}


## Homework 5.7
myW = c(1,1)
nu = 0.1
print(myW)
print(E_uv(myW[1],myW[2]))

for( ii in 1:15 ) {
	del_E = grad_E_uv(myW[1], myW[2])
	myW = myW - nu*(del_E * c(1,0))

	del_E = grad_E_uv(myW[1], myW[2])
	myW = myW - nu*(del_E * c(0,1))

	myE = E_uv(myW[1], myW[2])

	print(ii)
	print(myW)
	print(myE)
}

## Homework 5.8
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
plane
df = getRandomPoints(plane,100)
df

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

## Homework 5.7
epoch_list = c()
eout_list = c()
for(ii in 1:10) {

plane = getRandomHyperplane()
df = getRandomPoints(plane,100)

w_old = matrix(c(1,1,1),3,1)
w_new = matrix(c(1,0,0),3,1)
nu = 0.01
num_epoch = 0
while( sqrt(t(w_new - w_old) %*% (w_new - w_old)) >= 0.01 ) {
	num_epoch = num_epoch + 1
	w_old = w_new
	N = dim(df)[1]
	df = df[sample(N),]
	for(jj in 1:N) {
		y_n = df[jj,"y"] 
		x_n = df[jj,1:3]
		e_n  = (-1 * y_n * x_n) / (1 + exp( y_n * as.matrix(x_n) %*% w_new ) )	
		w_new = w_new - nu * e_n
		w_new = as.matrix(as.numeric(w_new))
	}
	#print(num_epoch)
	#print(w_old)
	#print(w_new)
}
epoch_list = c(epoch_list, num_epoch)
print(ii)
print(num_epoch)

df2 = getRandomPoints(plane,100)
s = as.matrix(df2[,1:3]) %*% w_new
theta_s = exp(s) / (1 + exp(s))
N = dim(df2)[1]
E_out = (1/N) * sum( ( (2*theta_s-1) - df2$y )^2 )
print(E_out)
eout_list = c(eout_list, E_out)
}
summary(epoch_list)
summary(eout_list)

## Additional comment

w0 = W[1]
w1 = W[2]
w2 = W[3]














































