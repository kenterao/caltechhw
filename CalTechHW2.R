
## Problem 1

myvec = c()
for(ii in 1:10000) {
	nu_min = min(rbinom(1000,10,0.5))/10
	myvec = c(myvec,nu_min)
}
summary(myvec)


## Problem 5

getRandomHyperplane = function() {
	r = runif(4)*2 - 1	
	x1 = r[1]; y1 = r[2]; x2 = r[3]; y2 = r[4]
	w0 = y1*x2 - x1*y2
	w1 = y2 - y1
	w2 = x1 - x2
	W = c(w0,w1,w2)
}

getRandomPoints = function(N) {
	D = array( c(rep(1,N),runif(2*N)*2 - 1),dim=c(N,3))
}


myvec = c()
for(ii in 1:1000) {
N = 100
D = getRandomPoints(N)

W0 = getRandomHyperplane()
df = data.frame(a=D[,1],x1=D[,2],x2=D[,3])
df$y = sign(D[,1:3] %*% W0)


reg = lm("y ~ x1 + x2", data=df)
summary(reg)
df$yhat = sign(predict(reg))

wrongPct = sum(df$y != df$yhat) / N
myvec = c(myvec,wrongPct)
}
summary(myvec)

## Problem 6


myvec = c()
for(ii in 1:1000) {
N = 100
D = getRandomPoints(N)

W0 = getRandomHyperplane()
df = data.frame(a=D[,1],x1=D[,2],x2=D[,3])
df$y = sign(D[,1:3] %*% W0)


reg = lm("y ~ x1 + x2", data=df)
summary(reg)

M = 1000
D2 = getRandomPoints(M)
df2 = data.frame(a=D2[,1],x1=D2[,2],x2=D2[,3])
df2$y = sign(D2[,1:3] %*% W0)

df2$yhat = sign(predict(reg, df2))

wrongPct = sum(df2$y != df2$yhat) / M
myvec = c(myvec,wrongPct)
}
summary(myvec)

## Problem 8
getRandomPoints = function(N) {
	D = array( runif(2*N)*2 - 1,dim=c(N,2))
}

myvec = c()
for(ii in 1:1000) {
N = 1000
D = getRandomPoints(N)
df = data.frame(x1 = D[,1], x2 = D[,2])
df$y = sign( df$x1*df$x1 + df$x2*df$x2 - 0.6 )

# add 10% bit flip noise
noise = (runif(N)>0.10) * 2 - 1
df$y = df$y * noise

reg = lm('y ~ x1 + x2', df)
summary(reg)
df$yhat = sign(predict(reg, df))

wrongPct = sum(df$y != df$yhat) / N
myvec = c(myvec,wrongPct)
}
summary(myvec)

## Problem 9
N = 1000
D = getRandomPoints(N)
df = data.frame(x1 = D[,1], x2 = D[,2])
df$y = sign( df$x1*df$x1 + df$x2*df$x2 - 0.6 )

# add 10% bit flip noise
noise = (runif(N)>0.10) * 2 - 1
df$y = df$y * noise

df$x1x2 = df$x1 * df$x2
df$x1x1 = df$x1 * df$x1
df$x2x2 = df$x2 * df$x2

reg = lm("y ~ 1 + x1 + x2 + x1x2 + x1x1 + x2x2", df)
summary(reg)

## Problem 10
myvec = c()
for(ii in 1:1000) {

N = 1000
D = getRandomPoints(N)
df = data.frame(x1 = D[,1], x2 = D[,2])
df$y = sign( df$x1*df$x1 + df$x2*df$x2 - 0.6 )

# add 10% bit flip noise
noise = (runif(N)>0.10) * 2 - 1
df$y = df$y * noise

df$x1x2 = df$x1 * df$x2
df$x1x1 = df$x1 * df$x1
df$x2x2 = df$x2 * df$x2

reg = lm("y ~ 1 + x1 + x2 + x1x2 + x1x1 + x2x2", df)

N = 1000
D2 = getRandomPoints(N)
df2 = data.frame(x1 = D2[,1], x2 = D2[,2])
df2$y = sign( df2$x1*df2$x1 + df2$x2*df2$x2 - 0.6 )
noise = (runif(N)>0.10) * 2 - 1
df2$y = df2$y * noise
df2$x1x2 = df2$x1 * df2$x2
df2$x1x1 = df2$x1 * df2$x1
df2$x2x2 = df2$x2 * df2$x2
df2$yhat = sign(predict(reg,df2))

wrongPct = sum(df2$y != df2$yhat) / N
myvec = c(myvec,wrongPct)
}
summary(myvec)






