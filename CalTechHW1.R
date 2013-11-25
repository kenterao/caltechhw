# This is a test script
library(ISLR)


a = rnorm(1000)

summary(a)
getwd()

## Write perceptron algorithm




## function: getRandomHyperplane through [0,1] x [0,1]
## 0 = w0 + w1*x1 + w2*x2

getRandomHyperplane = function() {
	r = runif(4)	
	x1 = r[1]; y1 = r[2]; x2 = r[3]; y2 = r[4]
	w0 = y1*x2 - x1*y2
	w1 = y2 - y1
	w2 = x1 - x2
	W = c(w0,w1,w2)
}

getRandomPoints = function(N) {
	D = array( c(rep(1,N),runif(2*N)),dim=c(N,3))
}


getLabels = function(N) {
	D = cbind(D, sign(D %*% W))
}



N = 100
D = getRandomPoints(N)

probvec = c()
for(jj in 1:1000) {
	W = getRandomHyperplane()
	trueLabel = sign(D %*% W)
	#print(trueLabel)

	counter = 0
	M = 1000
	for(ii in 1:M) {
		W2 = getRandomHyperplane()
		guessLabel = sign(D %*% W2)
		#print(guessLabel)

		if( sum(trueLabel == guessLabel) == N ) {
			counter = counter + 1
		}
	}
	avg = counter/M
	print(avg)
	probvec = c(probvec,avg)
}
summary(probvec)

#########################################

## function: updatePLA
## input: D containing trueLabel, last W
## output: W

updatePLA = function(D,W) {
	guessClass = sign( D[,1:3] %*% W )
	trueClass = D[,4]
	wrongIndex = which(trueClass!=guessClass)
	if( length(wrongIndex) > 0 ) {
		firstWrongIndex = wrongIndex[1]
		W = W + D[firstWrongIndex,4] * D[firstWrongIndex,1:3]
	} 
	return(W)
}


countervec = c()
for( ii in 1:1000 ) {
	N = 100
	D = getRandomPoints(N)
	W0 = getRandomHyperplane()
	D = cbind(D, sign(D %*% W0))

	W = getRandomHyperplane()
	
	counter = 0
	repeat{
		W = updatePLA(D,W)
		if( sum(D[,4] == sign(D[,1:3] %*% W)) == N ) {
			break
		}
		counter = counter + 1
	}
	#print(counter)
	countervec = c(countervec,counter)
}
summary(countervec)




## function: input dataset D and hyperplane, output classification fit metric

## Pass around hyperplane object (w vector)

## function: PLAiterator, input dataset D and hyperplane (w vector)


## do while loop until convergence

