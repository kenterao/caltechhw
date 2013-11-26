#### Homework 4.2, 4.3

N = 10000
#N = 5
delta = 0.05
dvc = 50

## Original VC Bound
sqrt(8/N * log(4 * (2*N)^dvc / delta) )

## Rademacher Penalty Bound
sqrt(2/N * log(2*N*(N^dvc))) + sqrt(2/N * log(1/delta)) + 1/N

## Parrondo and Van den Broek
A = 1
B = -2 / N
C = -log(6/delta * (2*N)^dvc) / N

(-B + sqrt(B*B - 4*A*C) ) / 2*A

## Devroye

A = 1
B = -4 / (2*N-4)
C = - ( log(4) - log(delta) + 2*dvc*log(N) ) / (2*N-4)

(-B + sqrt(B*B - 4*A*C) ) / 2*A

## Homework 4.4

beta = c()
for(ii in 1:10000) {
	x = runif(2)*2 - 1
	y = sin(pi*x)
	
	reg = lm(y ~ x - 1)
	beta = c(beta,reg$coeff)
}
summary(beta)

## Homework 4.5
X = -100:100 / 100

g = 1.42 * X
f = sin(pi*X)

summary(abs(g-f))
mean(abs(g-f)) / 200 / 0.01


