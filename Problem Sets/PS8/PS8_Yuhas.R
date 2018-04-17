# Problem Set 7
# Michael Yuhas


#Change working directory for use in OSCER

dird = "C://Users//MJYuh//Documents//OU 2017-2018//Spring 2018//ECON 5970//PS 8"
setwd(dird)

# Libraries
library(nloptr)


# Question 4
set.seed(100)

X = matrix(rnorm(1000000), 10)
X[,1] = 1

eps = rnorm(100000, mean = 0, sd = 0.5)

beta = c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y = beta%*%X + eps

# Question 5

#Debug: Error: cannot allocate vector of size 74.5 Gb

Beta.ols = ((crossprod(X)^(-1))%*%(crossprod(X,Y)))

# Question 6

# stepsize
alpha = 0.0000003

# number of iterations
iter = 500

# define the gradient
gradient = function(x) return(beta)

# randomly initialize a value to x
set.seed(100)
x = floor(runif(1)*10)

# create a vector to contain all xs for all steps
x.All = vector("numeric",iter)

# gradient descent method to find the minimum
for(i in 1:iter){
  x = x - alpha*gradient(x)
  x.All[i] = x
  print(x)
}

# print result and plot all xs for every iteration
print(paste("The minimum of f(x) is ", x, sep = ""))

# Question 7


# Question 9
lm(Y ~ X -1)


