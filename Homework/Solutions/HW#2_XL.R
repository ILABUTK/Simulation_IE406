## Generate 10000 random numbers using the LCM method. Feel free to use any programming language or
## software package. X0=27, a=17, c=9, and m=10000.
set.seed(1)
x0=27;a=17;c=9;m=10000
r = rep(0,m)
r[1] = (a*x0+c) %% m
for (i in 2:m){
	r[i] = (a*r[i-1] + c) %% m
}

x=r/m

hist(x)

#test the first 10 RNs to see whether they follow uniform distribution
x10=x[1:10]
x10 #list
ks.test(x10,punif)

# 	One-sample Kolmogorov-Smirnov test

# data:  x10
# D = 0.2052, p-value = 0.7217
# alternative hypothesis: two-sided
# Failed to reject the null hypothesis. i.e., it is uniform.

##___________________
## ERROR IF NUMBERS ARE EVEN BIGGER
## x0=0;a=6364136223846793005;c=1442695040888963407;m=10000

## This works.
x0=6413622;a=63641362238;c=1442695047;m=10000
r = rep(0,m)
r[1] = (a*x0+c) %% m
for (i in 2:m){
	r[i] = (a*r[i-1] + c) %% m
}

x=r/m

hist(x)
##___________________


## get exponential distribution via inverse method
lambda=10
z= -1/lambda*log(1-x)
hist(z)

#T-test the first 20 RNs
##method1
x20 = x[1:20]
x20 #list
mu=rep(0.5,20)
t.test(x20,mu,conf.level = 0.99)

##method2
x20 = x[1:20]-0.5
t.test(x20,conf.level = 0.99)

# 	Welch Two Sample t-test

# data:  x20 and mu
# t = 0.5473, df = 19, p-value = 0.5906
# alternative hypothesis: true difference in means is not equal to 0
# 99 percent confidence interval:
#  -0.157896  0.232596
# sample estimates:
# mean of x mean of y 
#   0.53735   0.50000 
#
# Failed to reject the null hypothesis. i.e., the mu is 0.5. 

Q2::
set.seed(1) 
n=10000
x=runif(n)
y=runif(n)
z=x^2 + y^2

p = 4*sum(z<1)/n
p
#[1] 3.1312

