HW 3 Problem5
========================================================


```r
# Let's set sample size n =100, and generate samples
n <- 100
samp <- rpois(n,5)
```



```r
# Directly sample from posterior (Let's do 1000 samples)
# Since the prior is Gamma(1,1), the posterior is simply: 
# Gamma(a + sum(x), b + n) = Gamma(1 + sum(x), 1 + n)  
a <- 1+sum(samp)
b <- 1 + n
post1 <- rgamma(1000, a, rate = b)
plot(post1)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
# Sample from the posterior using a random-walk Metropolis-Hasting algorithm.
# run for K iterations
k = 1000
# set prior for theta
a = 1
b = 1 
# R-W M-H tuning variance
vt =0.5
# Define holder for the sample 
theta = rep(0, k)
# set starting point
theta.s = 5.5
theta[1] = theta.s
# evaluate acceptance probability
accept = 0

for (iter in 2:k){
    theta.s = theta[iter-1] + vt*rnorm(1)
    #print(iter)
    #print(theta.s)
    loglik.cand = sum(dpois(samp, theta.s, log = TRUE))+ + dgamma(theta.s, a,rate = b, log = TRUE)
    loglik.old = sum(dpois(samp, theta[iter-1], log = TRUE)) + dgamma(theta[iter-1], a,rate = b, log=TRUE)
    ratio = loglik.cand/loglik.old
    #print("ratio = ")
    #print(ratio)
    u = runif(1)
    #print(u)
    if (u > min(1, ratio)){theta.s = theta[iter-1]} else{accept = accept + 1}
    theta[iter] = theta.s
}

print(accept/1000)
```

```
## [1] 0.988
```

```r
plot(theta)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


I use Gaussian as J, and it turns out out Posterior Gamma has heavier tails than the Normal. The samplier breaks down when moving to small/larger theta due to too big likelihood ratio.

The sampler will still work with super small random walking steps, however in this way, we won't sample the posterior distribution very well. In general I thinking using R-W M-H with a normal J to sample a 
Gamma distribution is not a good idea. Also since we only except 20% - 30% sampling in the ideal condition, so the sampling won't be as efficient as the direct method where 100% points are accepted.


```r
# Sample from the posterior using a random-walk Metropolis-Hasting algorithm.
# run for K iterations
k = 1000
# set prior for theta
a = 1
b = 1 
# R-W M-H tuning variance
vt =0.05
# Define holder for the sample 
theta = rep(0, k)
# set starting point
theta.s = 5.5
theta[1] = theta.s
# evaluate acceptance probability
accept = 0

for (iter in 2:k){
    theta.s = theta[iter-1] + vt*rnorm(1)
    #print(iter)
    #print(theta.s)
    lik.cand = sum(dpois(samp, theta.s, log = TRUE))+ + dgamma(theta.s, a,rate = b, log = TRUE)
    lik.old = sum(dpois(samp, theta[iter-1], log = TRUE)) + dgamma(theta[iter-1], a,rate = b, log=TRUE)
    ratio = lik.cand/lik.old
    #print("ratio = ")
    #print(ratio)
    u = runif(1)
    #print(u)
    if (u > min(1, ratio)){theta.s = theta[iter-1]} else{accept = accept + 1}
    theta[iter] = theta.s
}

print(accept/1000)
```

```
## [1] 0.999
```

```r
plot(theta)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



```r
# Sample from the posterior using auxiliary sampling
# run for K iterations
k = 1000

# set prior
a = 1
b = 1

# Define holder for the sample 
theta = rep(0, k)
u = rep(0, k)
# set starting point
theta[1] = 5.5
u[1] = 200

for (iter in 2:k){
    u[iter] = dunif(0, min=0, max = dgamma(theta[iter-1], a, rate = b))
    findtheta= pgamma(u[iter], a, rate = b)
    theta[iter] = dunif(theta[iter-1], min =theta[iter-1], max = dgamma(theta[iter-1], a, rate = b))
}
```

```
## Warning: NaNs produced
```



