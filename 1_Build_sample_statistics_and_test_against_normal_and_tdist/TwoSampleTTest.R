twosample.tstatistic = function(x1, x2){
  # sample sizes
  n1 = length(x1)
  n2 = length(x2)
  # standard deviations
  s1 = sd(x1)
  s2 = sd(x2)
  # test statistic
  sig = ((n1-1)*s1**2 +(n2-1)*s2**2)/((n1-1)+(n2-1))
  ttstat = (mean(x1) - mean(x2))/(1/n1 + 1/n2)**0.5/sig
  pval = (1-pt(abs(ttstat), df = n1+n2-2))*2
  return(c(ttstat, pval))
}


# Test the code with samples from Normal distribution.
alpha = 0.05
# Sample size
m = 15
# number of MC samples
N =100000
n.reject =0
for (i in 1:N){
  x1 = rnorm(m)
  x2 = rnorm(m)
  twosample.tstat = twosample.tstatistic(x1,x2)[1]
  if (abs(twosample.tstat) > qt(1-alpha/2, df = m-1)){
    n.reject = n.reject +1
  }
}

true.sig.level = n.reject/N
true.sig.level


# Test the code with samples from a t distribution.
alpha = 0.05
# Sample size
m = 15
# number of MC samples
N =100000
n.reject =0
for (i in 1:N){
  x1 = rt(m, df =3)
  x2 = rt(m, df =3)
  twosample.tstat = twosample.tstatistic(x1,x2)[1]
  if (abs(twosample.tstat) > qt(1-alpha/2, df = m-1)){
    n.reject = n.reject +1
  }
}

true.sig.level = n.reject/N
true.sig.level




