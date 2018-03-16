pkgname <- "Bolstad"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "Bolstad-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Bolstad')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("bayes.lin.reg")
### * bayes.lin.reg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayes.lin.reg
### Title: Bayesian inference for simple linear regression
### Aliases: bayes.lin.reg
### Keywords: misc

### ** Examples


## generate some data from a known model, where the true value of the
## intercept alpha is 2, the true value of the slope beta is 3, and the
## errors come from a normal(0,1) distribution
x = rnorm(50)
y = 22+3*x+rnorm(50)

## use the function with a flat prior for the slope beta and a
## flat prior for the intercept, alpha_xbar.

bayes.lin.reg(y,x)

## use the function with a normal(0,3) prior for the slope beta and a
## normal(30,10) prior for the intercept, alpha_xbar.

bayes.lin.reg(y,x,"n","n",0,3,30,10)

## use the same data but plot it and the credible interval

bayes.lin.reg(y,x,"n","n",0,3,30,10, plot.data = TRUE)

## The heart rate vs. O2 uptake example 14.1
O2 = c(0.47,0.75,0.83,0.98,1.18,1.29,1.40,1.60,1.75,1.90,2.23)
HR = c(94,96,94,95,104,106,108,113,115,121,131)
plot(HR,O2,xlab="Heart Rate",ylab="Oxygen uptake (Percent)")

bayes.lin.reg(O2,HR,"n","f",0,1,sigma=0.13)

## Repeat the example but obtain predictions for HR = 100 and 110

bayes.lin.reg(O2,HR,"n","f",0,1,sigma=0.13,pred.x=c(100,110))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bayes.lin.reg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bayes.lm")
### * bayes.lm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayes.lm
### Title: Bayesian inference for multiple linear regression
### Aliases: bayes.lm
### Keywords: misc

### ** Examples

data(bears)
bears = subset(bears, Obs.No==1)
bears = bears[,-c(1,2,3,11,12)]
bears = bears[ ,c(7, 1:6)]
bears$Sex = bears$Sex - 1
log.bears = data.frame(log.Weight = log(bears$Weight), bears[,2:7])

b0 = rep(0, 7)
V0 = diag(rep(1e6,7))

fit = bayes.lm(log(Weight)~Sex+Head.L+Head.W+Neck.G+Length+Chest.G, data = bears,
               prior = list(b0 = b0, V0 = V0))
summary(fit)
print(fit)


## Dobson (1990) Page 9: Plant Weight Data:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

lm.D9 <- lm(weight ~ group)
bayes.D9 <- bayes.lm(weight ~ group)

summary(lm.D9)
summary(bayes.D9)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bayes.lm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bayes.t.test")
### * bayes.t.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bayes.t.test
### Title: Bayesian t-test
### Aliases: bayes.t.test bayes.t.test.default bayes.t.test.formula

### ** Examples

bayes.t.test(1:10, y = c(7:20))      # P = .3.691e-01

## Same example but with using the joint conjugate prior
## We set the prior means equal (and it doesn't matter what the value is)
## the prior precision is 0.01, which is a prior standard deviation of 10
## we're saying the true difference of the means is between [-25.7, 25.7]
## with probability equal to 0.99. The median value for the prior on sigma is 2
## and we're using a scaled inverse chi-squared prior with 1 degree of freedom
bayes.t.test(1:10, y = c(7:20), var.equal = TRUE, prior = "joint.conj", 
             m = c(0,0), n0 =  rep(0.01, 2), sig.med = 2)

##' Same example but with a large outlier. Note the assumption of equal variances isn't sensible
bayes.t.test(1:10, y = c(7:20, 200)) # P = .1979    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)

## Traditional interface
with(sleep, bayes.t.test(extra[group == 1], extra[group == 2]))

## Formula interface
bayes.t.test(extra ~ group, data = sleep)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bayes.t.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bears")
### * bears

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bears
### Title: bears
### Aliases: bears
### Keywords: datasets

### ** Examples


data(bears)
boxplot(Weight~Sex, data = bears)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bears", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("binobp")
### * binobp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binobp
### Title: Binomial sampling with a beta prior
### Aliases: binobp
### Keywords: misc

### ** Examples


## simplest call with 6 successes observed in 8 trials and a beta(1,1) uniform
## prior
binobp(6,8)

## 6 successes observed in 8 trials and a non-uniform beta(0.5,6) prior
binobp(6,8,0.5,6)

## 4 successes observed in 12 trials with a non uniform beta(3,3) prior
## plot the stored prior, likelihood and posterior
results = binobp(4, 12, 3, 3)
decomp(results)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binobp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("binodp")
### * binodp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binodp
### Title: Binomial sampling with a discrete prior
### Aliases: binodp
### Keywords: misc

### ** Examples


## simplest call with 6 successes observed in 8 trials and a uniform prior
binodp(6,8)

## same as previous example but with more possibilities for pi
binodp(6, 8, n.pi = 100)

## 6 successes, 8 trials and a non-uniform discrete prior
pi = seq(0, 1, by = 0.01)
pi.prior = runif(101)
pi.prior = sort(pi.prior / sum(pi.prior))
binodp(6, 8, pi, pi.prior)

## 5 successes, 6 trials, non-uniform prior
pi = c(0.3, 0.4, 0.5)
pi.prior = c(0.2, 0.3, 0.5)
results = binodp(5, 6, pi, pi.prior)

## plot the results from the previous example using a side-by-side barplot
results.matrix = rbind(results$pi.prior,results$posterior)
colnames(results.matrix) = pi
barplot(results.matrix, col = c("red", "blue"), beside = TRUE,
	      xlab = expression(pi), ylab=expression(Probability(pi)))
box()
legend("topleft", bty = "n", cex = 0.7, 
       legend = c("Prior", "Posterior"), fill = c("red", "blue"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binodp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("binogcp")
### * binogcp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binogcp
### Title: Binomial sampling with a general continuous prior
### Aliases: binogcp
### Keywords: misc

### ** Examples


## simplest call with 6 successes observed in 8 trials and a continuous
## uniform prior
binogcp(6, 8)

## 6 successes, 8 trials and a Beta(2, 2) prior
binogcp(6, 8,density = "beta", params = c(2, 2))

## 5 successes, 10 trials and a N(0.5, 0.25) prior
binogcp(5, 10, density = "normal", params = c(0.5, 0.25))

## 4 successes, 12 trials with a user specified triangular continuous prior
pi = seq(0, 1,by = 0.001)
pi.prior = rep(0, length(pi))
priorFun = createPrior(x = c(0, 0.5, 1), wt = c(0, 2, 0))
pi.prior = priorFun(pi)
results = binogcp(4, 12, "user", pi = pi, pi.prior = pi.prior)

## find the posterior CDF using the previous example and Simpson's rule
myCdf = cdf(results)
plot(myCdf, type = "l", xlab = expression(pi[0]),
	   ylab = expression(Pr(pi <= pi[0])))

## use the quantile function to find the 95% credible region.
qtls = quantile(results, probs = c(0.025, 0.975))
cat(paste("Approximate 95% credible interval : ["
	, round(qtls[1], 4), " ", round(qtls, 4), "]\n", sep = ""))

## find the posterior mean, variance and std. deviation
## using the output from the previous example
post.mean = mean(results)
post.var = var(results)
post.sd = sd(results)

# calculate an approximate 95% credible region using the posterior mean and
# std. deviation
lb = post.mean - qnorm(0.975) * post.sd
ub = post.mean + qnorm(0.975) * post.sd

cat(paste("Approximate 95% credible interval : ["
	, round(lb, 4), " ", round(ub, 4), "]\n", sep = ""))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binogcp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("binomixp")
### * binomixp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binomixp
### Title: Binomial sampling with a beta mixture prior
### Aliases: binomixp
### Keywords: misc

### ** Examples


## simplest call with 6 successes observed in 8 trials and a 50:50 mix
## of two beta(1,1) uniform priors
binomixp(6,8)

## 6 successes observed in 8 trials and a 20:80 mix of a non-uniform
## beta(0.5,6) prior and a uniform beta(1,1) prior
binomixp(6,8,alpha0=c(0.5,6),alpha1=c(1,1),p=0.2)

## 4 successes observed in 12 trials with a 90:10 non uniform beta(3,3) prior
## and a non uniform beta(4,12).
## Plot the stored prior, likelihood and posterior
results = binomixp(4, 12, c(3, 3), c(4, 12), 0.9)$mix

par(mfrow = c(3,1))
y.lims = c(0, 1.1 * max(results$posterior, results$prior))

plot(results$pi,results$prior,ylim=y.lims,type='l'
,xlab=expression(pi),ylab='Density',main='Prior')
polygon(results$pi,results$prior,col='red')

plot(results$pi,results$likelihood,type='l',
     xlab = expression(pi), ylab = 'Density', main = 'Likelihood')
polygon(results$pi,results$likelihood,col='green')

plot(results$pi,results$posterior,ylim=y.lims,type='l'
,xlab=expression(pi),ylab='Density',main='Posterior')
polygon(results$pi,results$posterior,col='blue')







base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binomixp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("decomp")
### * decomp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: decomp
### Title: Plot the prior, likelihood, and posterior on the same plot.
### Aliases: decomp
### Keywords: plots

### ** Examples


# an example with a binomial sampling situation
results = binobp(4, 12, 3, 3, plot = FALSE)
decomp(results)

# an example with normal data
y = c(2.99,5.56,2.83,3.47)
results = normnp(y, 3, 2, 1, plot = FALSE)
decomp(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("decomp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mean.Bolstad")
### * mean.Bolstad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mean.Bolstad
### Title: Calculate the posterior mean
### Aliases: mean.Bolstad

### ** Examples

# The useful of this method is really highlighted when we have a general 
# continuous prior. In this example we are interested in the posterior mean of 
# an normal mean. Our prior is triangular over [-3, 3]
set.seed(123)
x = rnorm(20, -0.5, 1)
mu = seq(-3, 3, by = 0.001)
mu.prior = rep(0, length(mu))
mu.prior[mu <= 0] = 1 / 3 + mu[mu <= 0] / 9
mu.prior[mu > 0] = 1 / 3 - mu[mu > 0] / 9
results = normgcp(x, 1, density = "user", mu = mu, mu.prior = mu.prior)
mean(results)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mean.Bolstad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("moisture.df")
### * moisture.df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: moisture.df
### Title: Moisture data
### Aliases: moisture.df
### Keywords: datasets

### ** Examples


data(moisture.df)
plot(final.level~proc.level, data = moisture.df)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("moisture.df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normdp")
### * normdp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normdp
### Title: Bayesian inference on a normal mean with a discrete prior
### Aliases: normdp
### Keywords: misc

### ** Examples


## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a uniform prior on mu
normdp(x,1)

## find the posterior density with a non-uniform prior on mu
mu = seq(-3,3,by=0.1)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
normdp(x,1,mu,mu.prior)

## Let mu have the discrete distribution with 5 possible
## values, 2, 2.5, 3, 3.5 and 4, and associated prior probability of
## 0.1, 0.2, 0.4, 0.2, 0.1 respectively. Find the posterior
## distribution after a drawing random sample of n = 5 observations
## from a N(mu,1) distribution y = [1.52, 0.02, 3.35, 3.49, 1.82]
mu = seq(2,4,by=0.5)
mu.prior = c(0.1,0.2,0.4,0.2,0.1)
y = c(1.52,0.02,3.35,3.49,1.82)
normdp(y,1,mu,mu.prior)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normdp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normgcp")
### * normgcp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normgcp
### Title: Bayesian inference on a normal mean with a general continuous
###   prior
### Aliases: normgcp
### Keywords: misc

### ** Examples


## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a uniform U[-3,3] prior on mu
normgcp(x, 1, params = c(-3, 3))

## find the posterior density with a non-uniform prior on mu
mu = seq(-3, 3, by = 0.1)
mu.prior = rep(0, length(mu))
mu.prior[mu <= 0] = 1 / 3 + mu[mu <= 0] /9
mu.prior[mu > 0] = 1 / 3 - mu[mu > 0] / 9
normgcp(x, 1, density = "user", mu = mu, mu.prior = mu.prior)

## find the CDF for the previous example and plot it
## Note the syntax for sintegral has changed
results = normgcp(x,1,density="user",mu=mu,mu.prior=mu.prior)
cdf = sintegral(mu,results$posterior,n.pts=length(mu))$cdf
plot(cdf,type="l",xlab=expression(mu[0])
             ,ylab=expression(Pr(mu<=mu[0])))

## use the CDF for the previous example to find a 95%
## credible interval for mu. Thanks to John Wilkinson for this simplified code

lcb = cdf$x[with(cdf,which.max(x[y<=0.025]))]
ucb = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95% credible interval : ["
           ,round(lcb,4)," ",round(ucb,4),"]\n",sep=""))

## use the CDF from the previous example to find the posterior mean
## and std. deviation
dens = mu*results$posterior
post.mean = sintegral(mu,dens)$value

dens = (mu-post.mean)^2*results$posterior
post.var = sintegral(mu,dens)$value
post.sd = sqrt(post.var)

## use the mean and std. deviation from the previous example to find
## an approximate 95% credible interval
lb = post.mean-qnorm(0.975)*post.sd
ub = post.mean+qnorm(0.975)*post.sd


cat(paste("Approximate 95% credible interval : ["
   ,round(lb,4)," ",round(ub,4),"]\n",sep=""))

## repeat the last example but use the new summary functions for the posterior
results = normgcp(x, 1, density = "user", mu = mu, mu.prior = mu.prior)

## use the cdf function to get the cdf and plot it
postCDF = cdf(results) ## note this is a function
plot(results$mu, postCDF(results$mu), type="l", xlab = expression(mu[0]),
     ylab = expression(Pr(mu <= mu[0])))

## use the quantile function to get a 95% credible interval
ci = quantile(results, c(0.025, 0.975))
ci

## use the mean and sd functions to get the posterior mean and standard deviation
postMean = mean(results)
postSD = sd(results)
postMean
postSD

## use the mean and std. deviation from the previous example to find
## an approximate 95% credible interval
ciApprox = postMean + c(-1,1) * qnorm(0.975) * postSD
ciApprox




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normgcp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normmixp")
### * normmixp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normmixp
### Title: Bayesian inference on a normal mean with a mixture of normal
###   priors
### Aliases: normmixp
### Keywords: misc

### ** Examples


## generate a sample of 20 observations from a N(-0.5, 1) population
x = rnorm(20, -0.5, 1)

## find the posterior density with a N(0, 1) prior on mu - a 50:50 mix of
## two N(0, 1) densities
normmixp(x, 1, c(0, 1), c(0, 1))

## find the posterior density with 50:50 mix of a N(0.5, 3) prior and a
## N(0, 1) prior on mu
normmixp(x, 1, c(0.5, 3), c(0, 1))

## Find the posterior density for mu, given a random sample of 4
## observations from N(mu, 1), y = [2.99, 5.56, 2.83, 3.47],
## and a 80:20 mix of a N(3, 2) prior and a N(0, 100) prior for mu
x = c(2.99, 5.56, 2.83, 3.47)
normmixp(x, 1, c(3, 2), c(0, 100), 0.8)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normmixp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normnp")
### * normnp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normnp
### Title: Bayesian inference on a normal mean with a normal prior
### Aliases: normnp
### Keywords: misc

### ** Examples


## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a N(0,1) prior on mu
normnp(x,sigma=1)

## find the posterior density with N(0.5,3) prior on mu
normnp(x,0.5,3,1)

## Find the posterior density for mu, given a random sample of 4
## observations from N(mu,sigma^2=1), y = [2.99, 5.56, 2.83, 3.47],
## and a N(3,sd=2)$ prior for mu
y = c(2.99,5.56,2.83,3.47)
normnp(y,3,2,1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normnp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nvaricp")
### * nvaricp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nvaricp
### Title: Bayesian inference for a normal standard deviation with a scaled
###   inverse chi-squared distribution
### Aliases: nvaricp
### Keywords: misc

### ** Examples


## Suppose we have five observations from a normal(mu, sigma^2)
## distribution mu = 200 which are 206.4, 197.4, 212.7, 208.5.
y = c(206.4, 197.4, 212.7, 208.5, 203.4)

## We wish to choose a prior that has a median of 8. This happens when
## S0 = 29.11 and kappa = 1
nvaricp(y,200,29.11,1)

##  Same as the previous example but a calculate a 95% credible
## interval for sigma. NOTE this method has changed
results = nvaricp(y,200,29.11,1)
quantile(results, probs = c(0.025, 0.975))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nvaricp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.Bolstad")
### * plot.Bolstad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.Bolstad
### Title: Plot method for objects of type Bolstad
### Aliases: plot.Bolstad
### Keywords: plot

### ** Examples


x = rnorm(20,-0.5,1)
## find the posterior density with a N(0,1) prior on mu
b = normnp(x,sigma=1)
plot(b)
plot(b, which = 1:3)
plot(b, overlay = FALSE, which = 1:3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.Bolstad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("poisdp")
### * poisdp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: poisdp
### Title: Poisson sampling with a discrete prior
### Aliases: poisdp
### Keywords: misc

### ** Examples


## simplest call with an observation of 4 and a uniform prior on the
## values mu = 1,2,3
poisdp(4,1:3,c(1,1,1)/3)

##  Same as the previous example but a non-uniform discrete prior
mu = 1:3
mu.prior = c(0.3,0.4,0.3)
poisdp(4,mu=mu,mu.prior=mu.prior)

##  Same as the previous example but a non-uniform discrete prior
mu = seq(0.5,9.5,by=0.05)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
poisdp(4,mu=mu,mu.prior=mu.prior)

## A random sample of 50 observations from a Poisson distribution with
## parameter mu = 3 and  non-uniform prior
y.obs = rpois(50,3)
mu = c(1:5)
mu.prior = c(0.1,0.1,0.05,0.25,0.5)
results = poisdp(y.obs, mu, mu.prior)

##  Same as the previous example but a non-uniform discrete prior
mu = seq(0.5,5.5,by=0.05)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
y.obs = rpois(50,3)
poisdp(y.obs,mu=mu,mu.prior=mu.prior)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("poisdp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("poisgamp")
### * poisgamp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: poisgamp
### Title: Poisson sampling with a gamma prior
### Aliases: poisgamp
### Keywords: misc

### ** Examples


## simplest call with an observation of 4 and a gamma(1, 1), i.e. an exponential prior on the
## mu
poisgamp(4, 1, 1)

##  Same as the previous example but a gamma(10, ) prior
poisgamp(4, 10, 1)

##  Same as the previous example but an improper gamma(1, ) prior
poisgamp(4, 1, 0)

## A random sample of 50 observations from a Poisson distribution with
## parameter mu = 3 and  gamma(6,3) prior
set.seed(123)
y = rpois(50,3)
poisgamp(y,6,3)

## In this example we have a random sample from a Poisson distribution
## with an unknown mean. We will use a gamma(6,3) prior to obtain the
## posterior gamma distribution, and use the R function qgamma to get a
## 95% credible interval for mu
y = c(3,4,4,3,3,4,2,3,1,7)
results = poisgamp(y,6,3)
ci = qgamma(c(0.025,0.975),results$shape, results$rate)
cat(paste("95% credible interval for mu: [",round(ci[1],3), ",", round(ci[2],3)),"]\n")

## In this example we have a random sample from a Poisson distribution
## with an unknown mean. We will use a gamma(6,3) prior to obtain the
## posterior gamma distribution, and use the R function qgamma to get a
## 95% credible interval for mu
y = c(3,4,4,3,3,4,2,3,1,7)
results = poisgamp(y, 6, 3)
ci = quantile(results, c(0.025, 0.975))
cat(paste("95% credible interval for mu: [",round(ci[1],3), ",", round(ci[2],3)),"]\n")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("poisgamp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("poisgcp")
### * poisgcp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: poisgcp
### Title: Poisson sampling with a general continuous prior
### Aliases: poisgcp
### Keywords: misc

### ** Examples


## Our data is random sample is 3, 4, 3, 0, 1. We will try a normal
## prior with a mean of 2 and a standard deviation of 0.5.
y = c(3,4,3,0,1)
poisgcp(y, density = "normal", params = c(2,0.5))

## The same data as above, but with a gamma(6,8) prior
y = c(3,4,3,0,1)
poisgcp(y, density = "gamma", params = c(6,8))

## The same data as above, but a user specified continuous prior.
## We will use print.sum.stat to get a 99% credible interval for mu.
y = c(3,4,3,0,1)
mu = seq(0,8,by=0.001)
mu.prior = c(seq(0,2,by=0.001),rep(2,1999),seq(2,0,by=-0.0005))/10
poisgcp(y,"user",mu=mu,mu.prior=mu.prior,print.sum.stat=TRUE,alpha=0.01)

## find the posterior CDF using the results from the previous example
## and Simpson's rule. Note that the syntax of sintegral has changed.
results = poisgcp(y,"user",mu=mu,mu.prior=mu.prior)
cdf = sintegral(mu,results$posterior,n.pts=length(mu))$cdf
plot(cdf,type="l",xlab=expression(mu[0])
	,ylab=expression(Pr(mu<=mu[0])))

## use the cdf to find the 95% credible region.
lcb = cdf$x[with(cdf,which.max(x[y<=0.025]))]
ucb = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95% credible interval : ["
	,round(lcb,4)," ",round(ucb,4),"]\n",sep=""))

## find the posterior mean, variance and std. deviation
## using Simpson's rule and the output from the previous example
dens = mu*results$posterior # calculate mu*f(mu | x, n)
post.mean = sintegral(mu,dens)$value

dens = (mu-post.mean)^2*results$posterior
post.var = sintegral(mu,dens)$value
post.sd = sqrt(post.var)

# calculate an approximate 95% credible region using the posterior mean and
# std. deviation
lb = post.mean-qnorm(0.975)*post.sd
ub = post.mean+qnorm(0.975)*post.sd

cat(paste("Approximate 95% credible interval : ["
	,round(lb,4)," ",round(ub,4),"]\n",sep=""))

# NOTE: All the examples given above can now be done trivially in this package

## find the posterior CDF using the results from the previous example
results = poisgcp(y,"user",mu=mu,mu.prior=mu.prior)
cdf = cdf(results)
curve(cdf,type="l",xlab=expression(mu[0])
	,ylab=expression(Pr(mu<=mu[0])))

## use the quantile function to find the 95% credible region.
ci = quantile(results, c(0.025, 0.975))
cat(paste0("Approximate 95% credible interval : ["
	,round(ci[1],4)," ",round(ci[2],4),"]\n"))

## find the posterior mean, variance and std. deviation
## using the output from the previous example
post.mean = mean(results)

post.var = var(results)
post.sd = sd(results)

# calculate an approximate 95% credible region using the posterior mean and
# std. deviation
ci = post.mean + c(-1, 1) * qnorm(0.975) * post.sd

cat(paste("Approximate 95% credible interval : ["
	,round(ci[1],4)," ",round(ci[2],4),"]\n",sep=""))

## Example 10.1 Dianna's prior
# Firstly we need to write a function that replicates Diana's prior
f = function(mu){
   result = rep(0, length(mu))
   result[mu >=0 & mu <=2] = mu[mu >=0 & mu <=2]
   result[mu >=2 & mu <=4] = 2
   result[mu >=4 & mu <=8] = 4 - 0.5 * mu[mu >=4 & mu <=8]
   
   ## we don't need to scale so the prior integrates to one, 
   ## but it makes the results nicer to see
   
   A = 2 + 4 + 4
   result = result / A
   
   return(result)
 }
 
 results = poisgcp(y, mu = c(0, 10), mu.prior = f)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("poisgcp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sd.Bolstad")
### * sd.Bolstad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sd.Bolstad
### Title: Posterior standard deviation
### Aliases: sd.Bolstad

### ** Examples

## The usefulness of this method is really highlighted when we have a general 
## continuous prior. In this example we are interested in the posterior
## standard deviation of an normal mean. Our prior is triangular over [-3, 3]
set.seed(123)
x = rnorm(20, -0.5, 1)

mu = seq(-3, 3, by = 0.001)

mu.prior = rep(0, length(mu))
mu.prior[mu <= 0] = 1 / 3 + mu[mu <= 0] / 9
mu.prior[mu > 0] = 1 / 3 - mu[mu > 0] / 9

results = normgcp(x, 1, density = "user", mu = mu, mu.prior = mu.prior, plot = FALSE)
sd(results)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sd.Bolstad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sintegral")
### * sintegral

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sintegral
### Title: Numerical integration using Simpson's Rule
### Aliases: sintegral
### Keywords: misc

### ** Examples


## integrate the normal density from -3 to 3
x = seq(-3, 3, length = 100)
fx = dnorm(x)
estimate = sintegral(x,fx)$value
true.val = diff(pnorm(c(-3,3)))
abs.error = abs(estimate-true.val)
rel.pct.error =  100*abs(estimate-true.val)/true.val
cat(paste("Absolute error :",round(abs.error,7),"\n"))
cat(paste("Relative percentage error :",round(rel.pct.error,6),"percent\n"))

## repeat the example above using dnorm as function
x = seq(-3, 3, length = 100)
estimate = sintegral(x,dnorm)$value
true.val = diff(pnorm(c(-3,3)))
abs.error = abs(estimate-true.val)
rel.pct.error =  100*abs(estimate-true.val)/true.val
cat(paste("Absolute error :",round(abs.error,7),"\n"))
cat(paste("Relative percentage error :",round(rel.pct.error,6)," percent\n"))

## use the cdf

cdf = sintegral(x,dnorm)$cdf
plot(cdf, type = 'l', col = "black")
lines(x, pnorm(x), col = "red", lty = 2)

## integrate the function x^2-1 over the range 1-2
x = seq(1,2,length = 100)
sintegral(x,function(x){x^2-1})$value

## compare to integrate
integrate(function(x){x^2-1},1,2)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sintegral", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("slug")
### * slug

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: slug
### Title: Slug data
### Aliases: slug
### Keywords: datasets

### ** Examples


data(slug)
plot(weight~length, data = slug)
plot(log.wt~log.len, data = slug)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("slug", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sscsample")
### * sscsample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sscsample
### Title: Simple, Stratified and Cluster Sampling
### Aliases: sscsample
### Keywords: misc

### ** Examples


## Draw 200 samples of size 20 using simple random sampling
sscsample(20,200)

## Draw 200 samples of size 20 using simple random sampling and store the
## results. Extract the means of all 200 samples, and the 50th sample
res = sscsample(20,200)
res$means
res$samples[,50]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sscsample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sscsample.data")
### * sscsample.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sscsample.data
### Title: Data for simple random sampling, stratified sampling, and
###   clusting sampling experiments
### Aliases: sscsample.data
### Keywords: datasets

### ** Examples


data(sscsample.data)
plot(income~ethnicity, data = sscsample.data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sscsample.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xdesign")
### * xdesign

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xdesign
### Title: Monte Carlo study of randomized and blocked designs
### Aliases: xdesign
### Keywords: misc

### ** Examples


# Carry out simulations using the default parameters 

xdesign()

# Carry out simulations using a simulated response with 5 treaments, 
# groups of size 25, and a correlation of -0.6 between the response 
# and lurking variable

xdesign(corr = -0.6, size = 25, n.treatments = 5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xdesign", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
