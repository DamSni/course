###Beta

# Observations
y_obs <- c()
prior_shape1 <- 
prior_shape2 <- 
  
# Get the posterior parameters
post_shape1 <- prior_shape1 + sum(y_obs)
post_shape2 <- prior_shape2 + length(y_obs) - sum(y_obs)  


# Mean, Median, Mode Posterior
post_mean <-post_shape1 / (post_shape1 + post_shape2) ## a/(a+b)
post_median <- qbeta(0.5, post_shape1, post_shape2)
post_mode <- (post_shape1-1)/(post_shape1+post_shape2-2) ## ((a-1)/(a+b-2))


# classical confidence interval
alpha <-  # insert confidence level (significance)
et.inetrval <- qbeta(c(1-(1-alpha), (1-alpha)), post_shape1, post_shape2) # equal tailed interval


# Credible Interval
library(TeachingDemos)
hpd(qbeta, shape1=post_shape1, shape2=post_shape2, conf=) # Adjust the right conf interval


# Posterior probability and odds that parameter is greater than theta.h
theta.h <- # Set theta.h
  
# Posterior Prob
post_prob.h <- 1-pbeta(theta.h, post_shape1, post_shape2)
post_odds.h <- post_prob.h / (1-post_prob.h)

# Prior Prob
prior_prob.h <- 1-pbeta(theta.h, prior_shape1, prior_shape2)
prior_odds.h <- prior_prob.h / (1-prior_prob.h)


# Bayes Factor
bf <- post_odds.h / prior_odds.h
bf

# predictive posterior (predict new y) - optional
library(VGAM)
n <-  # number of trials for the new beta-binom distribution 
y.new <- rbetabinom.ab(1, n, post_shape1, post_shape2)
----------------------------------------------------------------
###Gamma

# Observations
y_obs <- c()

# Prior parameters
prior_shape <-
prior_rate <-
  
# Posterior parameters
post_shape <- prior_shape+sum(y_obs)
post_rate<- prior_rate+length(y_obs)


# Mean, Median, Mode Posterior
post_mean <- post_shape/post_rate
post_median <- qgamma(0.5,shape=post_shape,rate=post_rate)
post_mode <- (post_shape-1)/post_rate


# classical confidence interval
alpha <-  # insert confidence level (significance)
et.inetrval <- qgamma(c(1-(1-alpha), (1-alpha)), post_shape1, post_shape2) # equal tailed interval


# Credible Interval
library(TeachingDemos)
hpd(qgamma, shape=post_shape, rate=post_rate, conf=) # Adjust the right conf interval


# Posterior probability and odds that parameter is greater than theta.h
theta.h <- # Set theta.h
  
# Posterior Prob
post_prob.h <- 1-pgamma(theta.h, shape=post_shape, rate=post_rate)
post_odds.h <- post_prob.h / (1-post_prob.h)

# Prior Prob
prior_prob.h <- 1-pgamma(theta.h, shape=prior_shape, rate=prior_rate)
prior_odds.h <- prior_prob.h / (1-prior_prob.h)


# Bayes Factor
bf <- post_odds.h / prior_odds.h
bf

# predictive posterior (predict new y) - optional
y.new <- rgamma(1, size=post_shape, nu=post_mean)

----------------------------------------------------------------------------------------------------------------------
###Normal

#plot
x<- seq(0,100, length.out= 1000)
y<- dnorm(x, mean, sd)
plot(x,y)

##Normal with Known Variance

data<- c()
sigma2<- 

#priors param
prior_mu <- 
prior_sigma2 <- 
  
#post_param
post_mu<- (prior_mu/prior_sigma2 + sum(data)/sigma2)/(1/prior_sigma2 + length(data)/sigma2)
post_sigma2<- 1/(1/prior_sigma2 + length(data)/sigma2)

# post_mode
post_mode <- post_mu
post_mu

#post_median
post_median<- qnorm(0.5, post_mu, sqrt(post_sigma2))
post_median

#classical confidence interval
alpha <-  # insert confidence level (significance)
inetrval <- qnorm(c(1-(1-alpha), (1-alpha)), post_mu, post_sigma2) # equal tailed interval

#hpd
library(TeachingDemos)
hpd <- hpd(qnorm, post_mu, sqrt(post_sigma2), conf= , tol= 0.00000001)
hpd

#post prob
theta<-
post_prob<- pnorm(theta, post_mu, sqrt(post_sigma2))
post_prob

#post odd
post_odd<- post_prob/ (1 - post_prob)
post_odd

#prior prob
prior_prob<- pnorm(theta, prior_mu, sqrt(prior_sigma2))
prior_prob

#prior odd
prior_odd <- prior_prob / (1 - prior_prob)
prior_odd

#BF
bf<- post_odd / prior_odd
bf

# predictive posterior (predict new y) - optional
y.new <- rnorm(1, post_mu, post_sigma2 + sigma2)
  
------------------------------------------------------------
### Normal with both unknown param

y <- c()         # insert observed data
n <- length(y)   # sample size
y.bar <- mean(y) # sample mean
s2 <- var(y)     # sample variance

# insert parameters for the prior theta (normal)
mu.0 <-  # prior mean
k.0 <-   # prior sample size
  
# insert parameters for the prior variance sigma (gamma not reparametrized with shape and rate )
s2.0 <-  # prior sample variance
nu.0 <-  # prior sample size
  
# posterior parameters for theta
k.n <- k.0 + n
mu.n <- (k.0*mu.0 + n*y.bar)/k.n

# posterior parameters for sigma
nu.n <- nu.0 + n 
s2.n <- ((nu.0*s2.0 + (n-1)*s2) + (k.0*n/k.n)*(y.bar - mu.0)^2)/nu.n
  
--------------------------------------------------------------------------------------------------------------------
### without distribution
prob_post<- c()
theta <- 
  
#Normalize
prob_post_norm<- prob_post / sum(prob_post)
prob_post_norm

# mean
mean_post<- sum(theta * prob_post_norm)
mean_post

#median
cum_sum <- cumsum(prob_post_norm)
median_index <- theta[which.min(abs(cum_sum - 0.5))]
median_index

#mode
mode_index<- theta[which.max(prob_post_norm)]
mode_index

#credible set/hpd
lower_bound <- theta[which.min(abs(cum_sum - 0.1))]
lower_bound

upper_bound <- theta[which.min(abs(cum_sum-0.9))]
upper_bound

# prior prob
prob_prior <- c()
theta <- 
prob_prior_sum <- sum(prob_prior)
prob_prior_norm <- prob_prior / prob_prior_sum

mean_prior <- sum(theta * prob_prior_norm)

#var
var <- sum((theta - mean_prior)^2 * prob_prior_norm)
var

#sd
sd <- sqrt(var)
sd

#MLE
mle <- theta[which.max(prob_prior_norm)]
mle
-----------------------------------------------------------------------------------------------------





