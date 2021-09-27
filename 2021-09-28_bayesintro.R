# ------------------------------------------------------------------------------
#### R LADIES WORKSHOP (September 28, 2021) ####
# ------------------------------------------------------------------------------

dev.off()

#### Specify your prior distribution ####

# Change the shape parameters to find the prior distribution that represents
# your belief best. 
# Remember: theta = 0 = 0% dog people; theta = 1 = 100% dog people

shape1 <- 1
shape2 <- 1

curve(dbeta(x, shape1, shape2),
      bty="l", xlab = bquote(theta), ylab = "Density", cex.lab = 1.5)

#### The likelihood ####

n_observations <- 5
prop <- 0.5

plot(c(0:n_observations), 
     dbinom(c(0:n_observations), n_observations, prob=prop),
     bty="l", xlab = "Dog People", ylab = "Likelihood", type = "h")


#### The prior predictive distribution ####

ppred <- rep(NA, n_observations+1)

for(i in 0:n_observations){
  
  integrand <- function(theta){
    dbinom(i,n_observations,theta) * dbeta(theta,shape1,shape2)
  }
  
  ppred[i+1] <- integrate(integrand, lower = 0, upper = 1)$value
  
}

plot(c(0:n_observations), ppred,
     bty="l", xlab = "Dog People", ylab = "Probability p(X)", type = "h")


#### Observed data ####

dogpeople <- c(0,0,1,1,1)

#### Posterior distribution ####

# Marginal likelihood

integrand <- function(theta){
  dbinom(sum(dogpeople),length(dogpeople),theta) * dbeta(theta,shape1,shape2)
}

ML <- integrate(integrand, lower = 0, upper = 1)$value

# Posterior distribution

posterior <- function(theta){
  dbinom(sum(dogpeople),length(dogpeople),theta) * dbeta(theta,shape1,shape2) / ML
}

curve(posterior(x),
      bty="l", xlab = bquote(theta), ylab = "Density", cex.lab = 1.5)

# Plot prior and posterior for comparison

# Prior and posterior maximum 
# (this is only to scale the y-axis - no need to worry if you don't understand
# this bit)
prior_mode <- (shape1-1)/(shape1+shape2-2)
posterior_mode <- (shape1+sum(dogpeople)-1)/(shape1+shape2+length(dogpeople)-2)

prior_max <- ifelse(is.nan(prior_mode), 1, dbeta(prior_mode, shape1, shape2))
posterior_max <- dbeta(posterior_mode, 
                       shape1 + sum(dogpeople), 
                       shape2 + length(dogpeople) - sum(dogpeople))

# The plot
curve(dbeta(x, shape1, shape2),
      bty="l", xlab = bquote(theta), ylab = "Density", cex.lab = 1.5, 
      col="lightgrey", ylim = c(0, max(c(prior_max, posterior_max))))

curve(posterior(x),
      bty="l", xlab = bquote(theta), ylab = "Density", cex.lab = 1.5, 
      add = TRUE)

#### The credible interval ####

# Cumulative density function of the posterior
posterior_CDF <- function(theta){
  integrate(posterior, lower = 0, upper = theta)$value
}

# Which values of theta cut off 2.5% at either end of the posterior? These
# values mark the 95% central credible interval.

lower_bound <- optimize(function(x) (posterior_CDF(x)-0.025)^2,
                        lower = 0, upper = 1)$minimum
upper_bound <- optimize(function(x) (posterior_CDF(x)-0.975)^2,
                        lower = 0, upper = 1)$minimum

#### The Bayes factor ####

# Here, we compare the binomial model with the dispersed prior against a model
# where the binomial parameter is fixed to a constant. However, it would also
# be possible to compare two models with different dispersed priors or even
# non-nested models.

# Compute the marginal likelihoods (ML_1 was computed earlier)
ML_1 <- ML 
ML_2 <- dbinom(sum(dogpeople), length(dogpeople), prob = 0.5)

# Compute the Bayes factor
BF12 <- ML_1 / ML_2

#### Posterior model odds ####

# First, define your prior model odds
prior_prob_M1 <- 0.5
prior_prob_M2 <- 0.5
prior_model_odds <- prior_prob_M1 / prior_prob_M2

# Then, update with the Bayes factor
posterior_model_odds <- prior_model_odds * BF12



