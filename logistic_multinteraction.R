rm(list=ls())

library(boot)
library(ggplot2)
library(dplyr)
library(tidyr)

#set.seed(4)

## This is the function that takes prey size (x) and
## calculates the probability of a prey individual of that size
## being eaten
## a determines the strength of the size effect
## b shifts the size at which the inflexion occurs
logistic <- function(x, a, b){
  yy <- inv.logit(a*log10(x)+b)
  return(yy)
}

## parameters
mass_mean <- 1
mass_sd <- 1
num_species <- 50
real_a <- 10 # ~Real Parameters
real_b <- -5 # ~Real Parameters
num_interations_observed <- 200 ## total number of potential interactions

## create species and their body sizes
x <- rlnorm(num_species, mass_mean, mass_sd) # ~Real Body Sizes
real_b <- real_b - mean(log10(x))
## make a series for plotting the curve
x_seq <- seq(min(x), max(x), length=1000)

## observation process (not specified whether this is us observing the presence / absence of an interaction,
## or if it is the predator "observing" (i.e. encountering) its prey)
## get a number of observations of each prey
#obs_x <- sample(x, num_interations_observed, replace=TRUE, p=x/sum(x))
obs_x <- sample(x, num_interations_observed, replace=TRUE)


# ~Stochasticity in the system's biology
# determine whether an interaction occured or not
real_int <- numeric(length=num_interations_observed)
for(i in 1:num_interations_observed)
  {
  real_int[i] <- rbinom(1, 1, prob=logistic(obs_x[i], real_a, real_b))
  }
#real_data <- data.frame(obs_x, real_int, p=logistic(obs_x[i], real_a, real_b))  


# plot probability curve and the species on it
# ggplot(mapping=aes(x=log10(x_seq), y=logistic(x_seq, real_a, real_b))) +
#   geom_line() + 
#   geom_point(aes(x=log10(x), y=logistic(x, real_a, real_b))) +
#   geom_jitter(aes(x=log10(obs_x), y=real_int), col="red")


## add observation error to give if we observed and interaction or not
## p of observing x given y (p_x_y)
p_0_1 <- 0.0  ## larger values give greater observation error
p_1_0 <- 0.0  ## larger values give greater observation error
rans_temp <- runif(num_interations_observed)

obs_interaction <- real_int
obs_interaction <- ifelse(real_int==1 & rans_temp<p_0_1, obs_interaction-1,
                          ifelse(real_int==0 & rans_temp<p_1_0, obs_interaction+1, obs_interaction))

ggplot(mapping=aes(x=log10(x_seq), y=logistic(x_seq, real_a, real_b))) +
  geom_line() + 
  geom_point(aes(x=log10(x), y=logistic(x, real_a, real_b))) +
  geom_jitter(aes(x=log10(obs_x), y=real_int), col="red", height=0.1) +
  geom_jitter(aes(x=log10(obs_x), y=obs_interaction), col="green", height=0.1)



#Parameter Estimation (ABC)
numm <- 10000
guesses <- data.frame(a=runif(numm,0,1000),
                      b=runif(numm,-10,10),
                      Q = rep(NA,numm))

for(i in 1:numm)
{
  guesses$Q[i] <-  sum(1-dbinom(obs_interaction, 1, logistic(obs_x, guesses$a[i], guesses$b[i])))
}

sum(1-dbinom(obs_interaction, 1, logistic(obs_x, real_a, real_b)))

ggplot(gather(guesses), aes(x=value)) +
  facet_wrap(~key, scales="free_x") + 
  geom_histogram()

## find threshol
proportion_to_keep <- 0.01
temp <- arrange(guesses, Q) %>%
  slice(numm*proportion_to_keep)
threshold <- temp$Q

guesses %>%
  filter(Q<=threshold) %>%
  gather() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~key, scales="free_x") +
  geom_histogram()

means <- guesses %>%
  filter(Q<=threshold) %>%
  summarise_all(mean)

ggplot(mapping=aes(x=log10(x_seq), y=logistic(x_seq, real_a, real_b))) +
  geom_line() + 
  geom_point(aes(x=log10(x), y=logistic(x, real_a, real_b))) +
  geom_jitter(aes(x=log10(obs_x), y=real_int), col="red", height=0.1) +
  geom_jitter(aes(x=log10(obs_x), y=obs_interaction), col="green", height=0.1) +
  geom_line(aes(x=log10(x_seq), y=logistic(x_seq, means$a, means$b)), col="red", linetype="dashed")


