rm(list = ls())

library(rstan)
library(coda)

set.seed(1543)

# set settings for core usage by Stan
rstan_options(auto_write = TRUE)
parallel::detectCores(all.test = FALSE, logical = TRUE)
options(mc.cores = 3)

n = 145

a = 2.3
b = 1.5
sigma = 0.5

x = runif(n = n, min = 0, max = 3)
y = a + b * x + rnorm(n = n, mean = 0, sd = sigma)

df = data.frame(x = x, y = y)

plot(df)

data = list(n = n, x = df$x, y = df$y)

stan_code = '
data {
  // define structure of input data
  int n;
  vector[n] x;
  vector[n] y;
}
parameters{
  // define structure of output data & set their boundaries
  real a; // intercept
  real b; // slope
  real<lower=0> sigma; // residual sdev
}
model {
  vector[n] mu; // auxiliary variable: predictions
  // priors (weakly informative)
  a ~ normal(0,10); // mean=0, sdev=10
  b ~ normal(0,10);
  sigma ~ normal(0,5);
  
  // likelihood
  for(i in 1:n){
    mu[i] = a+b*x[i];
    y[i] ~ normal( mu[i], sigma );
  }
  // or short, in vector notation
  // mu = a+b*x;
  // y ~ normal(mu, sigma);
}
'

stan_model = stan_model( model_code=stan_code)

fit = sampling(stan_model, #
               data=data,
               iter=2000,
               warmup=1000,
               chains=3)

print(fit)
# print(fit,
#       digits=3,
#       probs=c(0.05, 0.95),
#       pars=c("a","b","sigma")
# )

plot(fit, pars=c("a", "b"))


posterior = As.mcmc.list(fit)

plot(posterior[ , 1:3])

# pairs(fit, pars=c("a","b","sigma"))

library(BayesianTools)
correlationPlot( as.matrix(fit)[, 1:3]   )

# # Other plotting options
# 
# stan_plot(fit)
# 
# stan_trace(fit)
# 
# stan_hist(fit)
# 
# stan_dens(fit)

# stan_scat(fit, pars = c("a", "b"))
