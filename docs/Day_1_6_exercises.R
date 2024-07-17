### Bayesian intro course - Day 1_6 exercises ###

rm(list = ls())

library(rstan)
library(coda)

rstan_options(auto_write = TRUE)
parallel::detectCores(all.test = FALSE, logical = TRUE)
options(mc.cores = 4)


## quadratic model ##

set.seed(431)

n = 111

b1 = 1
b2 = 2.4
b3 = 1.8
sigma = 3

x = runif(n, min = 0, max = 5)
y = rnorm(n, mean = b1 + b2 * x + b3 * x^2, sd = sigma)

df = data.frame(x = x, y = y)

plot(df)

data_list = list(n = n, x = df$x, y = df$y)

stan_code = '
data {
  int n;
  vector[n] x;
  vector[n] y;
}
parameters {
  vector[3] b;
  real<lower = 0> sigma;
}
model {
  // priors
  b ~ normal(0, 10);
  sigma ~ normal(0, 10);
  
  // likelihood
  for(i in 0:(n - 1)) {
    y[i] ~ normal(b[1] + b[2] * x[i] + b[3] * x[i]^2, sigma);
  }
}'

stan_model_quad = stan_model(model_code = stan_code)

fit = sampling(stan_model_quad,
               data = data_list,
               iter = 2000,
               warmup = 1000,
               chains = 4)






