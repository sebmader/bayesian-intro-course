### Bayesian intro course - Day 1_6 exercises ###

rm(list = ls())

library(rstan)
library(coda)

rstan_options(auto_write = TRUE)
parallel::detectCores(all.test = FALSE, logical = TRUE)
options(mc.cores = 3)


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
  for(i in 1:n) {
    y[i] ~ normal(b[1] + b[2] * x[i] + b[3] * x[i]^2, sigma);
  }
}
'

stan_model_quad = stan_model(model_code = stan_code)

fit = sampling(stan_model_quad,
               data = data_list,
               iter = 2000,
               warmup = 1000,
               chains = 3
)

print(fit,
      digits = 3,
      probs = c(0.05, 0.95),
      pars = c("b", "sigma")
)
#         mean se_mean    sd     5%   95% n_eff  Rhat
# b[1]  -0.167   0.025 0.746 -1.380 1.051   897 1.004
# b[2]   3.451   0.026 0.725  2.250 4.656   779 1.003
# b[3]   1.654   0.005 0.148  1.409 1.896   882 1.002
# sigma  3.007   0.006 0.214  2.673 3.388  1484 1.001
# -> interestingly messed up values...
### -> ask Benjamin?

posterior = As.mcmc.list(fit)

plot(posterior)
stan_trace(fit)

pairs(fit, pars = c("b", "sigma"))
# how to interpret this figure? E.g. "if b1 is estimated high, b2 is estimated low" and "if b3 is estimated high, b1 is estimated high, too"?

library(BayesianTools)
correlationPlot(as.matrix(fit)[,1:4], thin = 1)
