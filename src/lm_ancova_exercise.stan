data {
  int N;       // i=1:N observations
  int M;       // j=1:M levels
  vector[N] y;
  vector[N] area;
  array[N] int group;
}
parameters {
  vector[M] b;
  real a;
  real<lower=0> sigma;
}
model {
  for(j in 1:M){
    b[j] ~ normal(25, 10); 
  }
  // or short: b ~ normal(25,10);
  a ~ normal(30, 10);
  sigma ~ exponential(0.1);
  for(i in 1:N){
    y[i] ~ normal(b[group[i]] + a * area[i], sigma);
  }
  // or short: y ~ normal(b[group], sigma);
}
