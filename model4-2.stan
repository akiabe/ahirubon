data {
  int N;
  real X[N];
  real Y[N];
  int N_pred;
  real X_pred[N_pred];
}

parameters {
  real a;
  real b;
  real<lower=0> sigma;
}

transformed parameters {
  real mu[N];
  for (n in 1:N) {
    mu[n] = a + b*X[n];
  }
}

model {
  for (n in 1:N) {
    Y[n] ~ normal(mu[n], sigma);
  }
}

generated quantities {
  real mu_pred[N_pred];
  real Y_pred[N_pred];
  for (n in 1:N_pred) {
    mu_pred[n] = a + b*X_pred[n];
    Y_pred[n] = normal_rng(mu_pred[n], sigma);
  }
}

