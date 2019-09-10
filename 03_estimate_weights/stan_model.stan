
data {
  int<lower=0> N;
  vector[N] GF;
  vector[N] audit1_2;
  vector[N] audit1_3;
  vector[N] audit1_4;
  vector[N] audit1_5;
  vector[N] audit1_6;
  vector[N] audit2_1;
  vector[N] audit2_2;
  vector[N] audit2_3;
  vector[N] audit2_4;
  vector[N] audit2_5;
  vector[N] audit2_6;
  vector[N] audit2_7;
  vector[N] audit3_1;
  vector[N] audit3_2;
  vector[N] audit3_3;
  vector[N] audit3_4;
  vector[N] audit3_5;


}

parameters {

  real<lower=0> sigma;

  real F2;
  real F3;
  real F4;
  real F5;
  real F6;
  real Q1;
  real Q2;
  real Q3;
  real Q4;
  real Q5;
  real Q6;
  real Q7;
  real V1;
  real V2;
  real V3;
  real V4;
  real V5;
  real binge;

}

transformed parameters{
  
  
  vector[N] mu;

  vector[N] F = (F2*audit1_2) + (F3*audit1_3) + (F4*audit1_4) + (F5*audit1_5) + (F6*audit1_6);
  vector[N] Q = (audit2_1 * Q1) + (audit2_2 * Q2) + (audit2_3 * Q3) + (audit2_4 * Q4) + (audit2_5 * Q5) + (audit2_6 * Q6) + (audit2_7 * Q7);
  vector[N] V = (audit3_1 * V1) + (audit3_2 * V2) + (audit3_3 * V3) + (audit3_4 * V4) + (audit3_5 * V5);
  
  
  for(i in 1:N){
    mu[i] = Q[i] >= binge ? ( F[i] >= V[i] ? Q[i] * F[i] : Q[i] * V[i]) : ( V[i] < F[i] ? (F[i] * Q[i]) + (V[i] * binge) : F[i] * binge);
  }

  
} 
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  F2 ~ uniform(0.1, 0.5);
  F3 ~ uniform(0.5, 1.5);
  F4 ~ uniform(1.5, 3.5);
  F5 ~ uniform(3.5, 5.5);
  F6 ~ uniform(5.5, 7);
  Q1 ~ uniform(1, 2.5);
  Q2 ~ uniform(2.5, 4.5);
  Q3 ~ uniform(4.5, 6.5);
  Q4 ~ uniform(6.5, 9.5);
  Q5 ~ uniform(9.5, 12);
  Q6 ~ uniform(12, 15.5);
  Q7 ~ uniform(15.5, 25);
  V1 ~ uniform(0, 0.1);
  V2 ~ uniform(0.1, 0.5);
  V3 ~ uniform(0.5, 1);
  V4 ~ uniform(1, 3);
  V5 ~ uniform(3, 7);


  binge ~ uniform (5, 10);
 
  sigma ~ exponential(0.1);
  GF ~ normal(mu, sigma);
}

