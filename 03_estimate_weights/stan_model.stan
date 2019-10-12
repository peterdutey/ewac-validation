
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

  real F2hyper;
  real F3hyper;
  real F4hyper;
  real F5hyper;
  real F6hyper;
  real Q1hyper;
  real Q2hyper;
  real Q3hyper;
  real Q4hyper;
  real Q5hyper;
  real Q6hyper;
  real Q7hyper;
  real V1hyper;
  real V2hyper;
  real V3hyper;
  real V4hyper;
  real V5hyper;
  real bingehyper;

}

transformed parameters{
  
  real F2 =  ((F2hyper * 0.4) + 0.1);
  real F3 =  ((F3hyper * 1) + 0.5);
  real F4 =  ((F4hyper * 1.5) + 1.5);
  real F5 =  ((F5hyper * 2) + 3);
  real F6 =  ((F6hyper * 2) + 5);
  real Q1 =  ((Q1hyper * 1.5) + 1);
  real Q2 =  ((Q2hyper * 2) + 2.5);
  real Q3 =  ((Q3hyper * 2) + 4.5);
  real Q4 =  ((Q4hyper * 3) + 6.5);
  real Q5 =  ((Q5hyper * 2) + 9.5);
  real Q6 =  ((Q6hyper * 6) + 9.5);
  real Q7 =  ((Q7hyper * 9.5) + 15.5);
  real V1 =  ((V1hyper * 0.2) + 0);
  real V2 =  ((V2hyper * 0.3) + 0.2);
  real V3 =  ((V3hyper * 0.5) + 0.5);
  real V4 =  ((V4hyper * 2) + 1);
  real V5 =  ((V5hyper * 4) + 3);
  real binge = bingehyper + 5;

  vector[N] mu;

  vector[N] F = (F2*audit1_2) + (F3*audit1_3) + (F4*audit1_4) + (F5*audit1_5) + (F6*audit1_6);
  vector[N] Q = (audit2_1 * Q1) + (audit2_2 * Q2) + (audit2_3 * Q3) + (audit2_4 * Q4) + (audit2_5 * Q5) + (audit2_6 * Q6) + (audit2_7 * Q7);
  vector[N] V = (audit3_1 * V1) + (audit3_2 * V2) + (audit3_3 * V3) + (audit3_4 * V4) + (audit3_5 * V5);
  
  
    mu =  (F .* Q) + (V * binge);


  
} 
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  F2hyper ~ beta(2, 2);
  F3hyper ~ beta(2, 2);
  F4hyper ~ beta(2, 2);
  F5hyper ~ beta(2, 2);
  F6hyper ~ beta(2, 2);
  Q1hyper ~ beta(2, 2);
  Q2hyper ~ beta(2, 2);
  Q3hyper ~ beta(2, 2);
  Q4hyper ~ beta(2, 2);
  Q5hyper ~ beta(2, 2);
  Q6hyper ~ beta(2, 2);
  Q7hyper ~ beta(2, 2);
  V1hyper ~ beta(2, 2);
  V2hyper ~ beta(2, 2);
  V3hyper ~ beta(2, 2);
  V4hyper ~ beta(2, 2);
  V5hyper ~ beta(2, 2);
  
  bingehyper ~ gamma(4, 1.5);
 
  sigma ~ exponential(0.1);
  GF ~ normal(mu, sigma);
}

