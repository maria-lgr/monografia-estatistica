// Bloco de declaração de dados.
data{
  int<lower=1> N;           // declara que N é um inteiro positivo
  int<lower=1> q;
  vector[N] y;              // declara que y é um vetor de tamanho N
  matrix[N,q] x;
  vector[q] m_beta;         // declara que m_beta é um vetor de tamanho q
  matrix[q,q] S_beta;
  vector[q] m_gamma;
  matrix[q,q] S_gamma;
  vector[N] m_delta_t;
  matrix[N,N] S_delta_t;
  vector[N] m_delta_z;
  matrix[N,N] S_delta_z;
  real<lower=0> a_tau_t;
  real<lower=0> b_tau_t;
  real<lower=0> a_tau_z;
  real<lower=0> b_tau_z;
}

// Bloco de declaração de parâmetros.
// Declare aqui todos os parâmetros para os quais
// uma distribuição a priori é especificada.
parameters{
  vector[q] beta;
  vector[q] gamma;
  vector[N] delta_theta;
  vector[N] delta_zeta;
  real<lower=0> tau_theta;
  real<lower=0> tau_zeta;
}

// Bloco de parâmetros transformados.
// Se necessário, declare aqui novos parâmetros
// construídos como função daqueles
// declarados no bloco anterior.
transformed parameters{
  vector[N] theta;
  vector[N] zeta;
  
  for (n in 1:N) {
    theta[n] = 1 / (1 + exp(-x[n,] * beta - delta_theta[n]));
  }
  
  for (n in 1:N) {
    zeta[n] = exp(x[n,] * gamma + delta_zeta[n]); 
  }
  
}

// Bloco do modelo.
// Defina aqui a verossimilhança e as distribuições a priori.
model{
  
  // Verossimilhança
  for(n in 1:N){ 
    y[n] ~ beta(theta[n]*zeta[n], (1-theta[n])*zeta[n]); 
  }
  
  // Priori 1
  beta ~ multi_normal(m_beta, S_beta);
  
  // Priori 2
  gamma ~ multi_normal(m_gamma, S_gamma);
  
  // Priori 3
  delta_theta ~ multi_normal(m_delta_t, S_delta_t * tau_theta);
  
  // Priori 4
  delta_zeta ~ multi_normal(m_delta_z, S_delta_z * tau_zeta);
  
  // Priori 5
  tau_theta ~ gamma(a_tau_t, b_tau_t);
  
  // Priori 6
  tau_zeta ~ gamma(a_tau_z, b_tau_z);
  
}
// Deixe vazia a última linha do arquivo ".stan" (isso evita "warnings").
