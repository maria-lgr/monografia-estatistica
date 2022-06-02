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
  vector[N] m_delta;
  matrix[N,N] S_delta;
  real<lower=0> a_tau;
  real<lower=0> b_tau;
}

// Bloco de declaração de parâmetros.
// Declare aqui todos os parâmetros para os quais
// uma distribuição a priori é especificada.
parameters{
  vector[q] beta;
  vector[q] gamma;
  vector[N] delta;
  real<lower=0> tau;
}

// Bloco de parâmetros transformados.
// Se necessário, declare aqui novos parâmetros
// construídos como função daqueles
// declarados no bloco anterior.
transformed parameters{
  vector[N] theta;
  vector[N] zeta;
  
  for (n in 1:N) {
    theta[n] = 1 / (1 + exp(-x[n,] * beta - delta[n]));
  }
  
  for (n in 1:N) {
    zeta[n] = exp(x[n,] * gamma); 
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
  delta ~ multi_normal(m_delta, S_delta * tau);
  
  // Priori 4
  tau ~ gamma(a_tau, b_tau);
  
}
// Deixe vazia a última linha do arquivo ".stan" (isso evita "warnings").

