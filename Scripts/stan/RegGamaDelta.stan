// Bloco de declaração de dados.
data{
  int<lower=1> N;           // declara que N é um inteiro positivo
  int<lower=1> q;
  vector[N] y;              // declara que y é um vetor de tamanho N
  matrix[N,q] x;
  vector[q] m_beta;         // declara que m_beta é um vetor de tamanho q
  matrix[q,q] S_beta;
  real<lower=0> a_alpha;
  real<lower=0> b_alpha;
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
  real<lower=0> alpha;
  vector[N] delta;
  real<lower=0> tau;
}

// Bloco de parâmetros transformados.
// Se necessário, declare aqui novos parâmetros
// construídos como função daqueles
// declarados no bloco anterior.
transformed parameters{
  vector[N] theta;
  
  for (n in 1:N) {
    theta[n] = exp(x[n,] * beta + delta[n]);
  }
  
}

// Bloco do modelo.
// Defina aqui a verossimilhança e as distribuições a priori.
model{
  
  // Verossimilhança
  for(n in 1:N){ 
    y[n] ~ gamma(alpha, alpha/theta[n]); 
  }
  
  // Priori 1
  beta ~ multi_normal(m_beta, S_beta);
  
  // Priori 2
  alpha ~ gamma(a_alpha, b_alpha);
  
  // Priori 3
  delta ~ multi_normal(m_delta, S_delta * tau);
  
  // Priori 4
  tau ~ gamma(a_tau, b_tau);
}
// Deixe vazia a última linha do arquivo ".stan" (isso evita "warnings").

