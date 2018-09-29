



estimateProbability0 <- function(draws){

  tetrominoes <- c("O", "S", "T", "I", "J", "Z", "L")
  move_prior <- rep(1, 7)

  y <- match(draws, tetrominoes)

  dat_list <- list(
   y = y,
   K = length(unique(y)),
   N = length(y),
   alpha = move_prior
  )

  model0 <- "
    data {
      int<lower = 1> K; // number of outcome states
      int<lower = 1> N; // number of outcomes observed
      int<lower = 0> y[N]; // vector of outcomes
      vector<lower = 0>[K] alpha; // prior on states
    }

    parameters {
      simplex[K] theta; // outcome probabilities
    }

    model {
      theta ~ dirichlet(alpha);
      for (i in 1:N) y[i] ~ categorical(theta);
    }
  "

  m0 <- stan(model_code = model0, data = dat_list, chains = 1, iter = 1000, refresh = 100)

  return(m0)

}



# need an estimateProbability1 for first-order MC

estimateProbability1 <- function(draws){

  tetrominoes <- c("O", "S", "T", "I", "J", "Z", "L")
  n_states <- length(tetrominoes)

  n <- length(draws)
  z <- match(draws, tetrominoes)

  theta_prior <- list()
  theta_prior <- matrix(1, nrow = 7, ncol = 7)
  theta_prior <- t(apply(theta_prior, 1, simplex))

  dat_list <- list(
    K = n_states,
    T = n,
    z = z,
    theta_prior = theta_prior
  )

  model1 <- "
    data {
      int<lower = 1> K; // number of states
      int<lower = 0> T; // number of instances in chain
      int<lower = 1, upper = K> z[T]; // markovian data vector
      vector<lower = 0>[K] theta_prior[K]; // prior on transitions
    }

    parameters {
      simplex[K] theta[K]; // transition probabilities
    }

    model {
      for (k in 1:K){
    //    print(theta_prior[k]);
        theta[k] ~ dirichlet(theta_prior[k]);
      }
      for (t in 2:T) z[t] ~ categorical(theta[z[t - 1]]);
    }
  "

  m1 <- stan(model_code = model1, data = dat_list, chains = 1, iter = 1000, refresh = 100)

  return(m1)

}

simplex <- function(x) x/sum(x)

estimateProbability2 <- function(draws){

  tetrominoes <- c("O", "S", "T", "I", "J", "Z", "L")
  n_states <- length(tetrominoes)

  n <- length(draws)
  z <- match(draws, tetrominoes)
  theta_prior <- list()

  for(i in 1:n_states){
    transitions <- matrix(1, nrow = 7, ncol = 7)
    transitions <- t(apply(transitions, 1, simplex))
    theta_prior[[i]] <- transitions
  }

  dat_list <- list(
    K = n_states,
    T = n,
    z = z,
    theta_prior = theta_prior
  )

  model2 <- "
    data {
      int<lower = 1> K; // number of states
      int<lower = 0> T; // number of instances in chain
      int<lower = 1, upper = K> z[T]; // markovian data vector
      simplex[K] theta_prior[K, K]; // prior on transitions
    }

    parameters {
      simplex[K] theta[K, K]; // transition probabilities
    }

    model {
      for (k in 1:K){
        for(j in 1:K){
          theta[k][j] ~ dirichlet(theta_prior[k][j]);
        }
      }
      for (t in 3:T) z[t] ~ categorical(theta[z[t - 2]][z[t - 1]]);
    }
  "

  m2 <- stan(model_code = model2, data = dat_list, chains = 1, iter = 1000, refresh = 100)

  return(m2)

}









