
dtetris <- function(x, algo = "uniform", log = TRUE){
  tetrominoes <- c("I", "J", "L", "O", "S", "T", "Z")
  if (!all(x %in% tetrominoes) | length(x) == 0) {
    stop("x must be a character vector of only the seven tetrominoes (I, J, L, O, S, T, & Z)")
  }
  if (!algo %in% c("uniform", "nes1989", "gb1989", "7-bag")){
    stop("algo must be set to one of the valid algorithms: 'uniform', 'nes1989', 'gb1989', '7-bag'")
  }
  if (algo == "uniform") {
    n <- length(x)
    out <- rep(log(1/7), n)
  }
  if (algo == "nes1989") {
    log_liks <- numeric(0)
    if (length(x) > 1) {
      preview_piece <- x[1:(length(x) - 1)]
      next_piece <- x[2:length(x)]
      log_liks <- sapply(1:length(next_piece), {
        function(z) pr_nes1989(x = next_piece[z], state = preview_piece[z], log = TRUE)
      })
    }
    out <- c(log(1/7), log_liks)
  }
  if (algo == "gb1989") {
    log_liks <- 0
    if (length(x) > 2) {
      falling_piece <- x[1:(length(x) - 2)]
      preview_piece <- x[2:(length(x) - 1)]
      states <- paste0(falling_piece, preview_piece)
      next_piece <- x[3:length(x)]
      log_liks <- sapply(1:length(next_piece), {
        function(z) pr_gb1989(x = next_piece[z], state = states[z], log = TRUE)
      })
    }
    out <- c(log(1/7), log(1/7), log_liks)
  }
  if (algo == "7-bag") {
    n_full_bags <- floor(length(x) / 7)
    n_last_bag <- length(x) %% 7
    x_full <- x[1:(7 * n_full_bags)]
    log_lik_full <- numeric(0)
    if (n_full_bags > 0) {
      log_lik_full <- dmultinom(rep(n_full_bags, 7), length(x_full), rep(1/7, 7), log = TRUE)
    }
    log_lik_left <- numeric(0)
    if (n_last_bag > 0) {
      log_lik_left <- rep(log(1/7), n_last_bag)
    }
    out <- c(log_lik_full, log_lik_left)
    full_bags_balanced <- all(table(x_full) == n_full_bags)
    if (full_bags_balanced){
      warning("7-bag log-likelihoods are necessarily aggregated")
    }
    if (!full_bags_balanced){
      out <- log(0)
      warning("this data could not have come from a 7-bag; probability is 0!")
    }
  }
  if (log == FALSE) out <- exp(out)
  return(out)
}

