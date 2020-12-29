
# this is overly simple, since it doesn't involve the additoin of the orientation hex values
rtetris_nes_step_simple <- function(last_piece){
  tetrominoes <- c("T", "J", "Z", "O", "S", "L", "I")
  candidate <- sample(tetrominoes, 1)
  if (candidate == last_piece) candidate <- sample(tetrominoes, 1)
  return(candidate)
}

rtetris_nes_step <- function(last_piece) {
  tetrominoes <- c("T", "J", "Z", "O", "S", "L", "I")
  orientationID <- c(2, 7, 8, 10, 11, 14, 18) # numeric address for each piece
  lastID <- orientationID[which(tetrominoes == last_piece)]
  index <- sample(0:7, 1)
  next_piece <- tetrominoes[index]
  if (index == 0 || next_piece == last_piece) {
    index <- sample(0:7, 1)
    index <- index + lastID # introduces small biases
    index <- index %% 7
    next_piece <- tetrominoes[index + 1]
  }
  return(next_piece)
}

gameboy_lookup_piece <- function(x){
  if(identical(x, c(0, 0, 0))) output <- "L"
  if(identical(x, c(0, 0, 1))) output <- "J"
  if(identical(x, c(0, 1, 0))) output <- "I"
  if(identical(x, c(0, 1, 1))) output <- "O"
  if(identical(x, c(1, 0, 0))) output <- "Z"
  if(identical(x, c(1, 0, 1))) output <- "S"
  if(identical(x, c(1, 1, 0))) output <- "T"
  return(output)
}

rtetris_gameboy_step <- function(preview_piece, falling_piece){
  tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")
  tetrominoeID <- list(c(0, 0, 0), c(0, 0, 1), c(0, 1, 0), c(0, 1, 1), c(1, 0, 0), c(1, 0, 1), c(1, 1, 0))
  previewID <- tetrominoeID[[which(tetrominoes == preview_piece)]]
  fallingID <- tetrominoeID[[which(tetrominoes == falling_piece)]]
  index <- sample(1:7, 1)
  candidateID <- tetrominoeID[[index]]
  if (all((fallingID | previewID | candidateID) == fallingID)) {
    index <- sample(1:7, 1)
    candidateID <- tetrominoeID[[index]]
    if (all((fallingID | previewID | candidateID) == fallingID)) {
      index <- sample(1:7, 1)
      candidateID <- tetrominoeID[[index]]
    }
  }
  output <- gameboy_lookup_piece(candidateID)
  return(output)
}

rtetris <- function(n, algo = "gameboy", verbose = FALSE) {
  if (!algo %in% c("nes", "gameboy", "modern", "uniform")) {
    stop("algorithm not specified: nes, gameboy, or modern")
  }
  tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")
  if (algo == "nes") {
    x <- rep(NA, (n + 1))
    x[1] <- sample(tetrominoes, 1)
    for (j in 2:(n + 1)) {
      x[j] <- rtetris_nes_step(last_piece = x[j - 1])
      if (verbose & j %% 1000 == 0) print(j)
    }
    x <- x[2:(n + 1)]
  }
  if (algo == "gameboy") {
    x <- rep(NA, (n + 2))
    x[1] <- sample(tetrominoes, 1)
    x[2] <- sample(tetrominoes, 1)
    for (j in 3:(n + 2)) {
      x[j] <- rtetris_gameboy_step(preview_piece = x[j - 1], falling_piece = x[j - 2])
      if (verbose & j %% 1000 == 0) print(j)
    }
    x <- x[3:(n + 2)]
  }
  if (algo == "modern") {
    n_sets <- ceiling(n / 7)
    tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")
    x <- as.vector(replicate(n_sets, sample(tetrominoes)))
    x <- x[1:n]
  }
  if (algo == "uniform") {
    x <- sample(tetrominoes, n, replace = TRUE)
  }
  return(x)
}
