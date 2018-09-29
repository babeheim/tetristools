
lookup_piece <- function(x){
  if(identical(x, c(0, 0, 0))) output <- "L"
  if(identical(x, c(0, 0, 1))) output <- "J"
  if(identical(x, c(0, 1, 0))) output <- "I"
  if(identical(x, c(0, 1, 1))) output <- "O"
  if(identical(x, c(1, 0, 0))) output <- "Z"
  if(identical(x, c(1, 0, 1))) output <- "S"
  if(identical(x, c(1, 1, 0))) output <- "T"
  return(output)
}

rtetris <- function(n, init = NA, algo = "gameboy") {
  if(algo != "gameboy"){
    tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")
    x <- sample(tetrominoes, n, replace = TRUE)
  }
  if(algo == "gameboy"){
    tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")
    combos <- paste(rep(tetrominoes, each = 7), rep(tetrominoes, times = 7), sep = "")
    possible <- list(c(0, 0, 0), c(0, 0, 1), c(0, 1, 0), c(0, 1, 1), c(1, 0, 0), c(1, 0, 1), c(1, 1, 0))
    names(possible) <- tetrominoes
    if(is.na(init)){
      locking <- possible[[sample(tetrominoes, 1)]]
      preview <- possible[[sample(tetrominoes, 1)]]
    } else {
      if(init %in% combos){
        locking <- possible[[substr(init, 1, 1)]]
        preview <- possible[[substr(init, 2, 2)]]
      } else {
        stop("init is invalid")
      }
    }
    candidate <- possible[[sample(tetrominoes, 1)]]
    x <- rep(NA, length(n))
    first.counter <- 0
    second.counter <- 0
    third.counter <- 0
    for(j in 1:n){
      if(any((locking | preview | candidate) != locking)){
        x[j] <- lookup_piece(candidate)
        first.counter <- first.counter + 1
      } else {
        candidate <- possible[[sample(tetrominoes, 1)]]
        if(any((locking | preview | candidate) != locking)){
          x[j] <- lookup_piece(candidate)
          second.counter <- second.counter + 1
        } else {
          candidate <- possible[[sample(tetrominoes, 1)]]
          x[j] <- lookup_piece(candidate)
          third.counter <- third.counter + 1
        }
      }
      locking <- preview
      preview <- candidate
      candidate <- possible[[sample(tetrominoes, 1)]]
      if(j %% 1000 == 0) print(j) 
    }
  }
  return(x)
}