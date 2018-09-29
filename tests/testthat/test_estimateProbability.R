
# it would be great if we just loaded those compiled binary models into the workspace when we load the package
# can we do that???

test_draws <- rtetris(1000)

tetrominoes <- c("O", "S", "T", "I", "J", "Z", "L")

test_draws <- sample(tetrominoes, 100, replace = TRUE)

m0 <- estimateProbability0(test_draws)
m1 <- estimateProbability1(test_draws)
m2 <- estimateProbability2(test_draws)

