# This script can be run to test the cache matrix implementation
# performance compared to a regular matrix for repeated inversions.
# It first creates a random 1000x1000 matrix and then calculates
# the average time to invert it 10 times for both a normal
# and cache matrix. It then prints the average elapsed time
# (per inversion) to the console.

source("cachematrix.R")

originalMatrix <- replicate(1000, rnorm(1000, 0, 4))

cacheMatrix <- makeCacheMatrix(originalMatrix)

numIterations <- 10

message(sprintf("Calculating average time taken to invert original matrix %d times", numIterations))

printAverageTime <- function(times) {
  message(sprintf("Average inverse time is %f seconds", mean(times)))
}

times <- vector(mode = "numeric", length = numIterations)
for (i in 1:numIterations) {
  times[i] <- system.time(solve(originalMatrix))[["elapsed"]]
}

printAverageTime(times)

message(sprintf("Calculating average time taken to invert cached matrix %d times", numIterations))

for (i in 1:numIterations) {
  times[i] <- system.time(cacheSolve(cacheMatrix))[["elapsed"]]
}

printAverageTime(times)