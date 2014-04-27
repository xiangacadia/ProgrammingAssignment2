## This file is used to test cachematrix.R

source('cachematrix.R')

## Build an inversable matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8

## calculate the inverse of matrix h8
sh8 <- solve(h8)

## print the inverse of marix h8
sh8

## calculate the inverse with function "makeCacheMatrix()" and "cacheSolve()"
## if the result if the same as sh8, it indicates cachematrix.R works well
c <- makeCacheMatrix(h8)
cacheSolve(c)