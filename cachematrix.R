## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). 
## Here is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## inverse stores the inverse of matrix x
    inverse <- NULL
    
    ## update inverse to null after a new matrix is assigned to x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## return the original matrix x
    get <- function() x
    
    ## update the inverse value to the new_inverse
    setinverse <- function(new_inverse) inverse <<- new_inverse
    
    ## return the inverse value
    getinverse <- function() inverse
    
    ## return functions in a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## The input of this function is the return value of function "makeCacheMatrix".

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    ## if inverse is not null, use that value directly
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## if inverse in null, calculate inverse and update the value in cache
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    inverse
}
