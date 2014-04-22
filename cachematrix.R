## Put comments here that give an overall description of what your
## functions do
## 
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Once the special "matrix" has been created, cacheSolve can be used to compute
## its inverse ('of the special "matrix"') returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the special "matrix" has not
## changed), then the cacheSolve should retrieve the inverse from the cache.
## Example:
## >
## > # Create a matrix A
## > A <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
## > # Create the special matrix As
## > As <- makeCacheMatrix(A)
## > # Calculate the inverse of As for 1st time
## > invAs <- cacheSolve(As)
## > # Recalculate the inverse of As for 2nd time to get its cached value
## > invAs <- cacheSolve(As)
## > Getting cached inverse matrix
## >
## 
## Write a short comment describing this function
## 
## makeCacheMatrix takes an argument X of type matrix and creates
## an special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
    m <- NULL
    set <- function(y) {
        X <<- y
        m <<- NULL
    }
    get <- function() X
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}
##
## Write a short comment describing this function
##
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'
    m <- X$getSolve()      # query the cached X inverse matrix 
    if(!is.null(m)) {      # if a cache exists
        message("Getting cached inverse matrix")
        return(m)          # just return the cache, no computation needed
    }
    data <- X$get()        # if no cache exists
    m <- solve(data, ...)  # then compute its inverse
    X$setSolve(m)          # save the result back to the cache
    m                      # return the result
}
