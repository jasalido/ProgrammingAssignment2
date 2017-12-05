## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object
## that can cache its inverse.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y # Set the value
        inverse <<- NULL # Clear the cache
    }
    # Define function to get the value of the matrix
    get <- function() return(x)
    # Define function to set the inverse
    setinv <- function(inv) inverse <<- inv
    # Define function to get the inverse
    getinv <- function() return(inverse)
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # This fetches the cached value for the inverse
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    # The cache was empty. We need to calculate it, cache it, and then return it
    data <- x$get() # get matrix
    invserse <- solve(data) # calculate the inverse
    x$setinv(inverse)
    return(inverse) # return the inverse
}
