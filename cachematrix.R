## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object 
###   that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
###   returned by makeCacheMatrix above. If the inverse has already been 
###   calculated (and the matrix has not changed), then cacheSolve should 
###   retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function
###   in R. For example, if X is a square invertible matrix, then solve(X) returns
###   its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
