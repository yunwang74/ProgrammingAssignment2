## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
###   that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        ### set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  ### get the value of the matrix
        setsolve <- function(solve) m <<- solve ### set the value of the inverse of the matrix
        getsolve <- function() m ###get the value of the inverse of the matrix
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # return list
}


## cacheSolve: This function computes the inverse of the special "matrix"
###   returned by makeCacheMatrix above. If the inverse has already been 
###   calculated (and the matrix has not changed), then cacheSolve should 
###   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
        m <- x$getsolve()  ### retrieve from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() ### get a matrix
        m <- solve(data, ...) ### inverse the matrix
        x$setsolve(m) ### set the inversed of the matrix
        m ### return the result
        ## Return a matrix that is the inverse of 'x'
}
