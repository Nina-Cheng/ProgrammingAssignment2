## Below are two functions that are used to create a special object that stores an invertible matrix
## and caches its inverse to avoid repeated computation.

## MakeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## It will 1) set the value of the invertible matrix
##         2) get the value of the invertible matrix
##         3) set the value of the inverse of the invertible matrix
##         4) get the value of the inverse of the invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
        x <<- y
        s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache and skip the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
        message("getting cached data")
        return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
