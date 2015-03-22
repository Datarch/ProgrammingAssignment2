## The pair of functions below that cache the inverse of a matrix.
## Put comments here that give an overall description of what your
## functions do

## This 'makeCacheMatrix' function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        env <- environment()
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        getenv <- function() environment()
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse,
             getenv = getenv)
}


## This 'cacheSolve' function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()           ## Get cached inverse matrix

        if(!is.null(inv)) {
                ## Checking the cached inverse matrix availability
                
                message("Getting cached data")
                return(inv)             ## Return the cached inverse matrix of 'x'
                
        } else {
                ## Create and cache the inverse and version of 'x'
                
                inv <- solve(x, ...)
                x$setinverse(inv)
                inv
        }
}
