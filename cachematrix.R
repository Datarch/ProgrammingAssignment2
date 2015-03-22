## The pair of below functions ('makeCacheMatrix' & 'cacheSolve') created 
## to cache the inverse of a matrix.

## This 'makeCacheMatrix' function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        env <- environment()
        
        set <- function(y) {
                ## this function set the input matrix into cache
                x <<- y
                inv <<- NULL
        }
        get <- function() x
                ## this function get the input matrix from cache
        
        setinverse <- function(solve) inv <<- solve     
                ## this function set the inverse matrix into cache
        
        getinverse <- function() inv
                ## this function get the inverse matrix from cache
        
        getenv <- function() environment()
                ## this function get the environment of the function
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse,
             getenv = getenv)
                ## this will be the result special "matrix" to be used to cache its inverse
}


## This 'cacheSolve' function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()           ## Get already cached inverse matrix

        if(!is.null(inv)) {
                ## Checking if cached inverse matrix is available
                
                message("Getting cached data")
                return(inv)             ## Return the cached inverse matrix of 'x'
                
        } else {
                ## Create and cache the inverse and version of 'x'
                
                inv <- solve(x, ...)    ## Compute the inverse matrix of 'x'
                x$setinverse(inv)       ## Set the inverse matrix of 'x' into cache
                inv                     ## Return the inverse matrix of 'x'
        }
}
