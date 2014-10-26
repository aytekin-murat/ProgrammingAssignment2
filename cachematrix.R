## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y             # assign to x in parent environment
        inv <<- NULL        # assign to inv in parent environment
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    # return the list of functions all in the closure of the makeCacheMatrix,
    # same environment with inv
    list(set = set, get = get,
         setinverse = setinverse,         
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
