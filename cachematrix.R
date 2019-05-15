## Put comments here that give an overall description of what your
## functions do

## Matrix cache facility
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # Set matrix data
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    # Get matrix data
    get <- function() x
    
    # Set inverse data
    setinverse <- function(inverse) inv <<- inverse
    
    # Get inverse data
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Matrix cache helper
cacheSolve <- function(x, ...) {
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
