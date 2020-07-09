## the first function, makeCacheMatrix, creates a special matrix object and 
## can cache its inverse. The second function, cacheSolve, computes the inverse 
## of the special matrix returned by makeCacheMatrix. If the inverse has 
## already been calculated and the matrix has not changed, then cacheSolve
## retrieves the inverse from the cache

## creates a matrix and can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## computes the inverse of a makeCacheMatrix matrix if it has not already been 
## computed or if the matrix has changed
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setInverse(inv)
    inv
}