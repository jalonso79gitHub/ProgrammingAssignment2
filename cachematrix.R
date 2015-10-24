## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inver <- NULL
    
    set <- function(initialMatrix) {
        x <<- initialMatrix
        inver <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inver <<- inv
    
    getInverse <- function() inver
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x , ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getInverse()
  
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    
    data <- x$get()
    
    inver <- solve(data, ...)
    
    x$setInverse(inver)
    
    inver
}
