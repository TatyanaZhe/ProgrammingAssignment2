## Functions used to create matrix (list with proper matrix access and
## manipulation function) with cacheable inverse operation.

## Creates special matrix with accessor function for itself and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## cached inverse matrix of x
    inv <- NULL
    set <- function(m){
        x <<- m
        inv <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { inv <<- inverse }
    getinverse <- function() { inv }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the special "matrix" returned by
## makeCacheMatrix above. First time inverse is calculated, after that
## it uses cached version (cached when calculated).
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
