# Below are two functions that are used to create a special object 
# that stores a numeric matrix and cache's its inverse. 

# The first function, makeCacheMatrix creates a list containing a function to 
# 1.    set the value of the matrix
# 2.        get the value of the matrix
# 3.	set the inverse of matrix
# 4.	get the inverse of matrix
# A special "matrix" object can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {      # input x will be an invertible matrix
        
        s <- NULL       #  s will be our 'solve' and it's reset to NULL  
        #  every time makeCacheMatrix is called
        
        set <- function(z) {    # takes an input matrix
                x <<- z         # saves the input matrix 
                s <<- NULL      # resets the solve to NULL, basically what 
                # happens when a new object is generated.
        }
        
        #  note these next three functions are not run when makeCacheMatrix is called.
        #  instead, they will be used by cacheSolve() to get values for x or for
        #  s (solve) and for setting the inverse.
        get <- function() { x }   # this function returns the value of the original matrix
        
        setinverse <- function(sol)  { s <<- sol }
        # this is called by cacheSolve() during the first cacheSolve() access 
        # and it will store the value using superassignment
        
        getinverse <- function() { s }  # this will return the cached value to cacheSolve()
        # on subsequent accesses
        list(set = set, 
             get = get,                 #   this is accessed each time makeCacheMatrix() is called,       
             setinverse = setinverse,   #   that is, each time we make a new object.  This is a list of 
             getinverse = getinverse)   #   the internal functions.                            
}

# The second function, cacheSolve computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
        s <- x$getinverse()              # accesses the object 'x' and gets the inverse of the original matrix
        if(!is.null(s)) {              # if solve was already cached (not NULL) ...
                message("getting cached data")  # ... send this message to the console
                return(s)                       # ... and return the inverse ... "return" ends 
                #   the function cacheSolve()
        }
        data <- x$get()         # we reach this code only if x$getinverse() returned NULL
        s <- solve(data, ...)   # if s was NULL then we have to calculate the inverse
        x$setinverse(s)         # store the calculated inverse in x (see setinverse() in makeCacheMatrix
        s                       # return the inverse to the code that called this function
}
