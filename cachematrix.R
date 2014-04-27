## what these functions do
##  makeCacheMatrix  This function creates a special "matrix" 
##    		   object that cache its inverse.
##
##  cacheSolve  This function computes the inverse of the special "matrix" 
##		    returned by makeCacheMatrix. If the inverse islready
##              calculated (and the matrix has not changed), then the 
##		    cachesolve retrieve the inverse from the cache.

## short comment describing makeCacheMatrix
##  this function creates a special "matrix", which is really 
##  a list containing a function to do
## 	  set/get the value of the matrix
## 	  set/get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL 
        }
        get <- function() x
        setInverse <- function(Inv) m <<- Inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## short comment describing cacheSolve
## this function calculates the inverse of the special "matrix" created with the
## above function.
## However, it first checks to see if the inverse has already been calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
