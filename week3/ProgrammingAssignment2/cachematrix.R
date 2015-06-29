## Cache the inverse of a matrix 
## which is useful in repeat inverse computational situations
##

## Construct a matrix object, which caches inverse of the matrix
##
## Args: 
##       x: matrix, input
##
## Returns:
##       A cachedMatrix object, which returns cached inverse of the matrix 
##       when used with cacheSolve()
##
makeCacheMatrix <- function(x = matrix()) {

        # Initialize cache of matrix inverse
        m <- NULL
        
        # Getter and Setter for the matrix input
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        # Getter and Setter for the matrix inverse
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        # List of all methods
        list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes and returns the inverse of the matrix, 
## returns cached version if available
##
## Args: 
##       x: matrix object from makeCacheMatrix()
##
## Returns:
##       Inverse of the matrix input x, from cache if available 
##
cacheSolve <- function(x, ...) {
        
        # Try to get inverse of the matrix, from cache if available
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If not available in cache, create inverse of the matrix and cache it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
