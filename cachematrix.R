## These functions are used to cache the inverse of a matrix, to save time when
## performing calculations by not repeating the inversion.

## This function creates a special matrix-like object that is actually a list of functions to:
## 1) Set the value of the matrix
## 2) Retrieve the values of the matrix
## 3) Set the inverse of the matrix
## 4) Retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xInv <<- inverse
    getinverse <- function() xInv
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## This function returns the inverse of the matrix-lke object created by the previous function.
## If the inverse has already been calculated it will retrieve it from the cache. If not, the function
## calculates the inverse and then caches it. The amtrix is assumed to be invertible in this implementation.

cacheSolve <- function(x, ...) {
        
        ## First, check to see if the inverse already exists in the list. If it does, return it.
        xInv <- x$getinverse()
        if(!is.null(xInv)) {
            message("getting cached data")
            return(xInv)
        }
        
        ## If the inverse was null, pull the matrix in x into a temporary variable to work on it, then solve it.
        data <- x$get()
        xInv <- solve(data, ...)
        
        ## Set the inverse in the cache to avoid recalculating it.
        x$setinverse(xInv)
        
        ## Return a matrix that is the inverse of 'x'
        xInv
}
