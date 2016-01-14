## Using following functions we can Cache the Inverse of a Matrix.
## It will save our resources from repeatedly compute Inverse of a Matrix.

## This function will use to set or get matirix 
## and for also set or get inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
