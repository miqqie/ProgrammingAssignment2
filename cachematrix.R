## The intended output is to generate the inverse of matrices. 
## For values of the inverse of the matrices that are computed,
## they are then stored so that they can be retrieved 
## instead of being recomputed.

## The makeCacheMatrix function helps to store
## the inverse of the matrix calculated by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
	  setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the matrix 'x'.
## For inverses already computed, it will retrieve the values from makeCache matrix
## Else, it would proceed to compute the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else{data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m}      
}
