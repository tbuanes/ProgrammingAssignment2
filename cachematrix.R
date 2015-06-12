## Put comments here that give an overall description of what your
## functions do

## Make object which holds a matrix and it's inverse
## Inverse is not calculated when creating the object, but should be set by
## calling function
## If input matrix is not square, the function returns NULL
## Return makeCacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
    # Check if matrix is square
    if ( nrow(x)!=ncol(x) ) {
        message( "Matrix is not square, returning NULL" )
        return(NULL)
    }
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )   
}


## Find inverse of matrix of type makeCacheMatrix
## Only calculate inverse if this has not been cached.
## Cache inverse if not previously done
## Return inversed matrix
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if ( !is.null(inverse) ) {
        message("Inverse read from cache")
        return(inverse)
    }
    message("Inverse not cached, calculating it...")
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse      
}
