## The following functions compute the inverse of a matrix and 
## caches the result

## makeCacheMatrix takes a square invertible matrix argument and 
## returns a list with functions to set a new matrix, 
## get the current matrix, get the cached inverse and
## set the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes a special matrix list returned by makeCacheMatrix and
## computes and caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("returning cached result")
                return(i)
        } else {
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                return(i)
        }
}
