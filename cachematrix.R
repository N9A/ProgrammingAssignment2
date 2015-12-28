## Homework Asssignment programming #2
## Caching the inverse of a Matrix
## Below are the two functions that are used to create a special objects that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse

## Note to Self (The top  part of the function "makeCacheMatrix" is very similar to the example which was given by 
## the professor in the programming notes "makeVector" funtion.)

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        settoInverse <- function(inverse) invr <<- inverse
        gettheInverse <- function() invr
        list(set = set, get = get,
             settoInverse = settoInverse,
             gettheInverse = gettheInverse)
}


## This function inverse the "matrix" which was created by above fuction "makeCacheMatrix".

## Note to self, this function has very similar flow to what was provided by professor in "cachemean" 
## function without the same functionality.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invr <-x$gettheInverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$settoInverse(invr)
        invr
}

