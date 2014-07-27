## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function is used to cache a matrix and specify
## the function to determine its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## This function either returns the cached inverse of the matrix defined
## using makeCacheMatrix, or determines and caches the inverse using the
## solve function defined in makeCache Matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
