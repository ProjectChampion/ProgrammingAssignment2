## Assignment 2: Caching the inverse of a Matrix
## The goal of the following functions is to cache potentially time-consuming
## computation of the inverse of a matrix by calculating the result once and 
## caching it inside an R object (i.e. a list), by taking advantage of scoping 
## rules of the R language.


##The first function, makeCacheMatrix creates a special matrix, which is in fact
## a list containing a function to 
## 1. set the matrix
## 2. get the the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get, 
             setInverse= setInverse, getInverse = getInverse)
}


## The second function, cacheSolve computes the inverse of the special matrix returned 
## by makeCacheMatrix above. If the inverse has already been calculated and the matrix
## has not changed, then the cachesolveretrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                  message("getting cached data")
                  return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
