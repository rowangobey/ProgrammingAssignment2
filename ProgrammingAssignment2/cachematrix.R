## Matrix inversion is usually a costly computation and their may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly
## ASSUMPTIONS - The matrix is invertable

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" values returned by the makeCacheMatrix function.
## --If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                   i <- NULL
                   set <- function(y) {
                   x <<- y
                   i <<- NULL
                   }
                   get <- function() x
                   setinverse <- function(inverse) i <<- inverse
                   getinverse <- function() i
                   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



##The cacheSolve function calculates the inverse of the special "matrix" object created by the makeCacheMatrix function.

## 1 - First checks to see if the inverse has already been calculated. 
## 2 - If so, it gets the inverse from the cache and skips the computation. 
## 2 - Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
              i <- x$getinverse()
              if(!is.null(i)) {
              message("getting cached data.")
              return(i)
              }
              data <- x$get()
              inv <- solve(data)
              x$setinverse(i)
              i
}
