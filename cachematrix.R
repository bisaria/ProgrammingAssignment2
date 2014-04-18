## Matrix inversion is a costly computaion and if inverted matrix is 
## required repeatedly, it makes sense to cache the value instaed of 
## computing repeatedly.Functions 'makeCacheMatrix'  and  'cacheSolve' 
## compute and cache the inverse of any invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Function takes a matrix x as its argument.
## set(x) -> sets the value of the matrix 
## get() -> returns the value of the matrix x
## setinverse(inverse) -> sets the value of inverse of the matrix
## getinverse() -> returns the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the special 'matrix' object as argument and calculates 
## the inverse of this special 'matrix' object. It first checks if the 
## inverse has already been calculated. If yes, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse of the matrix
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- MASS::ginv(mat, ...)
  x$setinverse(inv)
  inv
}
