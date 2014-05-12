## Matrix inversion is a costly computaion and if inverted matrix is 
## required repeatedly, it makes sense to cache the value instead of 
## computing repeatedly.
## Functions 'makeCacheMatrix'  and  'cacheSolve' 
## compute and cache the inverse of an invertible square matrix using solve().
## Assumption: The matrix supplied is always invertible

## Example:
## > m<-matrix(c(1,2,1,4), 2,2)
## > M<-makeCacheMatrix(m)
## > cacheSolve(M)
## [,1] [,2]
## [1,]    2 -0.5
## [2,]   -1  0.5
## > cacheSolve(M)
## getting cached data
## [,1] [,2]
## [1,]    2 -0.5
## [2,]   -1  0.5

## This function creates a special "matrix" object that can cache its inverse.
## Function takes a matrix x as its argument.
## set(x) -> sets the value of the matrix 
## get() -> returns the value of the matrix x
## setinverse(inverse) -> sets the value of inverse of the matrix
## getinverse() -> returns the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                               
  set <- function(y) {                                      
    x <<- y                                                 # Assigns value y to matrix x
    inv <<- NULL                                            # Assigns value NULL to inv
  }
  get <- function() x                                       # Returns matrix x
  setinverse <- function(inverse) inv <<- inverse           # Assigns and caches the value of inverse of matrix x
  getinverse <- function() inv                              # Returns the cached value of inverse of matrix x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the special 'matrix' object as argument and  
## returns the inverse of this special 'matrix' object. 
## It first checks if the inverse has already been calculated.
## If yes, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse of the matrix in the cache 
## using the setinverse function.

cacheSolve <- function(x, ...) {  
  inv <- x$getinverse()                                    # Get cached inverse                         
  if(!is.null(inv)) {                                      # Check if cached inverse is not null.
    message("getting cached data")
    return(inv)                                            # Return cached inverse
  }

  mat <- x$get()                                           # Get the matrix mat from special 'matrix' object
  inv <- solve(mat, ...)                                   # Calculate inverse of matrix using solve()
  x$setinverse(inv)                                        # Cache the inverse using setInverse()
  inv                                                      # Return inverse of matrix
}