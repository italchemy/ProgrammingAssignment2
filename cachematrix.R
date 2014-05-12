## This file contains functions to create and manipulate a wrapper for the
## matrix type that caches its inverse. The purpose of the class is to avoid the
## computational overhead of re-calculating the matrix inverse once it has
## already been found.

## makeCacheMatrix - Creates a caching wrapper around the matrix passed into the
## program arguments.
## 
## Parameters:
## 
## x - a matrix
## 
## Returns: the provided matrix contained within a wrapper that will cache the 
## matrix inverse once it has been calculated. The returned type is a list with
## named entries corresponding to the following functions:
##
## get - returns the wrapped matrix
## set - sets the value of the matrix
## inverse
##
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  # Sets the value of the wrapped matrix and stores it in the parent environment
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Gets the wrapped matrix
  
  get <- function() x
  
  # Sets the inverse of the wrapped matrix and stores it in the parent environment
  
  setInverse <- function(mInverse) inverse <<- mInverse
  
  # Gets the stored inverse
  
  getInverse <- function() inverse
  
  # List of wrapper functions to enable calling by named element, e.g. x$set(m)
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve - Solves for the inverse of the provided matrix. 
##
## Parameters:
##
## x - a wrapped matrix previously created by a call to makeCacheMatrix()
##
## Returns: the inverse of the provided matrix.
##
cacheSolve <- function(x, ...) {
       
  inverse <- x$getInverse()
  
  if (is.null(inverse)) {
    
    # We didn't get a cache hit so solve for the inverse and cache the result
    
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    inverse <- i
    
  }
  
  inverse
  
}
