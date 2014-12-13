## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly
## These functions help 

## This function creates an object which stores an invertible matrix and 
## caches its inverse matrix.
## You can access the stored values via set and get methods.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinvert <- function(inverted_matrix) {i <<- inverted_matrix}
  getinvert <- function() {i}
  list(set = set, 
       get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## This function works on an object created by the makeCacheMatrix function.
## It calculates the inverse of the stored matrix if it is not cached yet. 
## If the inverse is stored then this function returns with its value without
## recalculation.

cacheSolve <- function(x, ...) {
  inverted_matrix <- x$getinvert()
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  data <- x$get()
  inverted_matrix <- solve(data, ...)
  x$setinvert(inverted_matrix)
  inverted_matrix
}
