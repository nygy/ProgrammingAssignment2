## These functions help to save costly computation by caching the inverse 
## of a matrix rather than computing it repeatedly.

## This function creates an object which stores an invertible matrix and 
## caches its inverse matrix.
## You can access the stored values via the set and get methods of the function.

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
## If the inverse is stored then it returns with the inverse without
## recalculation.

cacheSolve <- function(x, ...) {
  inverted_matrix <- x$getinvert()
  if(!is.null(inverted_matrix)) {return(inverted_matrix)}
  stored_matrix <- x$get()
  inverted_matrix <- solve(stored_matrix)
  x$setinvert(inverted_matrix)
  inverted_matrix
}
