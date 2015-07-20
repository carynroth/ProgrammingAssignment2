## This pair of functions cache the inverse of a matrix in order
## to avoid computing it repeatedly

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return value that matrix has been set to
  get <- function() x
  ## set inverse matrix  
  set_inverse_mat <- function(inverse_mat) m <<- inverse_mat
  ## return inverse matrix
  get_inverse_mat <- function() m
  ## store functions set, get, set_inverse_mat and get_inverse_mat within
  ## the makeCacheMatrix function
  list(set = set, get = get,
       set_inverse_mat = set_inverse_mat,
       get_inverse_mat = get_inverse_mat)
  
}

## this function computes the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated, 
## this function retreives it from the cache

cacheSolve <- function(x, ...) {
  ## call function from above to return inverse matrix
  m <- x$get_inverse_mat()
  ## if inverse matrix is already set, return it from the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if inverse matrix is not already set, calculate it and set it
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse_mat(m)
  m
}
