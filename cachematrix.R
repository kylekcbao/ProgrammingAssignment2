## Written by Kyle KC Bao for Coursera RProgramming Assignment 2
## makeCacheMatrix creates a matrix that caches its inverse.
## cacheSolve solves the matrix returned by makeCacheMatrix. If the inverse is
## already cached, the function will retrieve the cached results.
## TESTING 
## cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2))) should return:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix takes in a matrix object and creates a list of functions to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of the inverse matrix
  # 4. get the value of the inverse matrix
  
  inverse <- NULL
  
  # set matrix function
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get matrix function
  get <- function() x
  # set inverse matrix function
  set.inverse <- function(inv) inverse <<- inv
  # get inverse matrix function
  get.inverse <- function() inverse
  
  # outputs a list containing the above functions
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

cacheSolve <- function(x, ...) {
  # cacheSolve computes the inverse of the matrix returned by makeCacheMatrix()
  # If inverse has already been solved, the cached result is returned,
  # else it computes the inverse and caches it using set.inverse()
  
  # get cached inverse matrix
  inverse <- x$get.inverse()
  # checks if the cache is NULL, if not gets cached inverse matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if no cached results, gets matrix and computes inverse matrix
  data <- x$get()
  inverse <- solve(data, ...)
  # caches resultant inverse matrix for future use
  x$set.inverse(inverse)
  
  inverse
}
