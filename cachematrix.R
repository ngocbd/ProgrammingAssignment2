## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These functions are to proxy to solve function
## usage : cacheSolve ( makeCacheMatrix ( theMatrix ) )

## Caches the passed in matrix
makeCacheMatrix <- function(x = matrix()) {
	 matrix <- NULL
  set <- function(y) {
    x <<- y # x hold new value
    matrix <<- NULL # reset 
  }
  get <- function() x # return  x
  setsolve <- function(solve) matrix <<- solve 
  getsolve <- function() matrix
  list(set      = set, 
       get      = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## checks if object passed in makeCacheMatrix object already has the cached inverse then returns if it has
## computes solve on matrix of makeCacheMatrix, set it to makeCacheMatrix and return it
cacheSolve <- function(x, ...) {
  
  matrix <- x$getsolve()
  if(!is.null(matrix)) {
    message("getting cached data") # found
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data)
  x$setsolve(matrix)
  matrix
}

