## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 matrix <- NULL
  set <- function(y) {
    x <<- y # x hold new value
    matrix <<- NULL # reset 
  }
  get <- function() x
  setsolve <- function(solve) matrix <<- solve
  getsolve <- function() matrix
  list(set      = set, 
       get      = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

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

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}