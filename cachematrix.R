## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This function creates the special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## define a varible and set the value of the matrix
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  
  ##get the value 
  get <- function() x
  ##set the inverse
  setInverse <- function(inverse) invers <<- inverse
  ##get the inverse
  getInverse <- function() invers
  
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function returns the matrix that is inverse to x and that has been computed by the makeCache Matrix function.
cacheSolve <- function(x, ...) {
  invers <- x$getInverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data, ...)
  x$setInverse(invers)
  invers
}

### example  %*%data
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