## Computing the inverse of a square matrix might turn out to be time-consuming 
## especially when performed repeatedly (in a loop, for example). 
## A combination of these two functions allows to cache the inverse of the 
## matrix so that there is no need to repeat the operation.  

## The first function returns a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    invm <<- inv
  }
  getinv <- function() {
    invm
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function computes the inverse of the "matrix" object returned by 
## the previous function. If the initial matrix remains the same and the inverse has
## already been calculated, then cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
}