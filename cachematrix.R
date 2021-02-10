## Put comments here that give an overall description of what your
## functions do
#Below are two functions that are used to create a special object that stores a 
#"matrix" object and cache's its inverse matrix.

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. It first checks to see if the mean has already been 
#calculated. If so, it gets the inverse from the cache and skips the
#computation. Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
