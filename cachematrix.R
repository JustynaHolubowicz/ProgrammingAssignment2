## The aim of the functions below 
##is to cache the inverse of a matrix.


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) elc <<-inverse
  getinverse <- function() elc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSOlve function calculates the inverse 
## of the special "matrix" created with the makeCacheMatrix. 
## It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache 
## and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  elc <- x$getinverse()
  if (!is.null(elc)) {
    message("getting cached reinverse matrix")
    return(elc)
  } else {
    elc <- solve(x$get())
    x$setinverse(elc)
    return(elc)
  }