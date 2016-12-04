## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is responsible for creating an instance of a matrix
## Which has setters and getters responsible for handling the matrix's 
##inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function is dealing with two options:
##1. The matrix environment already has a cached inverse, in that case,
##the function returns it by using its "get" function.
##2. The matrix environment does not have a cached inverse, in that case,
##it uses "solve" and sets it as the environment's cached inverse for later use.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
