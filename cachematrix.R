## A set of functions to make a special matrix object
## that provides the ability to get and set the value
## of the matrix and its inverse

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set a new matrix
  set <- function(y){
    x <<- y
    inv <<- NULL # Resets the inverse to NULL when the matrix is set
  }
  
  # Get the matrix
  get <- function(){ x }
  
  # Takes a matrix and sets it as the inverse of our matrix
  setInverse <- function(inverse){ inv <<- inverse }
  
  # Get the inverse of the matrix
  getInverse <- function(){ inv }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes a matrix object created by makeCacheMatrix
## and checks if its inverse has already been calculated
## if not, it calculates it and caches it.
cacheSolve <- function(x, ...) {
  # If the inverse has already been calcuated it, simply return the inverse
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  # Otherwise calculate the inverse, set it, and return it
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}
