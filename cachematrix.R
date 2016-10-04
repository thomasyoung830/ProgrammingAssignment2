## Put comments here that give an overall description of what your
## functions do

## This function creates a unique matrix
makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the inverse property
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Method used to get the matrix
  get <- function() x
  
  ## Method used to set inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Method used to get inverse of matrix
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}


## Write a short comment describing this function
## Computes the inverse of the unique matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## Check to see if inverse is already calculated
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    ## Calculates the inverse of the matrix and sets the value of the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
