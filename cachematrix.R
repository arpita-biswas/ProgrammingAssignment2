
## The following lines of code help in caching the inverse of a matrix rather than computing it repeatedly 

## makeCacheMatrix creates a special 'matrix' that basically is a list containing a function to
## 1. set the value of the matrix 
## 2. set the inverse of the matrix 
## 3. get the value of the matrix
## 4. get the inverse of the matrix
## Assumption as per the assignment: matrix supplied is always invertible

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {   ## function to set the value of the special matrix
    m <<- y
    inv <<- NULL
  }
  get <- function() m    ## function to get the value of the special matrix
  setInv <- function(inverse) inv <<- inverse           ## function to set the inverse of the matrix
  getInv <- function() inv                              ## function to get the inverse of the matrix
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## cacheSolve takes the special matrix as argument, which is created using the above function
## It then checks whether the inverse of the matrix is already calculated.
## If yes, it gets the inverse from the cache and returns the value. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache using the setInv function, and then returns the inverse.


cacheSolve <- function(x, ...) {
  ## Get the inverse matrix of the special matrix 'x'
  inv <- x$getInv()
  ## Check whether the inverse of the matrix is already cached
  if(!is.null(inv)) {
    message("The inverse is obtained from cached data")
    ## Return the inverse of teh matrix
    return(inv)
  }
  ## If not already cached, get the matrix value
  m <- x$get()
  ## Compute the inverse of the matrix
  inv <- solve(m, ...)
  ## Cache or store the inverse of the matrix
  x$setInv(inv)
  ## Return the inverse of the matrix
  return(inv)
}
