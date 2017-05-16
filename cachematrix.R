## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix 'mat' and creates a list of functions
## of set, get, setinv and getinv. This function is called everytime 
## an instance of makeCacheMatrix is invoked. Function take one argument
## that is the matrix to be inversed. set method creates the matrix when a 
## change is involved. get returns the matrix. setinv is used by a function later in the code
## and getinv returns the cached inverse. The function returns a list with elements same names as the function
## it holds. 

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y ## <<- is used to assign values to symbols in parent scope
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function checks to see if the matrix object, which has to be an instance
## of makeCacheMatrix, has an inverse cached. If it does, it returns the cached inverse. 
## Else it computes the inverse of the matrix and uses the 'setinv' method to set the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
