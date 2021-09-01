## Put comments here that give an overall description of what your
## functions do

## First step: the matrix will be assigned to "x" variable
##and after, we will initialize mymat to NULL

makeCacheMatrix <- function(x = matrix()) {
  mymat <- NULL
  set <- function(y) {          ## if the user want to begin again, he will
    x <<- y                     ## resign the new matrix to x 
    mymat <<- NULL                  ## and, after, reinitialize mymat to NULL
  }
  get <- function() x
  setInvmatrix <- function(InvMatrix) mymat <<- InvMatrix
  getInvmatrix <- function() mymat
  list(set = set, get = get,
       setInvmatrix = setInvmatrix,
       getInvmatrix = getInvmatrix)
}


## Second step: it will return a matrix that is the inverse of "x"

cacheSolve <- function(x, ...) {
  
  mymat <- x$getInvmatrix()              
  if(!is.null(mymat)) {           ## if user had calculated the same matrix before
    message("we detected cached data and returned:")  
    return(mymat)                 ## it will return the old result (mymat) directly 
  }
  data <- x$get()                 ## otherwise, it will get the uncalculated matrix
  mymat <- solve(data, ...)       ## calculate the inverse matrix
  x$setInvmatrix(mymat)           ## resign the inverse matrix 
  mymat                           ## and, finally, print the inverse matrix 
}

