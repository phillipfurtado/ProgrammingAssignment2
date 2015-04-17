## This function creates a cached matrix with 
## basic set/get operations
makeCacheMatrix <- function(x = matrix()) {
  
  cMatrix <- NULL
  
  set <- function(y){
    x <<- y
    cMatrix <<- NULL
  }
  
  get <- function() { x }
  
  setmatrix <- function( newMatrixSolve ) { cMatrix <<- newMatrixSolve }
  
  getmatrix <-function() { cMatrix }
  
  #register methods
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## Solve the inverse matrix and cache it
## using makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
  cMatrix <- x$getmatrix()
  
  if (!is.null(cMatrix)) {
    message("getting cached data")
    return(cMatrix)
  }
  
  cMatrix <- x$get()
  
  cMatrix <- solve(cMatrix, ...)
  
  x$setmatrix(cMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  cMatrix
  
}
