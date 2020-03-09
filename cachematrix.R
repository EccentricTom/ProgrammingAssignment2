## These two functions will first create a matrix, and then the second with cache its inverse 

## This function creates a special matrix that is solvable (can be inversed), gets the values, then sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL #this creates the matrix
  }
  getMatrix <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## this function gets the special matrix and then presents the inverse. If the inverse has already been created, then the inverse is presented

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
