## This function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  
  z <- NULL
  set <- function(y) { ## sets the matrix
    x <<- y ## assigns a value to an object in a state that is different from the current state
    z <<- NULL
  }
  get <- function() x ## gets the matrix
  setinverse <- function(inverse) z <<- inverse ## sets the inverse
  getinverse <- function() z ## gets the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## returns the inverse of the original matrix

cacheSolve <- function(x, ...) { 
  z <- x$getinverse() ## fetch the inversed matrix from the previous function (makeCacheMatrix) 
  if(!is.null(z)) { ## if the inversed matrix is not null (empty) - in other words if it has already been computed  
    return(z) ## then return the cached inversed matrix
  }
  data <- x$get()
  z <- solve(data) ## otherwise compute the inversed matrix
  x$setinverse(z) ## then store it in the cache function (floating variable)
  z
}

To initiate the experiment type:

x <- matrix(c(3, 2, -7, -5), nrow = 2, ncol = 2) ## creates a 2x2 matrix
m = makeCacheMatrix(x) 
m$get()

cacheSolve(m) ## remember to run this twice; the first time to compute the inversed matrix and the second time to fetch it from cache
  