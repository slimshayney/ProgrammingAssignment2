## This function (makeCacheMatrix) will return the inverse of a matrix stored in another function (cacheSolve)
## The below 2x2 matrix was created to test the function 
## x <- matrix(c(3, 2, -7, -5), nrow = 2, ncol = 2)
## The inverse of the above matrix can be computed as follows:
## solve(x)

makeCacheMatrix <- function(x = matrix()) { ## This function sets the value of the matrix and the inverse
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  z <- x$getinverse() 
  if(!is.null(z)) {
    message("getting cached data.")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setinverse(z)
  z
}

To initiate the experiment type:

x <- matrix(c(3, 2, -7, -5), nrow = 2, ncol = 2)
m = makeCacheMatrix(x)
m$get()

cacheSolve(m)
  