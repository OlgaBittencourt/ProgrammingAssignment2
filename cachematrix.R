## Assignement 2 - R Programming
## by Olga Bittencourt - Aug 2020

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <<- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
#`makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- try(solve(data, ...))
  x$setinv(inv)
  print (inv)
  inv 
}

#Test
#x <- matrix(1:4, 2)
#print (x)
#y <- makeCacheMatrix(x)
#y$get()
#y$setinv()
#y$getinv()
#cacheSolve(y)
