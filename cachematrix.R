## This function creates a special "matrix" that can contain both its inverse and itself
## x is the matrix we need to give. 


makeCacheMatrix <- function(x = numeric()) {
  invx <- NULL
  setx <- function(y) {
    x <<- y
    invx <<- NULL
  }
  getx <- function() x
  setinvx <- function(inverse) invx <<- inverse
  getinvx <- function() invx
  list(setx = setx, 
       getx = getx,
       setinvx = setinvx,
       getinvx = getinvx)
}


## This function computes the inverse of the matrix created by function makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then it give the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinvx()
  if(!is.null(inv)) {
    message("getting cached data")
    return(invx)
  }
  dat <- x$getx()
  invx <- solve(dat, ...)
  x$setinvx(invx)
  invx
}
