## makeCacheMatrix creates a matrix encapsulation. 
## You can get and set the the matrix 
## and also get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse to NULL
  i <- NULL
  # set value of matrix x and clear inverse value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get matrix
  get <- function() x
  # set the value of the inverse 
  setinv <- function(inv) i <<- inv
  # get the value of the inverse 
  getinv <- function() i
  # return list the objects we created
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the input matrix x. 
## If the inverse is already computed return the cached version, else recompute.

cacheSolve <- function(x, ...) {
  # get inverse from our matrix object.
  i <- x$getinv()
  # if exists return cached inverse.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise get the matrix and compute inverse.
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
