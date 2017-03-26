## R Programming
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  s <- function(value)
  {
    x <<- value
    result <<- NULL
  }
  g <- function() x
  sinv <- function(inv) result <<- inv
  ginv <- function() result
  list(s = s, g = g, sinv = sinv, ginv = ginv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$ginv()
  if(!is.null(m)) {
    message("loading...")
    return(m)
  }
  data <- x$g()
  nrow(data) 
  ncol(data)
  if(nrow(data) != ncol(data))
  {
          stop("unsuitable matrix")
  }
  m <- solve(data, ...)
  x$sinv(m)
  m
}
