## R Programming
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  result <- NULL
  set <- function(value) {
    x <<- value
    result <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) result <<- inv
  getinverse <- function() result
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { message("loading...")
    return(m)
  }
  data <- x$get()
  nrow(data) 
  ncol(data)
  if(nrow(data) != ncol(data))
  { stop("unsuitable matrix")
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
