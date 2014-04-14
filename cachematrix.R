## Two functions to allow the caching of the calculated matrix inverse to
## avoid computationally costly recomputation

## Creates a CacheMatrix object that stores the computation of a matrix inverse 
## for later reuse. 

makeCacheMatrix <- function(mat = numeric()) {
  m <- NULL
  set <- function(y) {
    mat <<- y
    m <<- NULL
  }
  get <- function() mat
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks to see if a cached version of the matrix exists. Returned cached value
## if exists. Otherwise calculates the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
